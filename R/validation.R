# global variables
utils::globalVariables(c("hampel_outlier", "validated_outlier", "outlier_status", "Zscore"))

#' Validate detected outliers
#'
#' @description Applies validation methods to outliers detected by \code{detect()}.
#' This helps reduce false positives by applying secondary filtering.
#'
#' @param .x An object of class \code{maly_detect} from the \code{detect} function.
#' @param .method Character string specifying the validation method. Currently 
#'   supports \code{"hampel"}. Default is \code{"hampel"}.
#' @param .args Named list of additional arguments for the validation method.
#'   For \code{"hampel"}, options include \code{.k} (window half-width, default 5),
#'   \code{.t0} (MAD threshold, default 5), and \code{.use_zscore} (logical, default TRUE).
#'
#' @return A data frame of class \code{maly_validate} with validated outlier indicators.
#'
#' @examples
#' \dontrun{
#' # Full pipeline
#' data |>
#'   normalize(.value_col = "Value", .country_col = "Country",
#'             .indicator_col = "Indicator", .time_col = "Year") |>
#'   detect(.method = "zscore") |>
#'   validate(.method = "hampel")
#' }
#'
#' @importFrom R.utils doCall
#' @export
validate <- function(.x,
                     .method = "hampel",
                     .args = list()) {

  if (!inherits(.x, "maly_detect") || !is.data.frame(.x)) {
    stop("Input must be a data frame of class 'maly_detect'.", call. = FALSE)
  }
  
  valid_methods <- c("hampel")
  if (!.method %in% valid_methods) {
    stop(paste("Invalid method. Choose from:", paste(valid_methods, collapse = ", ")), 
         call. = FALSE)
  }
  
  # Retrieve column specs from maly_detect attributes
  .country_col <- attr(.x, "country_columns")
  .time_col <- attr(.x, "time_columns")
  .indicator_col <- attr(.x, "indicator_columns")
  .value_col <- attr(.x, "value_column")
  
  results <- switch(.method,
    hampel = doCall(hampel_validation, 
      .data = .x, 
      .country_col = .country_col,
      .time_col = .time_col,
      .indicator_col = .indicator_col,
      .value_col = .value_col,
      args = .args[["hampel"]]
    )
  )
  
  # Preserve attributes from previous steps
  attr(results, "country_columns") <- .country_col
  attr(results, "time_columns") <- .time_col
  attr(results, "indicator_columns") <- .indicator_col
  attr(results, "value_column") <- .value_col
  attr(results, "maly_detect_attr") <- attr(.x, "maly_detect_attr")
  attr(results, "maly_norm_attrs") <- attr(.x, "maly_norm_attrs")
  attr(results, "maly_validate_attr") <- list(method = .method)
  class(results) <- c("maly_validate", class(.x))
  
  return(results)
}


#' Hampel filter validation
#'
#' @description Validates outliers using the Hampel filter, which identifies 
#' outliers based on median absolute deviation within a sliding window.
#'
#' @param .data Data frame from detect().
#' @param .country_col Character vector of country column names.
#' @param .time_col Character string for time column name.
#' @param .indicator_col Character string for indicator column name.
#' @param .value_col Character string for value column name.
#' @param .k Integer. Half-width of the sliding window. Default is 5.
#' @param .t0 Numeric. Threshold for outlier detection (MAD multiplier). Default is 5.
#' @param .use_zscore Logical. If TRUE, apply Hampel to Zscore column; 
#'   if FALSE, apply to original value. Default is TRUE.
#'
#' @return Data frame with \code{hampel_outlier} and \code{validated_outlier} columns.
#'
#' @importFrom collapse fgroup_by roworderv GRP BY fungroup
#' @export
hampel_validation <- function(.data,
                              .country_col,
                              .time_col,
                              .indicator_col,
                              .value_col,
                              .k = 5,
                              .t0 = 5,
                              .use_zscore = TRUE) {
  
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("The 'pracma' package is required for Hampel validation. Please install it.", 
         call. = FALSE)
  }
  
  .filter_col <- if (.use_zscore) "Zscore" else .value_col
  
  # Build ordering columns vector
  .order_cols <- c(.indicator_col, .country_col[1], .time_col)
  
  .data <- .data |>
    fungroup() |>
    roworderv(.order_cols)
  
  .grouped <- .data |>
    fgroup_by(c(.indicator_col, .country_col[1])) |>
    GRP()
  
  hampel_results <- BY(
    .data[[.filter_col]], 
    .grouped, 
    FUN = function(x) {
      # Handle NA values: hampel() cannot process them
      result <- rep(FALSE, length(x))
      non_na_idx <- which(!is.na(x))
      
      if (length(non_na_idx) > 2 * .k) {
        # Enough non-NA values to apply Hampel filter
        h <- pracma::hampel(x[non_na_idx], k = .k, t0 = .t0)
        # Map back to original indices
        result[non_na_idx[h$ind]] <- TRUE
      }
      result
    },
    return = "list"
  )
  
  .data$hampel_outlier <- unlist(hampel_results)
  
  # Validated = flagged by BOTH detect() and Hampel
  if ("outlier_indicator_total" %in% names(.data)) {
    .data$validated_outlier <- .data$hampel_outlier & (.data$outlier_indicator_total > 0)
  } else {
    .data$validated_outlier <- .data$hampel_outlier & (.data$outlier_indicator == 1)
  }
  
  return(.data)
}


#' Summary method for maly_validate class
#'
#' @description Provides a summary of validation results, comparing detected 
#' outliers before and after validation.
#'
#' @param object An object of class maly_validate.
#' @param ... Additional arguments (not used).
#'
#' @export
#' @method summary maly_validate
summary.maly_validate <- function(object, ...) {
  
  if (!inherits(object, "maly_validate")) {
    stop("object must be of class 'maly_validate'.", call. = FALSE)
  }
  
  # Get attributes
  country_col <- attr(object, "country_columns")[1]
  time_col <- attr(object, "time_columns")
  indicator_col <- attr(object, "indicator_columns")
  detect_method <- attr(object, "maly_detect_attr")$method

  validate_method <- attr(object, "maly_validate_attr")$method
  
  # Determine which detection column to use
  if ("outlier_indicator_total" %in% names(object)) {
    detected_outliers <- sum(object$outlier_indicator_total > 0, na.rm = TRUE)
  } else {
    detected_outliers <- sum(object$outlier_indicator == 1, na.rm = TRUE)
  }
  
  # Validation counts
  hampel_outliers <- sum(object$hampel_outlier, na.rm = TRUE)
  validated_outliers <- sum(object$validated_outlier, na.rm = TRUE)
  
  # False positives removed (detected but not validated)
  false_positives_removed <- detected_outliers - validated_outliers
  
  # Hampel-only outliers (flagged by Hampel but not by detect)
  hampel_only <- hampel_outliers - validated_outliers
  
  # Count by country/indicator for validated outliers
  if (validated_outliers > 0) {
    outlier_by_country <- table(object[object$validated_outlier, country_col])
    top_country <- names(which.max(outlier_by_country))
    top_country_count <- max(outlier_by_country)
    
    outlier_by_indicator <- table(object[object$validated_outlier, indicator_col])
    top_indicator <- names(which.max(outlier_by_indicator))
    top_indicator_count <- max(outlier_by_indicator)
  }
  
  # Check for imputed values among validated outliers
  if ("Imputed" %in% names(object)) {
    validated_imputed <- sum(object$validated_outlier & object$Imputed, na.rm = TRUE)
    validated_not_imputed <- sum(object$validated_outlier & !object$Imputed, na.rm = TRUE)
  }
  
  # Print summary
  cat("Summary of Macroanomaly validation:\n")
  cat("-----------------------------------\n\n")
  
  cat("Methods:\n")
  cat("  Detection method(s):", paste(detect_method, collapse = ", "), "\n")
  cat("  Validation method:", validate_method, "\n\n")
  
  cat("Outlier counts:\n")
  cat("  Detected outliers (before validation):", detected_outliers, "\n")
  cat("  Hampel filter outliers:", hampel_outliers, "\n")
  cat("  Validated outliers (confirmed by both):", validated_outliers, "\n\n")
  
  cat("Validation impact:\n")
  cat("  Potential false positives removed:", false_positives_removed, "\n")
  cat("  Hampel-only outliers (not in detect):", hampel_only, "\n")
  
  if (detected_outliers > 0) {
    reduction_pct <- round(100 * false_positives_removed / detected_outliers, 1)
    cat("  Reduction rate:", reduction_pct, "%\n\n")
  }
  
  if (validated_outliers > 0) {
    cat("Top validated outliers:\n")
    cat("  Country with most outliers:", top_country, "(", top_country_count, ")\n")
    cat("  Indicator with most outliers:", top_indicator, "(", top_indicator_count, ")\n")
  }
  
  if ("Imputed" %in% names(object)) {
    cat("\nImputation status of validated outliers:\n")
    cat("  From imputed values:", validated_imputed, "\n")
    cat("  From original values:", validated_not_imputed, "\n")
  }
  
  invisible(object)
}


#' Plot method for maly_validate class
#'
#' @description Plots time series with validated outliers highlighted.
#'
#' @param x An object of class maly_validate.
#' @param country Character string specifying the country to plot.
#'   Default is NULL (uses first country with validated outliers).
#' @param indicator Character string specifying the indicator to plot.
#'   Default is NULL (uses first indicator with validated outliers).
#' @param show_all_outliers Logical. If TRUE, shows both detected and validated 
#'   outliers with different styling. Default is TRUE.
#' @param x.lab Label for x-axis. Default is NULL (uses time column name).
#' @param y.lab Label for y-axis. Default is NULL (uses "Obs. value").
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme element_text 
#'   facet_wrap scale_color_manual scale_shape_manual scale_alpha_manual guides
#' @importFrom collapse fungroup fgroup_by
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom dplyr case_when
#'
#' @export
#' @method plot maly_validate
plot.maly_validate <- function(x, 
                               country = NULL, 
                               indicator = NULL, 
                               show_all_outliers = TRUE,
                               x.lab = NULL, 
                               y.lab = NULL, 
                               ...) {
  
  # Get attributes
  country_col <- attr(x, "country_columns")[1]
  time_col <- attr(x, "time_columns")
  indicator_col <- attr(x, "indicator_columns")
  value_col <- attr(x, "value_column")
  
  # Default to first country/indicator with validated outliers
  if (is.null(indicator)) {
    validated_data <- x[x$validated_outlier, ]
    if (nrow(validated_data) > 0) {
      indicator <- validated_data[[indicator_col]][1]
    } else {
      indicator <- unique(x[[indicator_col]])[1]
    }
  }
  
  if (is.null(country)) {
    validated_data <- x[x$validated_outlier & x[[indicator_col]] == indicator, ]
    if (nrow(validated_data) > 0) {
      country <- validated_data[[country_col]][1]
    } else {
      country <- unique(x[x[[indicator_col]] == indicator, country_col])[1]
    }
  }
  
  # Filter data
  .data <- x[x[[country_col]] == country & x[[indicator_col]] == indicator, ]
  
  if (nrow(.data) == 0) {
    stop(paste("No data found for country:", country, "and indicator:", indicator), 
         call. = FALSE)
  }
  
  # Convert time to Date if yearly
  if (attr(x, "maly_norm_attrs")$frequency == "yearly") {
    if (!inherits(.data[[time_col]], "Date")) {
      .data[[time_col]] <- as.Date(paste0(.data[[time_col]], "-01-01"))
    }
  } else {
    if (!inherits(.data[[time_col]], "Date")) {
      .data[[time_col]] <- as.Date(.data[[time_col]])
    }
  }
  
  # Determine detected outliers
  if ("outlier_indicator_total" %in% names(.data)) {
    detected <- .data$outlier_indicator_total > 0
  } else {
    detected <- .data$outlier_indicator == 1
  }
  
  # Count outliers by type for this country/indicator
  n_validated <- sum(.data$validated_outlier, na.rm = TRUE)
  n_detected <- sum(detected, na.rm = TRUE)
  n_hampel <- sum(.data$hampel_outlier, na.rm = TRUE)
  n_detected_only <- sum(detected & !.data$validated_outlier, na.rm = TRUE)
  n_hampel_only <- sum(.data$hampel_outlier & !detected, na.rm = TRUE)
  
  # Create outlier status factor
  if (show_all_outliers) {
    .data$outlier_status <- dplyr::case_when(
      .data$validated_outlier ~ "Validated",
      detected & !.data$validated_outlier ~ "Detected only",
      .data$hampel_outlier & !detected ~ "Hampel only",
      TRUE ~ "Not outlier"
    )
    
    # Determine which levels are present in the data
    present_levels <- unique(.data$outlier_status)
    
    # Define all possible levels, colors, and alphas
    all_levels <- c("Validated", "Detected only", "Hampel only", "Not outlier")
    all_colors <- c("Validated" = "red", "Detected only" = "orange", 
                    "Hampel only" = "blue", "Not outlier" = "black")
    all_alphas <- c("Validated" = 1, "Detected only" = 0.8, 
                    "Hampel only" = 0.8, "Not outlier" = 0.2)
    
    # Keep only present levels (always keep "Not outlier")
    active_levels <- all_levels[all_levels %in% present_levels]
    color_values <- all_colors[active_levels]
    alpha_values <- all_alphas[active_levels]
    
    .data$outlier_status <- factor(.data$outlier_status, levels = active_levels)
    
  } else {
    .data$outlier_status <- ifelse(.data$validated_outlier, "Validated", "Not outlier")
    
    present_levels <- unique(.data$outlier_status)
    all_levels <- c("Validated", "Not outlier")
    active_levels <- all_levels[all_levels %in% present_levels]
    
    color_values <- c("Validated" = "red", "Not outlier" = "black")[active_levels]
    alpha_values <- c("Validated" = 1, "Not outlier" = 0.2)[active_levels]
    
    .data$outlier_status <- factor(.data$outlier_status, levels = active_levels)
  }
  

  # Build informative caption showing counts for each category
  caption_parts <- c()
  
 # Validated outliers
  if (n_validated == 0) {
    caption_parts <- c(caption_parts, "No validated outliers")
  } else {
    caption_parts <- c(caption_parts, paste0(n_validated, " validated outlier", ifelse(n_validated > 1, "s", "")))
  }
  
  # Detected outliers
  if (n_detected == 0) {
    caption_parts <- c(caption_parts, "No detected outliers")
  } else {
    caption_parts <- c(caption_parts, paste0(n_detected, " detected outlier", ifelse(n_detected > 1, "s", "")))
  }
  
  # Hampel outliers
  if (n_hampel == 0) {
    caption_parts <- c(caption_parts, "No outliers flagged by Hampel")
  } else {
    caption_parts <- c(caption_parts, paste0(n_hampel, " outlier", ifelse(n_hampel > 1, "s", ""), " flagged by Hampel"))
  }
  
  caption_text <- paste(caption_parts, collapse = "; ")
  
  # Axis labels
  if (is.null(x.lab)) x.lab <- time_col
  if (is.null(y.lab)) {
    y.lab.top <- "Obs. value"
    y.lab.bottom <- "Z-score"
  } else {
    y.lab.top <- y.lab
    y.lab.bottom <- "Z-score"
  }
  
  # Top plot: Original values
  p1 <- ggplot(.data, aes(x = .data[[time_col]], y = .data[[value_col]])) +
    geom_line(alpha = 0.3) +
    geom_point(aes(color = outlier_status, alpha = outlier_status)) +
    scale_color_manual(values = color_values, drop = TRUE) +
    scale_alpha_manual(values = alpha_values, guide = "none") +
    labs(
      title = paste0(country, " - ", indicator),
      subtitle = paste0("Validated outliers: ", n_validated),
      x = x.lab,
      y = y.lab.top,
      color = "Outlier Status"
    ) +
    theme(axis.text = element_text(size = 8))
  
  # Bottom plot: Z-scores
  p2 <- ggplot(.data, aes(x = .data[[time_col]], y = Zscore)) +
    geom_point(aes(color = outlier_status, alpha = outlier_status)) +
    scale_color_manual(values = color_values, drop = TRUE) +
    scale_alpha_manual(values = alpha_values, guide = "none") +
    labs(
      x = x.lab,
      y = y.lab.bottom,
      color = "Outlier Status"
    ) +
    theme(axis.text = element_text(size = 8))
  
  # Combine plots
  combined_plot <- p1 / p2 +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.direction = "horizontal")
  
  # Add caption
  combined_plot <- combined_plot + plot_annotation(caption = caption_text)
  
  return(combined_plot)
}
