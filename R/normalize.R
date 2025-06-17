#' Detrend a time series using a smoothing spline
#'
#' This function detrends a time series using a smoothing spline.
#'
#' @param .data A numeric vector representing the time series data to be detrended.
#' @param .time A numeric vector representing the time points corresponding to the data.
#' @return A numeric vector of the same length as .data, containing the detrended values.
detrend <- function(.data,.year) {
  Nnonmiss <- fsum(is.finite(.data))
  output <- rep(NA,length(.data))
  if (Nnonmiss>0) {
    S <- supsmu(x = .year, y = .data)
    .pred <- unlist2d(S$y)
    output[is.finite(.data)] <- .data[is.finite(.data)] - .pred
  } else {
    warning("Inf or NaN values in the data")
  }
  return(output)
}


#' Convert data to tsibble
#'
#' This function converts a data frame to a tsibble object.
#'
#' @param .data A data frame containing the data to be converted.
#' @param .country_col A character vector specifying the name(s) of the country
#' identifiers.
#' @param .time_col A character vector specifying the name(s) of the time
#' identifiers. See \code{tsibble::as_tsibble()} for more details.
#' @param .indicator_col A character vector specifying the name(s) of the indicator
#' identifiers.
#' @param .frequency A character vector specifying the frequency of the time series.
#' Default is "yearly" for yearly data. The other values are "month" and "quarter".
#' @param .long_format A logical value indicating whether to return the data in long format.
#' Default is TRUE.
#' @return A tsibble object with the specified country and time identifiers.
#'
#' @importFrom collapse fmutate fselect
#' @importFrom tsibble as_tsibble yearmonth yearquarter
#' @importFrom lubridate year quarter month
#' @export
convert_to_tsibble <- function(.data,
                               .country_col,
                               .time_col,
                               .indicator_col,
                               .frequency = "yearly",
                               .long_format = TRUE) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .time_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if only one indicator
  if (length(unique(.data[[.indicator_col]])) > 1) {
    .keys <- c(.country_col, .indicator_col)
  } else {
    .keys <- .country_col
  }

  # Convert to tsibble
  if (.frequency %in% c("yearly", "year")) {
    .data <- as_tsibble(.data, key = .keys, index = .time_col)
  } else if (.frequency %in% c("quarterly", "quarter")) {
    .data |>
      fmutate(across(.time_col, tsibble::yearquarter)) |>
      tsibble::as_tsibble(key = .keys, index = .time_col) -> .data
  } else if (.frequency %in% c("monthly", "month")) {
    .data |>
      fmutate(across(.time_col, tsibble::yearmonth)) |>
      tsibble::as_tsibble(key = .keys, index = .time_col) -> .data
  } else {
    stop("Invalid frequency. Choose 'yearly', 'quarterly', or 'monthly'.")
  }

  return(.data)
}

#' Impute missing values in a time series
#'
#' This function imputes missing values in a time series using a function
#' from imputeTS package. The current options are: na_interpolation,
#' na_kalman, na_mean, na_locf, na_ma, na_seadec, na_random.
#'
#' @param .data A tsibble object containing the data to be imputed.
#' @param .method A character vector specifying the imputation method to be used.
#' The default is \code{na_interpolation}. Other options are: na_kalman, na_mean,
#' na_locf, na_ma, na_seadec, na_random.
#' @param .country_col A character vector specifying the name(s) of the country
#' identifiers.
#' @param .time_col A character vector specifying the name(s) of the time
#' identifiers.
#' @param ... Additional arguments to be passed to the imputation function.
#' @return A tsibble object with the imputed data and a column identifying the imputed values.
#'
#' @importFrom imputeTS na_interpolation na_kalman na_mean na_locf na_ma na_seadec na_random
#' @importFrom tsibble as_tsibble group_by_key
#' @importFrom collapse fmutate fselect ftransform
#' @importFrom rlang sym expr
#' @export
impute_missing <- function(.data,
                           .value_col,
                           .method = "na_interpolation",
                           .country_col,
                           .time_col,
                           .indicator_col,
                           ...) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .time_col, .indicator_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if the method is valid
  if (!.method %in% c("na_interpolation", "na_kalman", "na_mean", "na_locf", "na_ma", "na_seadec", "na_random")) {
    stop("Invalid method. Choose 'na_interpolation', 'na_kalman', 'na_mean', 'na_locf', 'na_ma', 'na_seadec', or 'na_random'.")
  }

  # Missing minimum values per method
  if (.method == "na_interpolation") {
    .min_obs <- 2
  } else if (.method == "na_kalman") {
    .min_obs <- 3
  } else if (.method == "na_mean") {
    .min_obs <- 1
  } else if (.method == "na_locf") {
    .min_obs <- 1
  } else if (.method == "na_ma") {
    .min_obs <- 2
  } else if (.method == "na_seadec") {
    .min_obs <- 3
  } else if (.method == "na_random") {
    .min_obs <- 2
  }

  # Check if enough values for imputation
  .missing_data <- check_missing_countries(.data,
                              .value_col = .value_col,
                              .country_col = .country_col,
                              .time_col = .time_col,
                              .indicator_col = .indicator_col,
                              .min_obs = .min_obs)

  if (nrow(.missing_data) > 0) {
    stop(paste("The following countries do not have enough data (i.e.,", .min_obs, " data point) for imputation:\n"),
            paste(.missing_data[[.country_col[1]]], collapse = ", "), ".\n Consider excluding these countries.")
  }

  suppressWarnings({
  # Impute missing values using the specified method
  .data <- .data |>
    ftransform(
      Imputed = is.na(eval(sym(expr(!!.value_col))))
  ) |>
    tsibble::group_by_key() |>
    fmutate(
      across(.value_col,
                   \(x) get(.method)(x, ...)))
  })

  return(.data)
}

#' Function to detect countries without data enough to produce imputation
#'
#' This function detects countries without enough data to produce imputation.
#'
#' @param .data A tsibble object containing the data to be checked.
#' @param .value_col A character vector specifying the name of the value column.
#' @param .country_col A character vector specifying the name(s) of the country
#' identifiers.
#' @param .time_col A character vector specifying the name of the time
#' identifiers.
#' @param .indicator_col A character vector specifying the name(s) of the indicator
#' identifiers.
#' @param .min_obs A numeric value specifying the minimum number of observations
#' required for imputation. Default is 2.
#' @return A data frame with the countries that do not have enough data for imputation.
#'
#' @importFrom collapse fsummarise fgroup_by fsubset frename qDF
#' @importFrom dplyr n
#' @importFrom tsibble as_tibble
#' @export
check_missing_countries <- function(.data,
                              .value_col,
                              .country_col,
                              .time_col,
                              .indicator_col,
                              .min_obs = 2) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .time_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if the value column exists
  if (!.value_col %in% colnames(.data)) {
    stop("The specified value column does not exist in the data.")
  }

  # Check if the indicator column exists
  if (!.indicator_col %in% colnames(.data)) {
    stop("The specified indicator column does not exist in the data.")
  }

  # Check if enough values for imputation
  .data |>
    tsibble::as_tibble() |>
    fgroup_by(c(.country_col, .indicator_col)) |>
    fsummarise(qDF(all(is.na(.data)), TRUE),
               length(is.na(.data)) -  qDF(sum(is.na(.data))),
               .cols = .value_col) |>
    frename(c("Missing", "Non_missing"), cols = c(length(c(.country_col, .indicator_col)) + 1, length(c(.country_col, .indicator_col)) + 2)) |>
    fsubset(Missing == TRUE | Non_missing < .min_obs) -> .data

  return(.data)
}



#' Season and trend decomposition using the feasts package
#'
#' This function performs seasonal and trend decomposition using the feasts package.
#'
#' @param .data A tsibble object containing the data to be decomposed.
#' @param .value_col A character vector specifying the name of the value column.
#' @param .season A character vector specifying the seasonality to be used. Default is NULL.
#' @param .trend A boolean value indicating whether to include the trend component. Default is TRUE.
#' @param .frequency A character vector specifying the frequency of the time series.
#' @param .imputed A boolean value indicating whether values were imputed. Default is NULL.
#' Default is "yearly". The other values are "monthly" and "quarterly".
#' @return A tsibble object with the decomposed data.
#'
#' @importFrom feasts STL
#' @importFrom tsibble has_gaps
#' @importFrom fabletools model
#' @importFrom generics components
#' @export
decompose_tsibble <- function(.data,
                               .value_col,
                               .season = NULL,
                               .trend = TRUE,
                               .frequency = "yearly",
                               .imputed = NULL) {
  # Check if the required columns exist in the data
  if (!all(c(.value_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if the frequency is valid
  if (!.frequency %in% c("yearly", "quarterly", "monthly")) {
    stop("Invalid frequency. Choose 'yearly', 'quarterly', or 'monthly'.")
  }

  # Construct formula for decomposition
  if (!is.null(.season)) {
    formula <- as.formula(paste(.value_col, "~ trend() + season(period = ", .season, ")"))
  } else {
    formula <- as.formula(paste(.value_col, "~ trend()"))
  }

  # Check for gaps in the data using the tsibble package
  .gaps <- tsibble::has_gaps(.data)
  if(any(.gaps$.gaps)) {
    stop("The data contains gaps. Consider filling them before decomposition and using .impute = TRUE")
  }

  # Check for missing values in the data
  if (any(is.na(.data[[.value_col]]))) {
    stop("The data contains missing values. Consider filling them before decomposition or using .impute = TRUE with prefered method")
  }

  # Perform decomposition using the STL method
  .data |>
    fabletools::model(feasts::STL(formula)) -> .data_decomposed

  # Extract components
  .data_decomposed |>
    fabletools::components() -> .data_decomposed

  return(.data_decomposed)
}

#' Normalize and detrend the series for each country
#'
#' This function normalizes and detrends the series for each country.
#'
#' @param .data A data frame containing the data to be normalized and detrended.
#' @param .value_col A character with the name of the column to be normalized and detrended,
#' can be integer refering to indexes of the columns (e.g., 8), or the name
#' of the column (e.g., "AG.CON.FERT.PT.ZS" or "value").
#' @param .country_col A character vector specifying the name(s) of the country
#' identifiers
#' @param .time_col A character vector specifying the name(s) of the time
#' identifiers.
#' @param .indicator_col A character vector specifying the name of the indicator
#' identifiers.
#' @param .frequency A character vector specifying the frequency of the time series.
#' Default is "yearly". The other values are "monthly" and "quarterly".
#' @param .detrend A logical value indicating whether to detrend the data. The
#' \code{STL} function from the \code{feasts} is used for smoothing. Default is TRUE.
#' @param .season A numeric value specifying the seasonality to be used. Default is NULL.
#' If NULL, the function will use the default seasonality of the data.
#' @param .impute A logical value indicating whether to impute missing values using the
#' specified method from \code{imputeTS}. Default is TRUE.
#' @param .method A character vector specifying the imputation method to be used.
#' The default is \code{na_interpolation}. Other options are: na_kalman, na_mean,
#' na_locf, na_ma, na_seadec, na_random. Check \code{imputeTS} for more details.
#' @param .long_format A logical value indicating whether to return the data in long format.
#' Default is TRUE.
#' @param .keep_decomp A logical value indicating whether to keep the decomposition
#' components in the output. Default is FALSE.
#' @param ... Additional arguments to be passed to the imputation function.
#' See \code{imputeTS} for more details on the different methods.
#' @return A data frame with the normalized and detrended data.
#'
#'
#' @importFrom collapse fscale fsd fmutate fselect BY pivot frename ftransform get_vars fungroup
#' @importFrom imputeTS na_interpolation na_kalman na_mean na_locf na_ma na_seadec na_random
#' @importFrom tsibble has_gaps fill_gaps
#' @export
normalize <- function(.data,
                          .value_col,
                          .country_col,
                          .time_col,
                          .indicator_col,
                          .frequency = "yearly",
                          .detrend = TRUE,
                          .season = NULL,
                          .impute = TRUE,
                          .method = "na_interpolation",
                          .long_format = TRUE,
                          .keep_decomp = FALSE,
                          ...) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .time_col, .indicator_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if the value column exists
  if (!.value_col %in% colnames(.data)) {
    stop("The specified value column does not exist in the data.")
  }

  # Check the input of columns, if numbers obtain columns and check if they exist
  if(length(.value_col) > 1) {
    warning("Multiple columns specified to normalize. Only the first one will be used. Make sure the data is long format.")
    .value_col <- .value_col[1]
  }

  # Check if the value column is numeric
  if(!is.numeric(.data[[.value_col]])) {
      warning("The column specified is not numeric. Converting to numeric.")
      .data[[.value_col]] <- as.numeric(.data[[.value_col]])
  }

  # Check if data is tsibble, if not convert to tsibble
  if (!class(.data) %in% c("tbl_ts", "tbl_df")) {
    .data <- convert_to_tsibble(.data,
                      .country_col = .country_col,
                      .time_col = .time_col,
                      .indicator_col = .indicator_col,
                      .frequency = .frequency,
                      .long_format = TRUE)
  }

  # Check the tsibble for gaps, if so, correct them
  .gaps <- tsibble::has_gaps(.data)
  if(any(.gaps$.gaps)) {
    warning("The data contains gaps. Will include missing period and impute them.")
    .data <- tsibble::fill_gaps(.data)
    .total_gaps <- sum(tsibble::count_gaps(.data, .full = TRUE)$.n)
  } else {
    .total_gaps <- 0
  }

  # Count the total number of missing values
  .total_missing <- sum(is.na(.data[[.value_col]]))

  # Try to impute missing values using the specified method
  if (.impute & !is.null(.method)) {
    .data <- impute_missing(.data,
                             .value_col = .value_col,
                             .country_col = .country_col,
                             .time_col = .time_col,
                             .indicator_col = .indicator_col,
                             .method = .method,
                             ...)
  } else if (.impute & is.null(.method)) {
    warning("The imputation method is not specified. No imputation will be performed.")
  } else {
    warning("No imputation will be performed.")
  }

  # Detrend the data using the decompose_tsibble function
  if (.detrend) {
    .data_decomp <- decompose_tsibble(.data,
                                .value_col = .value_col,
                                .season = .season,
                                .trend = TRUE,
                                .frequency = .frequency,
                                .imputed = .impute)
  } else {
    # Copy .value_col as Zscore
    .data[["Zscore"]] <- .data[[.value_col]]
  }

  # If imputation, include the column Imputed
  if (.impute & .detrend) {
    .data <- .data_decomp |>
      ## fselect(-.model) |>
      frename(remainder = "Zscore") |>
      collapse::join(.data, how = "left", overid = 0, verbose = FALSE)
  } else if (!.impute & .detrend) {
    .data <- .data_decomp |>
      ## fselect(-.model) |>
      frename(remainder = "Zscore")
  }

  if (!"Zscore" %in% colnames(.data)){
    .data[["Zscore"]] <- .data[[.value_col]]
  }

  # If keep_decomp is TRUE, keep the decomposition components
  if (!.keep_decomp & .detrend) {
    .data <- .data |>
      fselect(-.model, -trend, -season_adjust)
    # If season specified, exclude season_year from variables
    if (!is.null(.season) | .frequency != "yearly") {
      collapse::get_vars(.data, "season*", regex = TRUE) <- NULL
    }
  }



  # Try to transform data into a data.frame and then group them by country
  # and indicator using fgroup_by
  tryCatch({
    .data <- as.data.frame(tsibble::as_tibble(.data))
    .data <- collapse::fgroup_by(.data, c(.country_col, .indicator_col))
  }, error = function(e) {
    warning("Error converting to data frame: ", e$message)
  })

  # Normalize the Zscore
  .data <- .data |>
    fmutate(
      Zscore_norm = fscale(Zscore),
      Zscore_sd = fsd(Zscore, TRA = 1),
      Zscore = Zscore_norm,
      Zscore = replace(Zscore_norm, Zscore_sd < 1e-13, NA)
    ) |>
    fselect(-Zscore_norm, -Zscore_sd)

  # Check if long format is FALSE, if so, pivot to wider format
  if (!.long_format) {
    .data <- pivot(
      .data,
      how = "wider",
      ids = c(.country_col, .time_col),
      values = c("Zscore"),
      names = .indicator_col
    )
  }

  # Ungroup and define new class with grouping columns as attributes
  .data <- fungroup(.data)
  attr(.data, "country_columns") <- .country_col
  attr(.data, "time_columns") <- .time_col
  attr(.data, "indicator_columns") <- .indicator_col
  attr(.data, "value_column") <- .value_col
  attr(.data, "frequency") <- .frequency
  attr(.data, "total_gaps") <- .total_gaps
  attr(.data, "detrend") <- .detrend
  attr(.data, "impute") <- .impute
  attr(.data, "impute_method") <- .method
  attr(.data, "total_missing") <- .total_missing
  attr(.data, "season") <- .season
  attr(.data, "keep_decomp") <- .keep_decomp
  attr(.data, "long_format") <- .long_format
  attr(.data, "class") <- c("maly_norm", class(.data))

  return(.data)
}


#' Summary method for maly_norm class
#'
#' This function prints the summary of the maly_norm object.
#'
#' @param x A maly_norm object.
#' @param ... Additional arguments (not used).
#'
#' @export
#' @method summary maly_norm
#' @order 1
summary.maly_norm <- function(x, ...){
  cat("Macroanomaly Normalized Object\n")
  cat("Country Columns: ", paste(attr(x, "country_columns"), collapse = ", "), "\n")
  cat("Total number of countries: ", length(unique(x[[attr(x, "country_columns")[1]]])), "\n")
  cat("Time Columns: ", paste(attr(x, "time_columns"), collapse = ", "), "\n")
  cat("Time column from: ", as.character(min(x[[attr(x, "time_columns")[1]]])), " to ", as.character(max(x[[attr(x, "time_columns")[1]]])), "\n")
  cat("Total number of gaps: ", attr(x, "total_gaps"), "\n")
  cat("Indicator Columns: ", paste(attr(x, "indicator_columns"), collapse = ", "), "\n")
  cat("Number of indicators: ", length(unique(x[[attr(x, "indicator_columns")[1]]])), "\n")
  cat("Value Column: ", attr(x, "value_column"), "\n")
  cat("Total number of missing values: ", attr(x, "total_missing"), "\n")
  cat("Frequency: ", attr(x, "frequency"), "\n")
  cat("\n")
  cat("Specified options:\n")
  cat("Detrend: ", ifelse(attr(x, "detrend"), "TRUE", "FALSE"), "\n")
  cat("Impute: ", ifelse(attr(x, "impute"), "TRUE", "FALSE"), "\n")
  cat("Impute Method: ", attr(x, "impute_method"), "\n")
  cat("Season: ", ifelse(is.null(attr(x, "season")), "NULL", attr(x, "season")), "\n")
  cat("Keep Decomposition Variables: ", ifelse(attr(x, "keep_decomp"), "TRUE", "FALSE"), "\n")
  cat("Long Format: ", ifelse(attr(x, "long_format"), "TRUE", "FALSE"), "\n")
  cat("\n")
  cat("Data Summary:\n")
  cat("Summary of: ", attr(x, "value_column"), "\n")
  print(summary(x[[attr(x, "value_column")]]))
  cat("\n")
  cat("Summary of Zscore:\n")
  print(summary(x$Zscore))
  cat("\n")
}

#' Plot method for maly_norm class
#'
#' This function plots the original series and the Zscore of the maly_norm object,
#' for selected country and indicator.
#'
#' @param x A maly_norm object.
#' @param country A character vector specifying the country to plot.
#' Default is NULL, which means first country in the data will be used.
#' @param indicator A character vector specifying the indicator to plot.
#' Default is NULL, which means first indicator in the data will be used.
#' @param x.lab A string specifying the label for the x-axis.
#' Default is NULL, which means the time column will be used.
#' @param y.lab A string specifying the label for the y-axis of the original value (top graph).
#' Default is NULL, which means the value column will be used.
#' @param ... Additional arguments to be passed to the plot function.
#'
#' @return A ggplot object with the original series and the Zscore of the maly_norm object.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme element_text facet_wrap scale_color_manual scale_shape_manual
#' @importFrom rlang sym
#' @importFrom collapse fsubset
#' @importFrom patchwork plot_layout
#'
#' @export
#' @method plot maly_norm
plot.maly_norm <- function(x, country = NULL, indicator = NULL, x.lab = NULL, y.lab = NULL, ...) {
  # Check if country and indicator are specified
  if (is.null(indicator)) {
    indicator <- unique(x[[attr(x, "indicator_columns")[1]]])[1]
  }
  if (is.null(country)) {
    # Filter the first country in the data with data of the indicator
    country <- sort(unique(x[ x[[attr(x, "indicator_columns")[1]]] == indicator , attr(x, "country_columns")[1]]))[1]
  }


  # Filter data for the specified country and indicator
  .data <- x[x[[attr(x, "country_columns")[1]]] %in% country &
               x[[attr(x, "indicator_columns")[1]]] %in% indicator, ]

  # Check if the data is empty
  if (nrow(.data) == 0) {
    stop(paste("No data found for country:", country, "and indicator:", indicator))
  }

  # Check if the time column exists
  if (!attr(x, "time_columns")[1] %in% colnames(.data)) {
    stop(paste("The time column", attr(x, "time_columns")[1], "does not exist in the data."))
  }

  # Check if the frequency is yearly or not
  if (attr(x, "frequency") == "yearly") {
    # Convert the time column to Date if it is not already
    if (!inherits(.data[[attr(x, "time_columns")[1]]], "Date")) {
      .data[[attr(x, "time_columns")[1]]] <- as.Date(paste(.data[[attr(x, "time_columns")[1]]], "-01-01", sep = ""))
    }
  } else {
    if (!inherits(.data[[attr(x, "time_columns")[1]]], "Date")) {
      .data[[attr(x, "time_columns")[1]]] <- as.Date(.data[[attr(x, "time_columns")[1]]])
    }
  }

  # Check if the value column exists
  if (!attr(x, "value_column") %in% colnames(.data)) {
    stop(paste("The value column", attr(x, "value_column"), "does not exist in the data."))
  }

  # Check if the Zscore column exists
  if (!"Zscore" %in% colnames(.data)) {
    stop("The Zscore column does not exist in the data. Consider running the normalize function first.")
  }

  # Relabel Imputed column if it exists
  if ("Imputed" %in% colnames(.data)) {
    .data$Imputed <- factor(.data$Imputed, levels = c(TRUE, FALSE), labels = c("Imputed", "Not Imputed"))
  }

  # Check if x.lab and y.lab are specified, if not, use the time and value columns
  if (is.null(x.lab)) {
    x.lab <- attr(x, "time_columns")[1]
  }

  if (is.null(y.lab)) {
    y.lab <- attr(x, "value_column")
  }

  # Create the plot based on imputation status
  if (!"Imputed" %in% colnames(.data)) {
    ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
      geom_line(alpha = 0.3) +
      geom_point() +
      facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
      guides(alpha = "none") +
      labs(title = paste("Original Series for country", country, " and series", indicator),
           x = x.lab,
           y = y.lab) -> original_plot

    ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
      geom_point() +
      facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
      theme(axis.text = element_text(size = 8), legend.position = "bottom") +
      labs(title = paste("Zscore for country", country, " and series", indicator),
           x = x.lab,
           y = "Zscore") -> zscore_plot
  } else{
    ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
      geom_line(alpha = 0.3) +
      geom_point(aes(color = Imputed), show.legend=TRUE) +
      scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
      # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
      facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
      guides(alpha = "none") +
      theme(axis.text = element_text(size = 8), legend.position = "bottom") +
      labs(title = paste("Original Series for country", country, " and series", indicator),
           color = "Imputation Status",
           x = x.lab,
           y = y.lab) -> original_plot

    ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
      geom_point(aes(color = Imputed), show.legend=TRUE) +
      scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
      # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
      facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
      labs(title = paste("Zscore for country", country, " and series", indicator),
           color = "Imputation Status",
           x = x.lab,
           y = "Zscore") -> zscore_plot
  }


  # Combine the two plots
  combined_plot <- original_plot / zscore_plot +
    plot_layout(guides = "collect") & theme(axis.text = element_text(size = 8),
                                            legend.position = "bottom",
                                            legend.direction = "horizontal")

  return(combined_plot)
}

