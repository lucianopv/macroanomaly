#' Function to apply outlier detection
#'
#' @description This function applies outlier detection to a given dataset using the specified method.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .method A single character string or character vector specifying the outlier detection method to be used.
#' Options are \code{"zscore"}, \code{"tsoutlier"}, \code{"isotree"}, \code{"outliertree"}, or \code{"capa"}.
#' \code{"zscore"} is the default method, it calculates the Z-score for each observation and identifies outliers
#' based on a specified threshold (defaults to 3). \code{"tsoutlier"} uses the IQR method to detect outliers in
#' time series data, identifying values that are greater than 3 (default) times the IQR or less than -3 (default)
#' times the IQR (one can change this threhold). \code{"isotree"} applies the \code{isotree} algorithm of the
#' \code{isotree} package to detect outliers. Similarly, \code{"outliertree"} applies the \code{outliertree} algorithm
#' of the \code{outliertree} package to detect outliers. Finally, \code{"capa"} applies the \code{capa} method
#' of the \code{anomaly} package to detect point and collective anomalies.
#' @param .country_col A character vector specifying the column names for country identifiers. Default is
#' \code{c("Country.Code", "Country.Name")}.
#' @param .time_col A character string specifying the column name for time identifiers. Default is \code{"Year"}.
#' @param .indicator_col A character string specifying the column name for indicator identifiers. Default is \code{"Indicator.Code"}.
#' @param ... Additional arguments to be passed to the specific outlier detection method. See Details
#' for arguments for each method.
#'
#' @details
#' The function supports multiple outlier detection methods:
#' - \code{"zscore"}: Detects outliers based on Z-scores. The threshold can be adjusted using the \code{.threshold} argument.
#' - \code{"tsoutlier"}: Detects outliers using the IQR method. The threshold can be adjusted using the \code{.threshold} argument.
#' - \code{"isotree"}: Applies the isotree algorithm. The number of dimensions, trees, threads, sample size, and threshold can be adjusted using
#'   the \code{.ndim}, \code{.ntrees}, \code{.nthreads}, \code{.sample_size}, and \code{.threshold} arguments, respectively.
#' - \code{"outliertree"}: Applies the outliertree algorithm. The columns to be used for outlier detection can be specified using
#'   the \code{.cols} argument. The threshold can be adjusted using the \code{.threshold} argument.
#' - \code{"capa"}: Applies the capa method from the anomaly package. The type of anomaly detection can be specified using the
#'   \code{.type} argument, which can be "robustmean" (default), "mean", or "meanvar". The minimum segment length can be adjusted using the
#'   \code{.min_seg_len} argument.
#'
#'
#'
#' @return A data frame containing the original dataset with additional columns indicating the outlier status.
#'
#' @importFrom R.utils doCall
#'
#'
#'@export
detect <- function(.data,
                   .method = "zscore",
                   .country_col = c("Country.Code", "Country.Name"),
                   .time_col = "Year",
                   .indicator_col = "Indicator.Code",
                   ...) {
  # Check if the method is valid
  valid_methods <- c("zscore", "tsoutlier", "isotree", "outliertree", "capa")
  if (!any(.method %in% valid_methods)) {
    stop(paste("Invalid method specified. Choose from:", paste(valid_methods, collapse = ", ")))
  }

  if (length(.method) == 1) {
    # Call the appropriate detection function based on the method
    results <- switch(.method,
         zscore = doCall(zscore_detection, .data = .data, ...),
         tsoutlier = doCall(tsoutliers_detection, .data  = .data, ...),
         isotree = doCall(isotree_detection, .data  = .data, ...),
         outliertree = doCall(outliertree_detection, .data = .data, ...),
         capa = doCall(capa_detection, .data = .data, .country_col = .country_col,
                       .time_col = .time_col, .indicator_col = .indicator_col, ...))

    return(results)
   } else if (length(.method) > 1) {
      # If multiple methods are specified, apply each method and combine results
      results <- lapply(.method, function(method) {
        switch(method,
               zscore = doCall(zscore_detection, .data = .data, ...),
               tsoutlier = doCall(tsoutliers_detection, .data = .data, ...),
               isotree = doCall(isotree_detection, .data = .data, ...),
               outliertree = doCall(outliertree_detection, .data = .data, ...),
               capa = doCall(capa_detection, .data = .data, .country_col = .country_col,
                             .time_col = .time_col, .indicator_col = .indicator_col, ...)
        )
      })

      # Select the original columns names
      .columns <- names(.data)

      # Add suffix to the outlier indicator columns
      results <- mapply(function(res, method) {
        .columns_to_replace <- colnames(res)[!colnames(res) %in% .columns]
        # Add suffix to the outlier indicator columns
        colnames(res)[colnames(res) %in% .columns_to_replace] <- paste0(colnames(res)[colnames(res) %in% .columns_to_replace], "_", method)
        # Return the modified data frame
        return(res)
      }, results, .method, SIMPLIFY = FALSE)

      # Combine results into a single data frame
      combined_results <- Reduce(function(x, y) {
        merge(x, y, by = .columns, suffixes = , all = TRUE)
      }, results)

      # Names of variables with outlier_indicator
      outlier_cols <- grep("outlier_indicator", names(combined_results), value = TRUE)

      # Create a new column that combines all outlier indicators
      combined_results$outlier_indicator_votes <- rowSums(combined_results[outlier_cols], na.rm = TRUE)

      return(combined_results)
    }
}





#' Function to apply the isotree algorithm
#'
#' @description This function applies the isotree algorithm to a given dataset for outlier detection.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .ndim A numeric value specifying the number of dimensions for the isotree algorithm. Default is 1.
#' @param .ntrees A numeric value specifying the number of trees to be used in the isotree algorithm. Default is 10.
#' @param .nthreads A numeric value specifying the number of threads to be used in the isotree algorithm. Default is 2.
#' @param .sample_size A numeric value specifying the sample size for the isotree algorithm. Default is 1, which means the entire dataset is used.
#' @param .threshold A numeric value specifying the threshold for outlier detection. Default is 0.5.
#' @param ... Additional arguments to be passed to the isotree algorithm.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier score and indicator
#' based on the specified threshold.
#'
#' @importFrom isotree isolation.forest predict.isolation_forest
#' @importFrom collapse fmutate roworder fungroup ftransform ftransformv fselect
#' @export
isotree_detection <- function(.data,
                              .cols = NULL,
                              .ndim = 1,
                              .ntrees = 10,
                              .nthreads = 2,
                              .sample_size = 1,
                              .threshold = 0.5, ...) {
  # Check if the isotree package is installed
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("The isotree package is required for this function. Please install it.")
  }

  # If .cols is NULL, select columns specified
  if (!is.null(.cols)){
    .data <- .data |>
      fselect(.cols)
  }

  # Check if any column is class "vctrs_vctr", if so, change it to Date
  if (any(sapply(.data, function(x) inherits(x, "vctrs_vctr")))) {
    # Obtain index of column with class "vctrs_vctr"
    vctrs_col_index <- which(sapply(.data, function(x) inherits(x, "vctrs_vctr")))
    .data <- .data |>
      ftransformv(vctrs_col_index, as.Date)
  }

  # Apply the isotree algorithm
  model <- isotree::isolation.forest(.data, ndim = .ndim, ntrees = .ntrees, nthreads = .nthreads, sample_size = .sample_size,  ...)

  # Predict outlier scores
  outlier_scores <- predict(model, .data, type = "score")

  # Convert back to original date format
  # TODO: Need to go back so we can merge with others, or create an if clause in detect

  # Create a data frame with the original dataset and outlier scores
  .data <- .data |>
    fungroup() |>
    fmutate(outlier_score = outlier_scores,
      outlier_indicator = ifelse(outlier_score > .threshold, 1, 0)) |>
    roworder(-outlier_score)

  return(.data)
}


#' Function to detect outliers using the outliertree package
#'
#' @description This function detects outliers in a dataset using the outliertree package.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .cols A character vector specifying the columns to be used for outlier detection. Default is NULL, which uses all columns.
#' @param .threshold A numeric value specifying the threshold for outlier detection. Default is 0.5.
#' @param .save_outliers A logical value indicating whether to save the outliers. Default is TRUE.
#' @param .nthreads A numeric value specifying the number of threads to be used. Default is 2.
#' @param ... Additional arguments to be passed to the outliertree function.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier status.
#'
#' @importFrom outliertree outlier.tree
#' @importFrom collapse fmutate roworder fungroup
#' @export
outliertree_detection <- function(.data, .cols = NULL, .threshold = 0.5, .save_outliers = TRUE, .nthreads = 2, ...) {
  # Check if the outliertree package is installed
  if (!requireNamespace("outliertree", quietly = TRUE)) {
    stop("The outliertree package is required for this function. Please install it.")
  }

  # If .cols is not NULL, select columns specified
  if (!is.null(.cols)){
    .data <- .data |>
      fselect(.cols)
  }

  .data <- fungroup(.data)

  # Apply the outliertree algorithm
  model <- outliertree::outlier.tree(.data, save_outliers = .save_outliers, nthreads = .nthreads, ...)

  # Predict outlier scores
  outlier_scores <- predict(model, .data)

  # Create a data frame with the original dataset and outlier scores
  .data <- .data |>
    fungroup() |>
    fmutate(outlier_score = outlier_scores,
            outlier_indicator = ifelse(outlier_score > .threshold, 1, 0)) |>
    roworder(-outlier_score)

  return(.data)
}

#' Function to detect outliers using the Zscore and ranking it
#'
#' @description This function detects outliers in a dataset using the Z-score method and ranks them based on their Z-scores.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .threshold A numeric value specifying the Z-score threshold for outlier detection. Default is 3.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier status
#'
#' @importFrom collapse fungroup fmutate roworder
#' @export
zscore_detection <- function(.data, .threshold = 3) {
  # Identify outliers based on the Z-score threshold
  .data <- .data |>
    fungroup() |>
    fmutate(absZscore = abs(Zscore),
            rankZscore = rank(-absZscore),
            outlier_indicator = as.numeric(absZscore > 3)) |>
    roworder(rankZscore)

  return(.data)
}


#' Function to detect outliers using the IQR method (tsoutliers from \code{forecast})
#'
#' @description This function detects outliers in a dataset using the IQR method. It follows
#' the approach described in the \code{forecast} package. An outlier is defined as the remainder
#' of the series, after removing the trend and seasonal components, that is greater than 3 times the IQR
#' or less than -3 times the IQR.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .threshold A numeric value specifying the IQR threshold for outlier detection. Default is 3.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier status
#'
#' @importFrom collapse fungroup fmutate fselect
#' @export
tsoutliers_detection <- function(.data, .threshold = 3) {
  # Identify outliers based on the IQR method
  .data <- .data |>
    fmutate(IQR = IQR(Zscore, na.rm = TRUE),
            lower_bound = quantile(Zscore, 0.25, na.rm = TRUE) - .threshold * IQR,
            upper_bound = quantile(Zscore, 0.75, na.rm = TRUE) + .threshold * IQR,
            outlier_indicator = ifelse(Zscore < lower_bound | Zscore > upper_bound, 1, 0)) |>
    fselect(-IQR, -lower_bound, -upper_bound)

  return(.data)
}


#' Function to detect point anomalies in a dataset using capa of the anomaly package
#'
#' @description This function detects point anomalies in a dataset using the capa method from the anomaly package.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .type A character string specifying the type of anomaly detection to be used, based on the
#' capa method in the anomaly package. Default is "meanvar", which is for collective anomalies using joint
#' changes in mean and variance.
#'
#' @return A data frame containing the original dataset with an additional column indicating the point anomaly status.
#' @importFrom anomaly capa
#' @importFrom collapse fmutate fungroup fselect fgroup_by BY GRP fcumsum na_locf frename join GRPnames unlist2d
#'
#' @export
capa_detection <- function(.data,
                           .type = "robustmean",
                           .min_seg_len = 2,
                           .country_col = c("Country.Code", "Country.Name"),
                           .time_col = "Year",
                           .indicator_col = "Indicator.Code") {
  # Check if the anomaly package is installed
  if (!requireNamespace("anomaly", quietly = TRUE)) {
    stop("The anomaly package is required for this function. Please install it.")
  }

  # Check columns exist
  if (!all(c(.country_col, .time_col, .indicator_col) %in% colnames(.data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }

  # Check if the .type argument is valid
  valid_types <- c("meanvar", "mean", "robustmean")
  if (!.type %in% valid_types) {
    stop(paste("Invalid type specified. Choose from:", paste(valid_types, collapse = ", ")))
  }

  # Check for missing values, remove them and alert the user
  if (any(is.na(.data$Zscore))) {
    warning("Missing values found in Zscore column. These will be removed from the analysis.")
    .data_sub <- .data |> fungroup() |> fsubset(!is.na(Zscore))
  } else {
    .data_sub <- .data
  }

  # Group the data by country and indicator
  .grouped_data <- .data_sub |>
    fungroup() |>
    fselect(.country_col, .time_col, .indicator_col, "Zscore") |>
    fgroup_by(c(.country_col, .indicator_col)) |>
    GRP()

  # Apply the capa method to detect point anomalies
  outliers <- unlist(BY(x = as.matrix(.data_sub[,"Zscore"]), g = .grouped_data, FUN = anomaly::capa, type = .type, min_seg_len = .min_seg_len, return = 4)[[1]])

  # Extract point anomalies using tryCatch to handle potential errors, if error, save as warning
  point_anomalies <- lapply(outliers, \(x)
                            tryCatch(anomaly::point_anomalies(x),
                                   error = function(e) {
                                     warning("Error in detecting point anomalies: ", e$message)
                                     return(data.frame(location = NA, variate = NA, strength = NA))
                                   }))
  names(point_anomalies) <- GRPnames(.grouped_data, sep = "._.")
  point_anomalies <- unlist2d(point_anomalies, idcols = "Indicator")

  .idx_point_anomalies <- as.data.frame(stringr::str_split_fixed(point_anomalies$Indicator, "._.", n = length(c(.country_col, .indicator_col))))
  colnames(.idx_point_anomalies) <- c(.country_col, .indicator_col)
  point_anomalies <- cbind(.idx_point_anomalies, point_anomalies[, -1])  # Remove the Indicator column

  # Extract the collective anomalies
  collective_anomalies <- lapply(outliers, anomaly::collective_anomalies)
  names(collective_anomalies) <- GRPnames(.grouped_data, sep = "._.")
  collective_anomalies <- unlist2d(collective_anomalies, idcols = "Indicator")

  .idx_collective_anomalies <- as.data.frame(stringr::str_split_fixed(collective_anomalies$Indicator, "._.", n = length(c(.country_col, .indicator_col))))
  colnames(.idx_collective_anomalies) <- c(.country_col, .indicator_col)
  collective_anomalies <- cbind(.idx_collective_anomalies, collective_anomalies[, -1])  # Remove the Indicator column
  collective_anomalies <- fmutate(collective_anomalies, location = start)


  # Combine the results with the original data
  .data <- .data |>
    fungroup() |>
    fgroup_by(c(.country_col, .indicator_col)) |>
    fmutate(location = 1,
            location = fcumsum(location),
            type = "point") |>
    join(point_anomalies, on = c(.country_col, .indicator_col, "location"), verbose = 0, overid = 2) |>
    frename(outlier_indicator = variate,
            capa_strength = strength) |>
    fmutate(outlier_indicator = ifelse(is.na(outlier_indicator), 0, outlier_indicator),
            type = ifelse(outlier_indicator == 0, NA, type)) |>
    fselect(-location)

  .data <- .data |>
    fungroup() |>
    fgroup_by(c(.country_col, .indicator_col)) |>
    fmutate(location = 1,
            location = fcumsum(location)) |>
    join(collective_anomalies, on = c(.country_col, .indicator_col, "location"), verbose = 0, overid = 2) |>
    fmutate(start = collapse::na_locf(start),
            end = collapse::na_locf(end),
            outlier_indicator = ifelse(!is.na(start) & !is.na(end) & location >= start & location <= end, 1, outlier_indicator),
            type = ifelse(location >= start & location <= end, "collective", type)) |>
    fselect(-c(start, end, location, start.lag, end.lag, variate))

  # TODO: What do we want to keep?

  return(.data)
}
