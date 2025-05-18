#' Function to apply outlier detection
#'
#' @description This function applies outlier detection to a given dataset using the specified method.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .method A string specifying the outlier detection method to be used. Options are "zscore", "iqr", or "mad".
#'
#'
#'
#'
#'
#'






#' Function to apply the isotree algorithm
#'
#' @description This function applies the isotree algorithm to a given dataset for outlier detection.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .ndim A numeric value specifying the number of dimensions for the isotree algorithm. Default is 1.
#' @param .ntrees A numeric value specifying the number of trees to be used in the isotree algorithm. Default is 10.
#' @param .nthreads A numeric value specifying the number of threads to be used in the isotree algorithm. Default is 2.
#' @param .threshold A numeric value specifying the threshold for outlier detection. Default is 0.5.
#' @param ... Additional arguments to be passed to the isotree algorithm.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier score and indicator
#' based on the specified threshold.
#'
#' @importFrom isotree isolation.forest predict.isolation_forest
#' @importFrom collapse fmutate roworder fungroup ftransform ftransformv fselect
#' @export
isotree_detection <- function(.data, .cols = NULL, .ndim = 1, .ntrees = 10, .nthreads = 2, .threshold = 0.5, ...) {
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
  model <- isotree::isolation.forest(.data, ndim = .ndim, ntrees = .ntrees, nthreads = .nthreads, ...)

  # Predict outlier scores
  outlier_scores <- predict(model, .data, type = "score")

  # Create a data frame with the original dataset and outlier scores
  .data <- .data |>
    fungroup() |>
    fmutate(outlier_score = outlier_scores,
      outlier_indicator = ifelse(outlier_score > .threshold, TRUE, FALSE)) |>
    roworder(-outlier_score)

  return(.data)
}


#' Function to detect outliers using the outliertree package
#'
#' @description This function detects outliers in a dataset using the outliertree package.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .cols A character vector specifying the columns to be used for outlier detection. Default is NULL, which uses all columns.
#' @param .save_outliers A logical value indicating whether to save the outliers. Default is TRUE.
#' @param .nthreads A numeric value specifying the number of threads to be used. Default is 2.
#' @param ... Additional arguments to be passed to the outliertree function.
#'
#' @return A data frame containing the original dataset with an additional column indicating the outlier status.
#'
#' @importFrom outliertree outlier.tree
#' @importFrom collapse fmutate roworder fungroup
#' @export
outliertree_detection <- function(.data, .cols = NULL, .save_outliers = TRUE, .nthreads = 2, ...) {
  # Check if the outliertree package is installed
  if (!requireNamespace("outliertree", quietly = TRUE)) {
    stop("The outliertree package is required for this function. Please install it.")
  }

  # If .cols is NULL, select columns specified
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
            outlier_indicator = ifelse(outlier_score > 0.5, TRUE, FALSE)) |>
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
            rank = rank(-absZscore),
            zscore_threshold = absZscore > 3) |>
    roworder(rank)

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
            outlier_indicator = ifelse(Zscore < lower_bound | Zscore > upper_bound, TRUE, FALSE)) |>
    fselect(-IQR, -lower_bound, -upper_bound)

  return(.data)
}
