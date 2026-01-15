# global variables
utils::globalVariables(c("Zscore", "outlier_indicator", "Imputed", "outlier_indicator_total",
                      "absZscore", "rankZscore", "IQR", "lower_bound", "upper_bound",
                      "outlier_score", "capa_strength", "location", "type",
                      "strength", "capa_score", "isoforest_score",
                      "start", "end", "start.lag", "end.lag", "variate"))

#' Function to apply outlier detection
#'
#' @description This function applies outlier detection to a given dataset using the specified method.
#'
#' @param .x An object from the \code{normalize} function, which is a data frame containing the normalized
#' data to be analyzed.
#' @param .method A single character string or character vector specifying the outlier detection method to be used.
#' Options are \code{"zscore"}, \code{"tsoutlier"}, \code{"isotree"}, \code{"outliertree"}, or \code{"capa"}.
#' \code{"zscore"} is the default method, it calculates the Z-score for each observation and identifies outliers
#' based on a specified threshold (defaults to 3). \code{"tsoutlier"} uses the IQR method to detect outliers in
#' time series data, identifying values that are greater than 3 (default) times the IQR or less than -3 (default)
#' times the IQR (one can change this threshold). \code{"isotree"} applies the \code{isotree} algorithm of the
#' \code{isotree} package to detect outliers. Similarly, \code{"outliertree"} applies the \code{outliertree} algorithm
#' of the \code{outliertree} package to detect outliers. Finally, \code{"capa"} applies the \code{capa} method
#' of the \code{anomaly} package to detect point and collective anomalies.
#' @param .country_col A character vector specifying the column names for country identifiers. Not needed if these
#' are the same as the ones defined for the \code{normalize} function.
#' @param .time_col A character string specifying the column name for time identifiers.Not needed if these are the
#' same as the ones defined for the \code{normalize} function.
#' @param .indicator_col A character string specifying the column name for indicator identifiers. Not needed if these
#' are the same as the ones defined for the \code{normalize} function.
#' @param .value_col A character string specifying the column name for value identifiers. Not needed if these are the
#' same as the ones defined for the \code{normalize} function.
#' @param .additional_cols A logical value indicating whether to include additional columns in the output data frame.
#' @param .args A named list of additional arguments to be passed to the specific outlier detection methods. For example,
#' for \code{"zscore"}, one can specify the threshold using \code{.args = list(zscore = c(.threshold = 2))}. For
#' multiple methods, the list should contain named lists for each method, e.g.,
#' \code{.args = list(zscore = c(.threshold = 2), tsoutlier = c(.threshold = 2))}.
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
detect <- function(.x,
                   .method = "zscore",
                   .country_col = NULL,
                   .time_col = NULL,
                   .indicator_col = NULL,
                   .value_col = NULL,
                   .additional_cols = FALSE,
                   .args = list()) {
  # Check the input is a data frame and "maly_norm" class
  if (!inherits(.x, "maly_norm") || !is.data.frame(.x)) {
    stop("Input must be a data frame and 'maly_norm' class.", call. = FALSE)
  }


  # Check if the method is valid
  valid_methods <- c("zscore", "tsoutlier", "isotree", "outliertree", "capa")
  if (!any(.method %in% valid_methods)) {
    stop(paste("Invalid method specified. Choose from:", paste(valid_methods, collapse = ", ")), call. = FALSE)
  }

  # Define .country_col, .value_col, .time_col and .indicator_col using the attribute if values are NULL
  if (any(is.null(c(.country_col, .time_col, .indicator_col, .value_col)))){
    if (is.null(.country_col)) {
      .country_col <- attr(.x, "country_columns", exact = TRUE)
    }
    if (is.null(.time_col)) {
      .time_col <- attr(.x, "time_columns", exact = TRUE)
    }
    if (is.null(.indicator_col)) {
      .indicator_col <- attr(.x, "indicator_columns", exact = TRUE)
    }
    if (is.null(.value_col)) {
      .value_col <- attr(.x, "value_column", exact = TRUE)
    }
  }

  # Define the value column
  .value_col <- attr(.x, "value_column", exact = TRUE)

  # Check if the required columns exist in the data
  if (!all(c(.country_col, .time_col, .indicator_col) %in% colnames(.x))) {
    stop("One or more specified columns do not exist in the data frame.", call. = FALSE)
  }

  if (length(.method) == 1) {
    # Call the appropriate detection function based on the method
    results <- switch(.method,
         zscore = doCall(zscore_detection, .data = .x, args = .args[["zscore"]]),
         tsoutlier = doCall(tsoutliers_detection, .data  = .x, args = .args[["tsoutlier"]]),
         isotree = doCall(isotree_detection, .data  = .x, args = .args[["isotree"]]),
         outliertree = doCall(outliertree_detection, .data = .x, .value_col = .value_col, args = .args[["outliertree"]]),
         capa = doCall(capa_detection, .data = .x, .country_col = .country_col,
                       .time_col = .time_col, .indicator_col = .indicator_col, args = .args[["capa"]])
    )

    # If additional columns are specified, select them
    if (!.additional_cols) {
      # Select only the original columns and the outlier indicator columns
      .columns <- c(.country_col, .time_col, .indicator_col, .value_col, "Zscore", "outlier_indicator")
      if("Imputed" %in% colnames(.x)) {
        .columns <- c(.columns, "Imputed")
      }
      results <- results[, .columns, drop = FALSE]
    } else {
      # If additional columns are specified, keep all columns
      results <- results
    }

   } else if (length(.method) > 1) {
      # If multiple methods are specified, apply each method and combine results
      results <- lapply(.method, function(method) {
        switch(method,
               zscore = doCall(zscore_detection, .data = .x, args = .args[["zscore"]]),
               tsoutlier = doCall(tsoutliers_detection, .data = .x, args = .args[["tsoutlier"]]),
               isotree = doCall(isotree_detection, .data = .x, args = .args[["isotree"]]),
               outliertree = doCall(outliertree_detection, .data = .x, args = .args[["outliertree"]]),
               capa = doCall(capa_detection, .data = .x, .country_col = .country_col,
                             .time_col = .time_col, .indicator_col = .indicator_col, args = .args[["capa"]])
        )
      })

      # Select the original columns names
      .columns <- names(.x)

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
        merge(x, y, by = .columns, all = TRUE)
      }, results)

      # Names of variables with outlier_indicator
      outlier_cols <- grep("outlier_indicator", names(combined_results), value = TRUE)

      # Create a new column that combines all outlier indicators
      combined_results$outlier_indicator_total <- rowSums(combined_results[outlier_cols], na.rm = TRUE)

      # Sort according to the total outlier indicator column
      combined_results <- combined_results[order(-combined_results$outlier_indicator_total), ]

      # If .additional_cols is FALSE, select only the original columns and the outlier indicator columns
      # including the outlier_indicator for each method and the outlier_indicator_total
      if (!.additional_cols) {
        .columns <- c(.country_col, .time_col, .indicator_col, .value_col, outlier_cols, "Zscore", "outlier_indicator_total")
        if("Imputed" %in% colnames(.x)) {
          .columns <- c(.columns, "Imputed")
        }
        combined_results <- combined_results[, .columns]
      } else {
        # If additional columns are specified, keep all columns
        combined_results <- combined_results
      }

      results <- combined_results
   }

  # Save some of the original attributes in a new attribute
  .attr_norm_names <- c("frequency", "detrend", "impute", "impute_method",
                  "keep_decomp", "long_format")
  .attr_norm <- lapply(.attr_norm_names, \(x) attr(.x, x))
  names(.attr_norm) <- .attr_norm_names
  attr(results, "maly_norm_attrs") <- .attr_norm

  # Include new attributes from detect
  attr(results, "country_columns") <- .country_col
  attr(results, "time_columns") <- .time_col
  attr(results, "indicator_columns") <- .indicator_col
  attr(results, "value_column") <- attr(.x, "value_column", exact = TRUE)
  attr(results, "maly_detect_attr") <- list("method" = .method,
                                            "additional_cols" = .additional_cols)
  attr(results, "class") <- c("maly_detect", class(.x))

  return(results)
}





#' Function to apply the isotree algorithm
#'
#' @description This function applies the isotree algorithm to a given dataset for outlier detection.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .cols A character vector specifying the columns to be used for outlier detection. Default is NULL, which uses all columns.
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
#' @importFrom stats predict
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
    stop("isotree: The isotree package is required for this function. Please install it.", call. = FALSE)
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
    .original_data <- .data[, vctrs_col_index]
    .data <- .data |>
      ftransformv(vctrs_col_index, as.Date)

  }

  # Apply the isotree algorithm
  model <- isotree::isolation.forest(.data, ndim = .ndim, ntrees = .ntrees, nthreads = .nthreads, sample_size = .sample_size,  ...)

  # Predict outlier scores
  outlier_scores <- predict(model, .data, type = "score")


  # If the original data was a vctrs_vctr, replace the transformed dates with the original ones
  if (exists(".original_data")) {
    .data[, vctrs_col_index] <- .original_data
  }

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
#' @param .value_col A character string specifying the column name for value identifiers. Default is NULL, which uses the value column defined in the normalize function.
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
outliertree_detection <- function(.data, .cols = NULL, .value_col = NULL, .threshold = 0.5, .save_outliers = TRUE, .nthreads = 2, ...) {
  # Check if the outliertree package is installed
  if (!requireNamespace("outliertree", quietly = TRUE)) {
    stop("outliertree: The outliertree package is required for this function. Please install it.", call. = FALSE)
  }

  # Save the order of columns in the original data
  .original_cols_ord <- colnames(.data)

  # If .cols is not NULL, select columns specified
  if (!is.null(.cols)){
    # Check if Zscore is included in .cols, if not, add it and warn the user
    if (!"Zscore" %in% .cols) {
      warning("outliertree: Zscore column is not included in .cols. It will be added automatically.", call. = FALSE)
      .cols <- c(.cols, "Zscore")
    }

    # Check if the .value_col is included in .cols, if it is, remove it and warn the user
    if (!is.null(.value_col) && .value_col %in% .cols) {
      warning(paste("outliertree: It is not recommended that column", .value_col, "is included in the outlier detection. It will be excluded from the analysis."), call. = FALSE)
      .cols <- .cols[!(.cols %in% .value_col)]
    }

    # Select the non-selected columns for further join
    .original_cols <- colnames(.data)[!colnames(.data) %in% .cols]
    .original_data <- .data[, .original_cols, drop = FALSE]

    .data <- .data |>
      fselect(.cols)
  } else {
    # Record the names of the initial columns
    .cols <- colnames(.data)
    .original_data <- .data

    # Exclude the .value_col if it exists in the data and warn the user
    if (!is.null(.value_col) && .value_col %in% colnames(.data)) {
      warning(paste("outliertree: The column", .value_col, "is not included in the outlier detection. It will be excluded from the analysis."), call. = FALSE)
      .original_cols <- .cols
      .cols <- .cols[!.cols %in% .value_col]
      .data <- .data |>
        fselect(.cols)
    }
  }

  .data <- fungroup(.data)

  # Check if any column is class "vctrs_vctr", if so, change it to Date
  if (any(sapply(.data, function(x) inherits(x, "vctrs_vctr")))) {
    # Obtain index of column with class "vctrs_vctr"
    vctrs_col_index <- which(sapply(.data, function(x) inherits(x, "vctrs_vctr")))
    .original_time <- .data[, vctrs_col_index]
    .data <- .data |>
      ftransformv(vctrs_col_index, as.Date)

  }

  # Apply the outliertree algorithm
  capture.output(invisible(model <- outliertree::outlier.tree(.data, save_outliers = .save_outliers, nthreads = .nthreads, ...)))

  # Predict outlier scores
  cat("Message from outliertree:\n")
  outlier_scores <- predict(model, .data)

  # Check the outlier_score within each object in the list outlier_scores
  # If NA, then no outliers were detected and create a vector of zeros
  if (all(unlist(lapply(outlier_scores, \(x) is.na(x$outlier_score))))) {
    outlier_scores <- rep(0, nrow(.data))
  } else {
    # If not NA, extract the outlier scores
    outlier_scores <- unlist(lapply(outlier_scores, function(x) x$outlier_score))
    outlier_scores <- ifelse(!is.na(outlier_scores), 1, 0)
  }

  # If .cols is not NULL, join the original data with the outlier scores
  # if (!is.null(.cols)) {
    # Join the original data with the outlier scores
  .data <- cbind(.original_data, .data)
  # }

  # If the original data was a vctrs_vctr, replace the transformed dates with the original ones
  if (exists(".original_time")) {
    .data[, names(vctrs_col_index)] <- .original_time
  }

  # Order the columns in the original order
  .data <- .data[, .original_cols_ord, drop = FALSE]

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
            outlier_indicator = as.numeric(absZscore > .threshold)) |>
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
#' @importFrom stats IQR quantile
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

#' Function to return point anomalies
#'
#' @param object An instance of an S4 class produced by \code{\link{capa}}.
#' @param epoch Positive integer. CAPA methods are sequential and as such, can generate results up to, and including, any epoch within the data series. This can be controlled by the value
#' of \code{epoch} and is useful for examining how the inferred anomalies are modified as the data series grows. The default value for \code{epoch} is the length of the data series.
#'
#' @return A data frame.
#'
#' @export
point_anomalies <- function(object,epoch=NULL) {
        if (is.null(epoch)) {
          epoch <- nrow(object@data)
        }
        if(epoch < 0)
            {
              stop("epoch should be a positive integer")
            }
            if(epoch > nrow(object@data))
            {
              stop("epoch cannot be greater than the number of observations in the data")
            }
            # get the anomalies
            anoms<-anomaly:::anomalies(object,epoch)
            # transform data
            data_dash<-object@data
            p_anoms<-Map(function(x) x[1],Filter(function(x) x[1] == x[2],anoms))
            p_anom_daf <- NULL
            if(length(p_anoms) > 0)
            {
              p_anom_daf <- Reduce(rbind,
                                   Map(
                                     function(p_anom)
                                     {
                                       variates<-seq(1,ncol(data_dash))[as.logical(object@components[p_anom[1],])]
                                       location<-rep(p_anom[1],length(variates))
                                       strength<-abs(data_dash[p_anom[1],variates])
                                       return(
                                         data.frame("location"=location,
                                                    "variate"=variates,
                                                    "strength"=strength)
                                       )
                                     },
                                     p_anoms)
              )
            }

            extra_anoms <- data.frame("location"=integer(0),"variate"=integer(0),"strength"=integer(0))

            if (object@type == "robustmean"){

              tmp <- collective_anomalies(as(object,"capa.class"))

              if (nrow(tmp)>0)
              {

                extra_anoms <- Reduce(rbind,
                                      Map(
                                        function(ii)
                                        {
                                          relevant_row = tmp[ii,]
                                          if (is.null(relevant_row$start.lag)){
                                            effective_start = relevant_row$start
                                            effective_end   = relevant_row$end
                                          } else{
                                            effective_start = relevant_row$start+relevant_row$start.lag
                                            effective_end   = relevant_row$end-relevant_row$start.lag
                                          }
                                          x_data = data_dash[effective_start:effective_end,relevant_row$variate]
                                          standardised_x_data = x_data - anomaly:::tukey_mean(x_data,sqrt(object@beta_tilde))

                                          location<-which(abs(standardised_x_data)>sqrt(object@beta_tilde))
                                          strength<-abs(standardised_x_data[location])
                                          variates<-rep(relevant_row$variate,length(location))
                                          if (length(location > 0)){
                                            location = location - 1 + effective_start
                                          }
                                          return(
                                            data.frame("location"=location,
                                                       "variate"=variates,
                                                       "strength"=strength)
                                          )
                                        },
                                        1:nrow(tmp))
                )

              }

            }

            if(length(p_anoms) + nrow(extra_anoms) == 0)
            {
              return(data.frame("location"=integer(0),"variate"=integer(0),"strength"=integer(0)))
            }
            else
            {
              out <- rbind(p_anom_daf,extra_anoms)
              return(out[order(out$location,out$variate),])
            }
          }




#' Function to detect point anomalies in a dataset using capa of the anomaly package
#'
#' @description This function detects point anomalies in a dataset using the capa method from the anomaly package.
#'
#' @param .data A data frame containing the dataset to be analyzed.
#' @param .type A character string specifying the type of anomaly detection to be used, based on the
#' capa method in the anomaly package. Default is "meanvar", which is for collective anomalies using joint
#' changes in mean and variance.
#' @param .min_seg_len A numeric value specifying the minimum segment length for the capa method. Default is 2.
#' @param .country_col A character vector specifying the column names for country identifiers.
#' @param .time_col A character string specifying the column name for time identifiers.
#' @param .indicator_col A character string specifying the column name for indicator identifiers.
#'
#' @return A data frame containing the original dataset with an additional column indicating the point anomaly status.
#'
#' @importFrom collapse fmutate fungroup fselect fgroup_by BY GRP fcumsum frename join GRPnames unlist2d
#' @importFrom anomaly capa collective_anomalies
#' @importFrom stringr str_split_fixed
#' @export

capa_detection <- function(.data,
                           .type = "robustmean",
                           .min_seg_len = 2,
                           .country_col = NULL,
                           .time_col = NULL,
                           .indicator_col = NULL) {
  # Check if the anomaly package is installed
  if (!requireNamespace("anomaly", quietly = TRUE)) {
    stop("capa: The anomaly package is required for this function. Please install it.", call. = FALSE)
  }

  # Check columns exist
  if (!all(c(.country_col, .time_col, .indicator_col) %in% colnames(.data))) {
    stop("capa: One or more specified columns do not exist in the data frame.", call. = FALSE)
  }

  # Check if the .type argument is valid
  valid_types <- c("meanvar", "mean", "robustmean")
  if (!.type %in% valid_types) {
    stop(paste("capa: Invalid type specified. Choose from:", paste(valid_types, collapse = ", ")))
  }

  # Check for missing values, remove them and alert the user
  if (any(is.na(.data$Zscore))) {
    warning("capa: Missing values found in Zscore column. These will be removed from the analysis.", call. = FALSE)
    .data_sub <- .data |> fungroup() |> fsubset(!is.na(Zscore))
  } else {
    .data_sub <- .data
  }


  # Check if column names used during the function are already in the data, if so, change them and rename back later
  if (any(c("location", "start", "end", "start.lag", "end.lag", "variate", "strength") %in% colnames(.data_sub))) {
    warning("capa: Column names 'location', 'start', 'end', 'start.lag', 'end.lag', 'variate', or 'strength' found in the data frame. These will be temporarily renamed during the analysis.", call. = FALSE)
    # Identify which columns need to be renamed
    cols_to_rename <- c("location", "start", "end", "start.lag", "end.lag", "variate", "strength")
    existing_cols <- cols_to_rename[cols_to_rename %in% colnames(.data_sub)]
    # Rename the columns by adding "_orig" suffix
    for (col in existing_cols) {
      new_col_name <- paste0(col, "_orig")
      colnames(.data_sub)[colnames(.data_sub) == col] <- new_col_name
      }
  }

  # Save columns not included in the grouping variables
  .other_cols <- colnames(.data_sub)[!colnames(.data_sub) %in% c(.country_col, .time_col, .indicator_col, "Zscore")]

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
                            #tryCatch(anomaly::point_anomalies(x),
                            tryCatch(point_anomalies(x),
                                   error = function(e) {
                                     warning("capa - Error in detecting point anomalies: ", e$message, call. = FALSE)
                                     return(data.frame(location = NA, variate = NA, strength = NA))
                                   }))
  names(point_anomalies) <- GRPnames(.grouped_data, sep = "._.")
  point_anomalies <- unlist2d(point_anomalies, idcols = "Indicator")

  .idx_point_anomalies <- as.data.frame(str_split_fixed(point_anomalies$Indicator, "._.", n = length(c(.country_col, .indicator_col))))
  colnames(.idx_point_anomalies) <- c(.country_col, .indicator_col)
  point_anomalies <- cbind(.idx_point_anomalies, point_anomalies[, -1])  # Remove the Indicator column

  # Extract the collective anomalies
  collective_anomalies <- lapply(outliers, collective_anomalies)
  names(collective_anomalies) <- GRPnames(.grouped_data, sep = "._.")
  collective_anomalies <- unlist2d(collective_anomalies, idcols = "Indicator")

  .idx_collective_anomalies <- as.data.frame(str_split_fixed(collective_anomalies$Indicator, "._.", n = length(c(.country_col, .indicator_col))))
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
            type = ifelse(location >= start & location <= end, "collective", type),
            type = ifelse(is.na(type) & !is.na(capa_strength), "point", type )) |>
    fselect(-c(start, end, location, start.lag, end.lag, variate))

  # Include original columns excluded and reorder columns to have original columns first
  if (length(.other_cols) > 0) {
    .data <- .data |>
      join(.data_sub[, c(.country_col, .time_col, .indicator_col, .other_cols)], on = c(.country_col, .time_col, .indicator_col), verbose = 0)
  }
  .data <- .data[, colnames(.data_sub), drop = FALSE]

  # If column names were renamed, rename them back to original
  if (any(c("location_orig", "start_orig", "end_orig", "start_lag_orig", "end_lag_orig", "variate_orig", "strength_orig") %in% colnames(.data))) {
    # Rename back to original names, only for those that exist
    for (col in existing_cols) {
      new_col_name <- paste0(col, "_orig")
      colnames(.data)[colnames(.data) == new_col_name] <- col
    }

  }

  return(.data)
}


#' Summary method for maly_detect class
#'
#' @description This function provides a summary of the maly_detect class object.
#'
#' @param object An object of class maly_detect.
#' @param ... Additional arguments (not used).
#'
#' @export
#' @method summary maly_detect
#' @order 1
summary.maly_detect <- function(object, ...) {
  # Check if the object is of class maly_detect
  if (!inherits(object, "maly_detect")) {
    stop("object must be of class 'maly_detect'.", call. = FALSE)
  }

  # Get the method used for detection
  method <- attr(object, "maly_detect_attr")$method

  # Get the number of rows and columns in the data frame
  n_rows <- nrow(object)
  n_cols <- ncol(object)

  # Get the columns used for country, time, and indicator
  country_cols <- attr(object, "country_columns")
  time_col <- attr(object, "time_columns")
  indicator_col <- attr(object, "indicator_columns")


  if (length(method) > 1) {
    # Get the number of countries with outliers
    n_countries <- length(unique(object[object$outlier_indicator_total > 0, country_cols[1]]))
    unique_indicators <- unique(object[object$outlier_indicator_total > 0, indicator_col])
    n_indicators <- length(unique_indicators[!is.na(unique_indicators)])
    n_time_periods <- length(unique(object[object$outlier_indicator_total > 0, time_col]))

    # Get the number of outliers detected
    n_outliers <- sum(object$outlier_indicator_total, na.rm = TRUE)
  } else {
    # Get the number of countries with outliers
    n_countries <- length(unique(object[object$outlier_indicator == 1, country_cols[1]]))
    unique_indicators <- unique(object[object$outlier_indicator == 1, indicator_col])
    n_indicators <- length(unique_indicators[!is.na(unique_indicators)])
    n_time_periods <- length(unique(object[object$outlier_indicator == 1, time_col]))

    # Get the number of outliers detected
    n_outliers <- sum(object$outlier_indicator, na.rm = TRUE)
  }

  # Table of outliers per country
  if (n_countries > 0) {
    if(length(method) > 1) {
      outlier_table <- table(object[object$outlier_indicator_total > 0, country_cols[1]])
      # Extract the country with most outliers
      most_outliers_country <- names(which.max(outlier_table))
    } else {
      outlier_table <- table(object[object$outlier_indicator == 1, country_cols[1]])
      # Extract the country with most outliers
      most_outliers_country <- names(which.max(outlier_table))
    }
  }

  # If method is NULL, set it to "Unknown"
  if (is.null(method)) {
    method <- "Unknown"
  } else if (length(method) > 1) {
    method <- paste(method, collapse = ", ")
  }

  # Print summary information
  cat("Summary of Macroanomaly detect:\n")
  cat("  Method(s) used for detection:", method, "\n")
  cat("  Number of rows:", n_rows, "\n")
  cat("  Number of columns:", n_cols, "\n\n")

  cat(" Information of outliers: \n")

  cat("  Number of countries with outliers:", n_countries, "of a total of" ,
      length(unique(object[[country_cols[1]]])), "countries\n")
  cat("  Number of indicators with outliers:", n_indicators, "of a total of",
      length(unique(object[[indicator_col]])), "indicators\n")
  cat("  Number of time periods with outliers:", n_time_periods, "of a total of",
      length(unique(object[[time_col]])), "time periods\n")
  cat("  Number of outliers detected:", n_outliers, "\n")

  if (n_countries > 0) {
    cat("  Country with most outliers:", most_outliers_country, "\n")
    cat("    - Number of outliers in this country:", outlier_table[most_outliers_country], "\n")
  } else {
    cat("No outliers detected.\n")
  }

}


#' Plot method for maly_detect class
#'
#' @description This function provides a plot of the outliers detected in the maly_detect class object.
#'
#' @param x An object of class maly_detect.
#' @param country A character vector specifying the country to plot.
#' Default is NULL, which means first country in the data will be used.
#' @param indicator A string specifying the indicator to plot. Only one per graph.
#' Default is NULL, which means the first indicator in the data will be used.
#' @param .total_threshold A numeric value specifying the threshold for the total outlier indicator when using
#' multiple methods. Default is \code{M-1}, where \code{M} is total number of methods; which means that an
#' outlier will be highlighted if it detected by all methods.
#' @param x.lab A string specifying the label for the x-axis.
#' Default is NULL, which means the time column will be used.
#' @param y.lab A string specifying the label for the y-axis of the original value (top graph).
#' Default is NULL, which means the value column will be used.
#' @param ... Additional arguments to be passed to the plot function.
#'
#' @return A ggplot object showing the outliers detected in the specified country and indicator.
#'
#' @importFrom ggplot2 ggplot aes guides geom_line geom_point labs theme element_text facet_wrap scale_color_manual scale_shape_manual geom_ribbon scale_fill_manual geom_vline scale_alpha_manual
#' @importFrom rlang sym
#' @importFrom collapse fsubset fgroup_by fmutate fselect ftransformv
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom lubridate NA_Date_
#'
#' @export
#' @method plot maly_detect
plot.maly_detect <- function(x, country = NULL, indicator = NULL, .total_threshold = NULL, x.lab = NULL, y.lab = NULL, ...) {
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
    stop(paste("No data found for country:", country, "and indicator:", indicator), call. = FALSE)
  }

  # Check if the time column exists
  if (!attr(x, "time_columns")[1] %in% colnames(.data)) {
    stop(paste("The time column", attr(x, "time_columns")[1], "does not exist in the data."), call. = FALSE)
  }

  # Check if the frequency is yearly or not
  if (attr(x, "maly_norm_attrs")$frequency == "yearly") {
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
    stop(paste("The value column", attr(x, "value_column"), "does not exist in the data."), call. = FALSE)
  }

  # Check if the Zscore column exists
  if (!"Zscore" %in% colnames(.data)) {
    stop("The Zscore column does not exist in the data. Consider running the normalize function first.", call. = FALSE)
  }

  # Relabel Imputed column if it exists and obtain the total number of imputed values
  if ("Imputed" %in% colnames(.data)) {
    .data$Imputed <- factor(.data$Imputed, levels = c(TRUE, FALSE), labels = c("Imputed", "Not Imputed"))
    n_imputed <- sum(.data$Imputed == "Imputed", na.rm = TRUE)
  }

  # Check if .total_threshold is specified, if not, set it to M-1
  if (is.null(.total_threshold)) {
    .total_threshold <- length(attr(x, "maly_detect_attr")$method) - 1
  }

  # Check if multiple methods were used
  if (length(attr(x, "maly_detect_attr")$method) > 1) {
    .detection_col <- "outlier_indicator_total"
    .data[[.detection_col]] <- ifelse(.data[[.detection_col]] >= .total_threshold, "Outlier", "Not Outlier")
    .data[[.detection_col]] <- factor(.data[[.detection_col]], levels = c("Outlier", "Not Outlier"))

    # Summarise by country and indicator the time periods with outliers
    .data_grouped <- .data[.data[[.detection_col]] == "Outlier",] |>
      fgroup_by(c(attr(x, "country_columns")[1], attr(x, "indicator_columns")[1])) |>
      fungroup()

    # If no outliers, warn the user
    if (nrow(.data_grouped) == 0) {
      warning(paste("No outliers detected for country:", country, "and indicator:", indicator), call. = FALSE)
    }

  } else {
    .detection_col <- "outlier_indicator"
    .data[[.detection_col]] <- ifelse(.data[[.detection_col]] > 0, "Outlier", "Not Outlier")
    .data[[.detection_col]] <- factor(.data[[.detection_col]], levels = c("Outlier", "Not Outlier"))

    .data_grouped <- .data[.data[[.detection_col]] == "Outlier",] |>
      fgroup_by(c(attr(x, "country_columns")[1], attr(x, "indicator_columns")[1])) |>
      fungroup()

    # If no outliers, warn the user
    if (nrow(.data_grouped) == 0) {
      warning(paste("No outliers detected for country:", country, "and indicator:", indicator), call. = FALSE)
    }

  }

  # Check if x.lab and y.lab are specified, if not, use the time and value columns
  if (is.null(x.lab)) {
    x.lab <- attr(x, "time_columns")[1]
  }

  if (is.null(y.lab)) {
    # y.lab <- attr(x, "value_column")
    y.lab.top <- "Obs. value"
    y.lab.bottom <-  "Z-score"
  } else {
    y.lab.top <- y.lab
    y.lab.bottom <- "Z-score"
 }

  # Create the plot based on imputation status
  if (n_imputed == 0) {
    if (nrow(.data_grouped) != 0){
      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
        geom_line(alpha = 0.3) +
        geom_point(aes(alpha = .data[[.detection_col]])) +
        # geom_vline(data = .data_grouped, aes(xintercept = .data[[attr(x, "time_columns")[1]]]), color = "blue", linetype = "dashed") +
        scale_alpha_manual(values = c("Outlier" = 1, "Not Outlier" = 0.2)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        guides(alpha = "none") +
        labs(title = paste0(country, " - ", indicator, " - Observation values and Z-scores"),
             subtitle = indicator,
             alpha = "Outlier Indicator",
             x = x.lab,
             y = y.lab.top) -> original_plot

      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
        geom_point(aes(alpha = .data[[.detection_col]])) +
        # geom_vline(data = .data_grouped, aes(xintercept = .data[[attr(x, "time_columns")[1]]]), color = "blue", linetype = "dashed") +
        scale_alpha_manual(values = c("Outlier" = 1, "Not Outlier" = 0.2)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(alpha = "Outlier Indicator",
             x = x.lab,
             y = y.lab.bottom) -> zscore_plot

      # Combine the two plots
      combined_plot <- original_plot / zscore_plot +
        plot_layout(guides = "collect") & theme(axis.text = element_text(size = 8),
                                                legend.position = "bottom",
                                                legend.direction = "horizontal")

      combined_plot <- combined_plot + plot_annotation(
        caption =  "No imputed values in series and outliers detected"
      )

    } else {
      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
        geom_line(alpha = 0.3) +
        geom_point() +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        guides(alpha = "none") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(title = paste0(country, " - ", indicator, " - Observation values and Z-scores"),
             subtitle = indicator,
             x = x.lab,
             y = y.lab.top) -> original_plot

      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
        geom_point() +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(x = x.lab,
             y = y.lab.bottom) -> zscore_plot

      # Combine the two plots
      combined_plot <- original_plot / zscore_plot +
        plot_layout(guides = "collect") & theme(axis.text = element_text(size = 8),
                                                legend.position = "bottom",
                                                legend.direction = "horizontal")

      combined_plot <- combined_plot + plot_annotation(
        caption =  "No imputed values in the series and no outliers detected"
      )

    }
  } else {
    if (nrow(.data_grouped) != 0){
      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
        geom_line(alpha = 0.3) +
        geom_point(aes(color = Imputed, alpha = .data[[.detection_col]]), show.legend=TRUE) +
        # geom_vline(data = .data_grouped, aes(xintercept = .data[[attr(x, "time_columns")[1]]]), color = "blue", linetype = "dashed") +
        scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
        # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
        scale_alpha_manual(values = c("Outlier" = 1, "Not Outlier" = 0.2)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        guides(alpha = "none") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(title = paste0(country, " - ", indicator, " - Observation values and Z-scores"),
             subtitle = indicator,
             alpha = "Outlier Indicator",
             color = "Imputation Status",
             x = x.lab,
             y = y.lab.top) -> original_plot

      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
        geom_point(aes(color = Imputed, alpha = .data[[.detection_col]]), show.legend=TRUE) +
        # geom_vline(data = .data_grouped, aes(xintercept = .data[[attr(x, "time_columns")[1]]]), color = "blue", linetype = "dashed") +
        scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
        # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
        scale_alpha_manual(values = c("Outlier" = 1, "Not Outlier" = 0.2)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        labs(alpha = "Outlier Indicator",
             color = "Imputation Status",
             x = x.lab,
             y = y.lab.bottom) -> zscore_plot

      # Combine the two plots
      combined_plot <- original_plot / zscore_plot +
        plot_layout(guides = "collect") & theme(axis.text = element_text(size = 8),
                                                legend.position = "bottom",
                                                legend.direction = "horizontal")

      combined_plot <- combined_plot + plot_annotation(
        caption = "Imputed values in the series and outliers detected"
      )
    } else {
      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = .data[[attr(x, "value_column")]])) +
        geom_line(alpha = 0.3) +
        geom_point(aes(color = Imputed), show.legend=TRUE) +
        scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
        # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        guides(alpha = "none") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(title = paste0(country, " - ", indicator, " - Observation values and Z-scores"),
             subtitle = indicator,
             color = "Imputation Status",
             x = x.lab,
             y = y.lab.top) -> original_plot

      ggplot(.data, aes(x = .data[[attr(x, "time_columns")[1]]], y = Zscore)) +
        geom_point(aes(color = Imputed), show.legend=TRUE) +
        scale_color_manual(values = c("Imputed" = "red", "Not Imputed" = "black"), drop = FALSE) +
        # scale_shape_manual(values = c("Imputed" = 1, "Not Imputed" = 16)) +
        facet_wrap(~ .data[[attr(x, "country_columns")[1]]], scales = "free_y") +
        theme(axis.text = element_text(size = 8), legend.position = "bottom") +
        labs(color = "Imputation Status",
             x = x.lab,
             y = y.lab.bottom) -> zscore_plot

      # Combine the two plots
      combined_plot <- original_plot / zscore_plot +
        plot_layout(guides = "collect") & theme(axis.text = element_text(size = 8),
                                                legend.position = "bottom",
                                                legend.direction = "horizontal")

      combined_plot <- combined_plot + plot_annotation(
        caption = "Imputed values in the series but no outliers detected"
      )
    }
  }

  return(combined_plot)
}


#' Function to save the maly_detect object to a file
#'
#' @description This function saves the maly_detect object to a file in csv format.
#'
#' @param x An object of class maly_detect.
#' @param file A character string specifying the file path where the object should be saved.
#' @param top_outliers A integer value indicating whether to save only the top outliers.
#' #' Default is NULL, which saves all data. If only one method specified, it will use the Zscore
#' to order the data based on the absolute value of the Zscore. If multiple methods were used,
#' it will use the \code{outlier_indicator_total} column to order the data.
#' @param additional_cols A logical value indicating whether to save additional columns in the data frame.
#' These columns include the original columns, the additional columns from normalize and
#' the outlier indicator columns.
#' @param ... Additional arguments to be passed to the write.csv function.
#'
#' @return NULL
#' @export
write_to_csv <- function(x, file, top_outliers = NULL, additional_cols = FALSE, ...) {
  # Check if the object is of class maly_detect
  if (!inherits(x, "maly_detect")) {
    stop("x must be of class 'maly_detect'.")
  }

  # Check if the file path is provided
  if (missing(file)) {
    stop("Please provide a file path to save the maly_detect object.", call. = FALSE)
  }

  # Check if outliers is a numeric value if not null and subset the data
  if (!is.numeric(top_outliers) && !is.null(top_outliers)) {
    stop("top_outliers must be a numeric value or NULL.", call. = FALSE)
  } else if (!is.null(top_outliers)) {
    # Subset the data to keep only the top outliers
    if (length(attr(x, "maly_detect_attr")$method) > 1) {
      # If multiple methods were used, use the outlier_indicator_total column
      x <- x[order(-x$outlier_indicator_total), ]
    } else {
      # If only one method was used, use the outlier_indicator column
      x <- x[order(-abs(x$Zscore)), ]
    }
    x <- x[1:top_outliers, ]
  }

  # If additional_cols is FALSE, select only the original columns and the outlier indicator columns
  if (!additional_cols) {
    # Get the original columns names
    .columns <- attr(x, "country_columns")
    .columns <- c(.columns, attr(x, "time_columns"))
    .columns <- c(.columns, attr(x, "indicator_columns"))
    .columns <- c(.columns, attr(x, "value_column"))

    # If multiple methods were used, keep the outlier_indicator_total column
    if (length(attr(x, "maly_detect_attr")$method) > 1) {
      .columns <- c(.columns, "outlier_indicator_total")
    } else {
      .columns <- c(.columns, "outlier_indicator")
    }

    # If Imputed column exists, add it to the columns
    if ("Imputed" %in% colnames(x)) {
      if (!"Imputed" %in% .columns) {
        .columns <- c(.columns, "Imputed")
      }
    }
    # Select only the original columns and the outlier indicator columns
    x <- x[, .columns]
  }

  # Write the data frame to a csv file
  write.csv(x, file = file, row.names = FALSE, ...)

  # Return NULL
  return(NULL)
}
