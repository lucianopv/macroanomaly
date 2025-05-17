#' Prepare format using the csv file downloaded from WorldBank
#'
#' @param .data The data frame to be prepared obtained directly from the CSV file
#' of the World Bank.
#' @returns A data frame in a tidy format with the following columns:
#' \itemize{
#' \item Country.Name
#' \item Country.Code
#' \item Year
#' \item Indicator Code as columns ...
#'}
#'
prepare_csv <- function(.data){
  .data |>
    pivot(
      ids = c("Country.Name", "Country.Code", "Indicator.Code", "Indicator.Name"),
      names = list(variable = "Year")
    ) |>
    fmutate(
      Length = nchar(as.character(Year))
    ) -> .data

  if(allv(.data$Length, 5)){
    .data |>
      fmutate(
        Year = as.integer(substr(Year, 2, 6))
      ) |>
      fselect(-Length, -Indicator.Name) -> .data
  } else {
    warning("`Year` column is not in the expected format (e.g., X1980)")
  }

  .data |>
    pivot(
      how = "wider",
      ids = c("Country.Name", "Country.Code", "Year"),
      values = c("value"),
      names = "Indicator.Code"
    ) -> .data

  return(.data)
}

#' Function to download and prepare CSV file from the World Bank
#'
#' @param .path The path where the CSV file will be downloaded. Default is the temporary directory.
#' @param .additional A logical value indicating whether to download additional files.
#' The additional files contain the information about the countries and series. Default is FALSE.
#' \itemize{
#' \item Series information
#' \item Series by year information
#' \item Country information
#' \item Country by series information
#' }
#' @param .prepare A logical value indicating whether to prepare the data in wide format
#' with the following columns:
#' \itemize{
#' \item Country.Name
#' \item Country.Code
#' \item Year
#' \item Indicator Code as columns ...
#' }
#' @returns Returns either a data frame in a tidy format (\code{.prepare = TRUE}),
#' a data frame with the data directory (\code{.prepare = FALSE}), or
#' a list with the data (either tidy or not) and additional data.frame with the
#' information provided in \code{.additional = TRUE}.
#' @export
wdi_download <- function(.path = tempdir(), .prepare = TRUE, .additional = FALSE){
  # Url to download the CSV files
  url <- "https://databank.worldbank.org/data/download/WDI_CSV.zip"

  # Name of file with the data
  .wdi_csv <- "WDICSV.csv"
  .wdi_zip <- "wdi.zip"

  # Name of additional files:
  wdi_additional <- c("Series" = "WDIseries.csv",
                      "Series-time" = "WDIseries-time.csv",
                      "Country" = "WDIcountry.csv",
                      "Country-series" = "WDIcountry-series.csv")

  # Check if the file already exists
  .path_csv <- file.path(.path, .wdi_csv)
  if(!file.exists(.path_csv)) {
    print("Checking if file exists and downloading...")
    download.file(url, file.path(.path, .wdi_zip) , mode = "wb")
    unzip(file.path(.path, .wdi_zip), exdir = .path)
    ## unlink(file.path(.path, .wdi_zip))
  } else {
    print("File already exists. No need to download.")
  }

  .data <- read.csv(.path_csv, stringsAsFactors = FALSE)

  # Check if data wants to be prepared
  if (.prepare) {
    # Check if the data is in the expected format
    if (all(c("Country.Name", "Country.Code", "Indicator.Code", "Indicator.Name") %in% colnames(.data))) {
      .data <- prepare_csv(.data)
    } else {
      warning("The data is not in the expected format. Please check the CSV file.")
    }
  }


  if (.additional){
    # create empty list for results
    .res <- list("Data" = .data)

    # Check and the files exist and read them
    for (i in seq_along(wdi_additional)) {
      if (!file.exists(file.path(.path, wdi_additional[i]))) {
        warning("File ", wdi_additional[i], " not found. Downloading again...")
        tryCatch({
          download.file(url, file.path(.path, .wdi_zip), mode = "wb")
        }, error = function(e) {
          warning("Error downloading file: ", e$message)
        })
        tryCatch({
          unzip(file.path(.path, .wdi_zip), files = wdi_additional[i])
        }, error = function(e) {
          warning("Error unzipping file: ", e$message)
        })
        unlink(file.path(.path, .wdi_zip))
      }
      tryCatch({
        .data_tmp <- read.csv(file.path(.path, wdi_additional[i]), stringsAsFactors = FALSE)
      }, error = function(e) {
        warning("Error reading file ", wdi_additional[i], ": ", e$message)
        warning("Try to download file manually or avoid including additional files")
      })

      # Include additional files in the list
      .res[[names(wdi_additional)[i]]] <- .data_tmp
    }
    return(.res)
  } else (
    return(.data)
  )
}

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
#' @param .year_col A character vector specifying the name(s) of the time
#' identifiers.
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
                               .country_col = c("Country.Code", "Country.Name"),
                               .year_col = "Year",
                               .indicator_col = "Indicator.Code",
                               .frequency = "yearly",
                               .long_format = TRUE) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .year_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

  # Check if only one indicator
  if (unique(.data[[.indicator_col]]) > 1) {
    .keys <- c(.country_col, .indicator_col)
  } else {
    .keys <- .country_col
  }

  # Convert to tsibble
  if (.frequency %in% c("yearly", "year")) {
    .data <- as_tsibble(.data, key = .keys, index = .year_col)
  } else if (.frequency %in% c("quarterly", "quarter")) {
    .data |>
      fmutate(across(.year_col, tsibble::yearquarter)) |>
      tsibble::as_tsibble(key = .keys, index = .year_col) -> .data
  } else if (.frequency %in% c("monthly", "month")) {
    .data |>
      fmutate(across(.year_col, tsibble::yearmonth)) |>
      tsibble::as_tsibble(key = .keys, index = .year_col) -> .data
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
#' @param .year_col A character vector specifying the name(s) of the time
#' identifiers.
#' @param ... Additional arguments to be passed to the imputation function.
#' @return A tsibble object with the imputed data and a column identifying the imputed values.
#'
#' @importFrom imputeTS na_interpolation na_kalman na_mean na_locf na_ma na_seadec na_random
#' @importFrom tsibble as_tsibble group_by_key
#' @importFrom collapse fmutate fselect
#' @export
impute_missing <- function(.data,
                           .value_col,
                           .method = "na_interpolation",
                           .country_col = c("Country.Code", "Country.Name"),
                           .year_col = "Year",
                           .indicator_col = "Indicator.Code",
                           ...) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .year_col, .indicator_col) %in% colnames(.data))) {
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
                              .year_col = .year_col,
                              .indicator_col = .indicator_col,
                              .min_obs = .min_obs)

  if (nrow(.missing_data) > 0) {
    stop(paste("The following countries do not have enough data (i.e.,", .min_obs, " data point) for imputation:\n"),
            paste(.missing_data[[.country_col[1]]], collapse = ", "), ".\n Consider excluding these countries.")
  }

  # Impute missing values using the specified method
  .data <- .data |>
    tsibble::group_by_key() |>
    fmutate(
      Imputed = ifelse(is.na(x), TRUE, FALSE),
      across(.value_col,
                   \(x) get(.method)(x, ...)))

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
#' @param .year_col A character vector specifying the name of the time
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
                              .country_col = c("Country.Code", "Country.Name"),
                              .year_col = "Year",
                              .indicator_col = "Indicator.Code",
                              .min_obs = 2) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .year_col) %in% colnames(.data))) {
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
#' @param .cols A character vector of column names to be normalized and detrended,
#' can be integers refering to indexes of the columns (e.g., 1:10), or the names
#' of the columns (e.g., c("AG.CON.FERT.PT.ZS", "DC.DAC.AUTL.CD")).
#' @param .country_col A character vector specifying the name(s) of the country
#' identifiers
#' @param .year_col A character vector specifying the name of the year column.
#' @param .detrend A logical value indicating whether to detrend the data. The
#' \code{supsmu} function is used for smoothing. Default is TRUE.
#' @param .long_format A logical value indicating whether to return the data in long format.
#' Default is TRUE.
#' @return A data frame with the normalized and detrended data.
#'
#'
#' @importFrom collapse fscale fsd fmutate fselect BY pivot
#' @export
normalize <- function(.data,
                          .cols,
                          .country_col = c("Country.Code", "Country.Name"),
                          .year_col = "Year",
                          .detrend = TRUE,
                          .long_format = TRUE) {
  # Check if the required columns exist in the data
  if (!all(c(.country_col, .year_col) %in% colnames(.data))) {
    stop("The specified columns do not exist in the data.")
  }

    # Check the input of columns, if numbers obtain columns and check if they exist
  if(is.numeric(.cols)) {
    if (all(.cols %in% seq_len(ncol(.data)))) {
      .cols <- colnames(.data)[.cols]
    } else {
      stop("The specified columns do not exist in the data.")
    }
  } else if (all(.cols %in% colnames(.data))) {
    # Do nothing, .cols is already in the correct format
  } else {
    stop("The specified columns do not exist in the data.")
  }

  # Check that the column is a numeric column
  if (!all(sapply(.data[, .cols], is.numeric))) {
    stop("All specified columns must be numeric.")
  }

  if(length(.cols) == 0) {
    stop("No columns specified for normalization.")
  }

  if (length(.year_col) != 1) {
    stop("Only one country and year column can be specified.")
  }

  # Identifiers for country and year
  .ids_names <- c(.country_col, .year_col)
  .ids <- .data[, .ids_names]

  # Year column
  .year_data <- as.numeric(.data[[.year_col]])

  # Subset data
  .data <- .data[, c(.cols)]

  # Normalize the data
  .data_norm <- fscale(.data, g = .ids[,.country_col])
  .data_norm_sd <- fsd(.data, g = .ids[,.country_col], TRA = 1)
  .data_norm <- replace(.data_norm, .data_norm_sd<1e-13, NA)

  # Check if detrend is TRUE
  if (!is.logical(.detrend)) {
    stop("The detrend argument must be a logical value (TRUE or FALSE).")
  }

  # Apply detrend if specified
  if (.detrend) {
    # Identify first year
    first_year <- min(.year_data, na.rm = TRUE) - 1

    # Create a new column for time
    .time_data <- as.integer(.year_data) - first_year

    # Detrend the data using the detrend function
    .data <- cbind(.ids, BY(x = .data_norm, detrend, g = .ids[,.country_col], .year = .time_data))
  } else {
    # If not detrending, just bind the ids
    .data <- cbind(.ids, .data_norm)
  }

  # Check if long format is TRUE
  if (.long_format){
    # Convert to long format
    .data <- pivot(
      .data,
      how = "longer",
      ids = c(.country_col, .year_col),
      values = c(.cols),
      names = list(variable = "Indicator.Code", value =  "Zscore")
    )
  }

  return(.data)
}
