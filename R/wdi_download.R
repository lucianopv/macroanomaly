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
#'@importFrom collapse fmutate fselect allv pivot
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
#'
#' @importFrom utils download.file unzip read.csv write.csv
#'
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
