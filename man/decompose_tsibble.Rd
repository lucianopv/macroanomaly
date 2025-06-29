% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{decompose_tsibble}
\alias{decompose_tsibble}
\title{Season and trend decomposition using the feasts package}
\usage{
decompose_tsibble(
  .data,
  .value_col,
  .season = NULL,
  .trend = TRUE,
  .frequency = "yearly",
  .imputed = NULL
)
}
\arguments{
\item{.data}{A tsibble object containing the data to be decomposed.}

\item{.value_col}{A character vector specifying the name of the value column.}

\item{.season}{A character vector specifying the seasonality to be used. Default is NULL.}

\item{.trend}{A boolean value indicating whether to include the trend component. Default is TRUE.}

\item{.frequency}{A character vector specifying the frequency of the time series.}

\item{.imputed}{A boolean value indicating whether values were imputed. Default is NULL.
Default is "yearly". The other values are "monthly" and "quarterly".}
}
\value{
A tsibble object with the decomposed data.
}
\description{
This function performs seasonal and trend decomposition using the feasts package.
}
