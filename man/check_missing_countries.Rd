% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{check_missing_countries}
\alias{check_missing_countries}
\title{Function to detect countries without data enough to produce imputation}
\usage{
check_missing_countries(
  .data,
  .value_col,
  .country_col,
  .time_col,
  .indicator_col,
  .min_obs = 2
)
}
\arguments{
\item{.data}{A tsibble object containing the data to be checked.}

\item{.value_col}{A character vector specifying the name of the value column.}

\item{.country_col}{A character vector specifying the name(s) of the country
identifiers.}

\item{.time_col}{A character vector specifying the name of the time
identifiers.}

\item{.indicator_col}{A character vector specifying the name(s) of the indicator
identifiers.}

\item{.min_obs}{A numeric value specifying the minimum number of observations
required for imputation. Default is 2.}
}
\value{
A data frame with the countries that do not have enough data for imputation.
}
\description{
This function detects countries without enough data to produce imputation.
}
