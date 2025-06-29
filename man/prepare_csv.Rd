% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wdi_download.R
\name{prepare_csv}
\alias{prepare_csv}
\title{Prepare format using the csv file downloaded from WorldBank}
\usage{
prepare_csv(.data)
}
\arguments{
\item{.data}{The data frame to be prepared obtained directly from the CSV file
of the World Bank.}
}
\value{
A data frame in a tidy format with the following columns:
\itemize{
\item Country.Name
\item Country.Code
\item Year
\item Indicator Code as columns ...
}
}
\description{
Prepare format using the csv file downloaded from WorldBank
}
