% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detection.R
\name{tsoutliers_detection}
\alias{tsoutliers_detection}
\title{Function to detect outliers using the IQR method (tsoutliers from \code{forecast})}
\usage{
tsoutliers_detection(.data, .threshold = 3)
}
\arguments{
\item{.data}{A data frame containing the dataset to be analyzed.}

\item{.threshold}{A numeric value specifying the IQR threshold for outlier detection. Default is 3.}
}
\value{
A data frame containing the original dataset with an additional column indicating the outlier status
}
\description{
This function detects outliers in a dataset using the IQR method. It follows
the approach described in the \code{forecast} package. An outlier is defined as the remainder
of the series, after removing the trend and seasonal components, that is greater than 3 times the IQR
or less than -3 times the IQR.
}
