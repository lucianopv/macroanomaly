% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detection.R
\name{isotree_detection}
\alias{isotree_detection}
\title{Function to apply the isotree algorithm}
\usage{
isotree_detection(
  .data,
  .cols = NULL,
  .ndim = 1,
  .ntrees = 10,
  .nthreads = 2,
  .sample_size = 1,
  .threshold = 0.5,
  ...
)
}
\arguments{
\item{.data}{A data frame containing the dataset to be analyzed.}

\item{.cols}{A character vector specifying the columns to be used for outlier detection. Default is NULL, which uses all columns.}

\item{.ndim}{A numeric value specifying the number of dimensions for the isotree algorithm. Default is 1.}

\item{.ntrees}{A numeric value specifying the number of trees to be used in the isotree algorithm. Default is 10.}

\item{.nthreads}{A numeric value specifying the number of threads to be used in the isotree algorithm. Default is 2.}

\item{.sample_size}{A numeric value specifying the sample size for the isotree algorithm. Default is 1, which means the entire dataset is used.}

\item{.threshold}{A numeric value specifying the threshold for outlier detection. Default is 0.5.}

\item{...}{Additional arguments to be passed to the isotree algorithm.}
}
\value{
A data frame containing the original dataset with an additional column indicating the outlier score and indicator
based on the specified threshold.
}
\description{
This function applies the isotree algorithm to a given dataset for outlier detection.
}
