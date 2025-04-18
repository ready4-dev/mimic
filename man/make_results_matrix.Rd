% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_results_matrix}
\alias{make_results_matrix}
\title{Make results matrix}
\usage{
make_results_matrix(
  data_tb,
  names_chr,
  arms_1L_chr = "Data",
  var_1L_chr = "Cost"
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{names_chr}{Names (a character vector)}

\item{arms_1L_chr}{Arms (a character vector of length one), Default: 'Data'}

\item{var_1L_chr}{Variable (a character vector of length one), Default: 'Cost'}
}
\value{
Results (a matrix)
}
\description{
make_results_matrix() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make results matrix. The function returns Results (a matrix).
}
\keyword{internal}
