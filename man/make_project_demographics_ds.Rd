% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_project_demographics_ds}
\alias{make_project_demographics_ds}
\title{Make project demographics dataset}
\usage{
make_project_demographics_ds(raw_data_ls, uid_1L_chr = "UID")
}
\arguments{
\item{raw_data_ls}{Raw data (a list)}

\item{uid_1L_chr}{Unique identifier (a character vector of length one), Default: 'UID'}
}
\value{
Demographics (a tibble)
}
\description{
make_project_demographics_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project demographics dataset. The function returns Demographics (a tibble).
}
\keyword{internal}
