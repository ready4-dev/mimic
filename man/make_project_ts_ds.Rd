% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_project_ts_ds}
\alias{make_project_ts_ds}
\title{Make project time series dataset}
\usage{
make_project_ts_ds(
  X_Ready4useDyad,
  processed_ls,
  index_1L_chr = "Date",
  key_vars_chr = make_project_keys(type_1L_chr = "ts")
)
}
\arguments{
\item{X_Ready4useDyad}{PARAM_DESCRIPTION}

\item{processed_ls}{Processed (a list)}

\item{index_1L_chr}{Index (a character vector of length one), Default: 'Date'}

\item{key_vars_chr}{Key variables (a character vector), Default: make_project_keys(type_1L_chr = "ts")}
}
\value{
Y (A dataset and data dictionary pair.)
}
\description{
make_project_ts_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project time series dataset. The function is called for its side effects and does not return a value.
}
\keyword{internal}
