% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_project_results}
\alias{make_project_results}
\title{Make project results}
\usage{
make_project_results(
  X_Ready4useDyad,
  inputs_ls,
  min_cell_size_1L_int = 30L,
  modifiable_chr = character(0),
  outcomes_chr = character(0),
  threshold_1L_dbl = 96000
)
}
\arguments{
\item{X_Ready4useDyad}{PARAM_DESCRIPTION}

\item{inputs_ls}{Inputs (a list)}

\item{min_cell_size_1L_int}{Minimum cell size (an integer vector of length one), Default: 30}

\item{modifiable_chr}{Modifiable (a character vector), Default: character(0)}

\item{outcomes_chr}{Outcomes (a character vector), Default: character(0)}

\item{threshold_1L_dbl}{Threshold (a double vector of length one), Default: 96000}
}
\value{
Sim results (a list)
}
\description{
make_project_results() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project results. The function returns Sim results (a list).
}
\keyword{internal}
