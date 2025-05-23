% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_project_outcomes_data}
\alias{add_project_outcomes_data}
\title{Add project outcomes data}
\usage{
add_project_outcomes_data(model_data_ls, processed_ls, mdls_lup = NULL)
}
\arguments{
\item{model_data_ls}{Model data (a list)}

\item{processed_ls}{Processed (a list)}

\item{mdls_lup}{Models (a lookup table), Default: NULL}
}
\value{
Model data (a list)
}
\description{
add_project_outcomes_data() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add project outcomes data. The function returns Model data (a list).
}
\keyword{internal}
