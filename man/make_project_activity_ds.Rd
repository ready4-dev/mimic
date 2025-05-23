% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_project_activity_ds}
\alias{make_project_activity_ds}
\title{Make project activity dataset}
\usage{
make_project_activity_ds(raw_data_ls, type_1L_chr = c("initial", "wip"))
}
\arguments{
\item{raw_data_ls}{Raw data (a list)}

\item{type_1L_chr}{Type (a character vector of length one), Default: c("initial", "wip")}
}
\value{
Activity (a tibble)
}
\description{
make_project_activity_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make project activity dataset. The function returns Activity (a tibble).
}
\keyword{internal}
