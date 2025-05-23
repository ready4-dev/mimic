% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_unit_cost_detail}
\alias{get_unit_cost_detail}
\title{Get unit cost detail}
\usage{
get_unit_cost_detail(
  unit_costs_tb,
  what_1L_chr = c("scenarios", "fixed", "names", "variable")
)
}
\arguments{
\item{unit_costs_tb}{Unit costs (a tibble)}

\item{what_1L_chr}{What (a character vector of length one), Default: c("scenarios", "fixed", "names", "variable")}
}
\value{
Detail (an output object of multiple potential types)
}
\description{
get_unit_cost_detail() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get unit cost detail. The function returns Detail (an output object of multiple potential types).
}
\keyword{internal}
