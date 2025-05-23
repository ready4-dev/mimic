% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_project_RDS}
\alias{write_project_RDS}
\title{Write projectDS}
\usage{
write_project_RDS(
  data_ls,
  path_to_private_1L_chr,
  processed_dir_1L_chr,
  divider_1L_chr = "\\\\"
)
}
\arguments{
\item{data_ls}{Data (a list)}

\item{path_to_private_1L_chr}{Path to private (a character vector of length one)}

\item{processed_dir_1L_chr}{Processed directory (a character vector of length one)}

\item{divider_1L_chr}{Divider (a character vector of length one), Default: '\'}
}
\value{
No return value, called for side effects.
}
\description{
write_project_RDS() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write projectds. The function is called for its side effects and does not return a value.
}
\keyword{internal}
