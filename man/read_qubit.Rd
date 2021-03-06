% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{read_qubit}
\alias{read_qubit}
\title{Parses the output of the Qubit fluorometer}
\usage{
read_qubit(path, volume_used = NULL)
}
\arguments{
\item{path}{(\code{character} of length 1) The file path to the output file of Qubit measurments.}

\item{volume_used}{(\code{numeric} of length 1) The volume of sample added during dilution.}
}
\description{
Parses the output of the Qubit fluorometer into a dataframe. Since the Qubit stores data in the oppisite
order that the tubes are inserted, the rows order will be reversed (i.e. the first row will be the first
measured).
NOTE: if a sample is too low and has a < symbol, this function does not handle it correctly yet!
}
\keyword{Qubit}
\keyword{qubit}
