% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregateResult.R
\name{parAggregateMCall}
\alias{parAggregateMCall}
\title{Creation of Mc_all (only antares > V6)}
\usage{
parAggregateMCall(opts, nbcl = 8, verbose = 1)
}
\arguments{
\item{opts}{\code{list} of simulation parameters returned by the function \link{setSimulationPath}}

\item{nbcl}{\code{numeric} Number of parralel process}

\item{verbose}{\code{numeric} see logs (1) or not (0)}
}
\description{
Creation of Mc_all (only antares > V6)
}
\examples{
\dontrun{
   parAggregateMCall(opts)

}

}
