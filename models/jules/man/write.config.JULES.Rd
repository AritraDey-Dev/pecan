% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.config.JULES.R
\name{write.config.JULES}
\alias{write.config.JULES}
\title{Writes a JULES config file.}
\usage{
write.config.JULES(defaults, trait.values, settings, run.id)
}
\arguments{
\item{defaults}{list of defaults to process}

\item{trait.values}{vector of samples for a given trait}

\item{settings}{list of settings from pecan settings file}

\item{run.id}{id of run}
}
\value{
configuration file for JULES for given run
}
\description{
Requires a pft xml object, a list of trait values for a single model run,
and the name of the file to create
}
\examples{
\dontrun{
  write.config.JULES(defaults, trait.values, settings, run.id)
}
}
\author{
Mike Dietze, Rob Kooper
}
