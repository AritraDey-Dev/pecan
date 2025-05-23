% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hop_test.R
\name{hop_test}
\alias{hop_test}
\title{hop_test}
\usage{
hop_test(settings, ens.runid = NULL, nyear)
}
\arguments{
\item{settings}{SDA PEcAn settings object}

\item{ens.runid}{run id. If not provided, is looked up from [settings$outdir]/runs.txt}

\item{nyear}{number of years to run hop test over}
}
\value{
NONE
}
\description{
Hop test. This script tests that the model successfully reads it's own restart and can restart without loss of information.
}
\author{
Ann Raiho \email{araiho@nd.edu}
}
