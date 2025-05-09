% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_edr.R
\name{setup_edr}
\alias{setup_edr}
\title{Setup EDR run}
\usage{
setup_edr(
  ed2in,
  output_dir,
  datetime = ISOdatetime(ed2in[["IYEARA"]], ed2in[["IMONTHA"]], ed2in[["IDATEA"]], 12, 0,
    0, tz = "UTC"),
  ...
)
}
\arguments{
\item{ed2in}{ED2IN list object (see \link[PEcAn.ED2:read_ed2in]{PEcAn.ED2::read_ed2in}).}

\item{output_dir}{Directory in which run files will be stored}

\item{datetime}{Date time object (or compliant string) at which to run EDR.
Defaults to 12 noon on start date in ED2IN.}

\item{...}{Additional arguments passed on to \code{PEcAn.ED2::modify_ed2in}}
}
\value{
Path to EDR-configured ED2IN file.
}
\description{
Using an existing ED2IN file as a template, create a new ED2IN and history
file configured for running EDR.
}
\author{
Alexey Shiklomanov
}
