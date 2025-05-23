% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model2netcdf.LINKAGES.R
\name{model2netcdf.LINKAGES}
\alias{model2netcdf.LINKAGES}
\title{Convert LINKAGES's output into netCDF format}
\usage{
model2netcdf.LINKAGES(
  outdir,
  sitelat,
  sitelon,
  start_date = NULL,
  end_date = NULL,
  pft_names = NULL
)
}
\arguments{
\item{outdir}{Location of model output}

\item{sitelat}{Latitude of the site}

\item{sitelon}{Longitude of the site}

\item{start_date}{Start time of the simulation}

\item{end_date}{End time of the simulation}

\item{pft_names}{names of PFTs to use in output labels}
}
\description{
Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
}
\author{
Ann Raiho, Betsy Cowdery
}
