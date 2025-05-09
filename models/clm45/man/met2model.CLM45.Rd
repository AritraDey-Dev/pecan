% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/met2model.CLM45.R
\name{met2model.CLM45}
\alias{met2model.CLM45}
\title{met2model for CLM45}
\usage{
met2model.CLM45(
  in.path,
  in.prefix,
  outfolder,
  start_date,
  end_date,
  lst = 0,
  lat,
  lon,
  ...,
  overwrite = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{in.path}{location on disk where inputs are stored}

\item{in.prefix}{prefix of input and output files}

\item{outfolder}{location on disk where outputs will be stored}

\item{start_date}{the start date of the data to be downloaded (will only use the year part of the date)}

\item{end_date}{the end date of the data to be downloaded (will only use the year part of the date)}

\item{lst}{timezone offset to GMT in hours}

\item{lat, lon}{site coordinates}

\item{...}{other arguments, currently ignored}

\item{overwrite}{should existing files be overwritten}

\item{verbose}{should the function be very verbosefor(year in start_year:end_year)}
}
\description{
met2model wrapper for CLM45
}
