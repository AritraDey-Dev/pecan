% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/met2model.LPJGUESS.R
\name{met2model.LPJGUESS}
\alias{met2model.LPJGUESS}
\title{met2model.LPJGUESS}
\usage{
met2model.LPJGUESS(
  in.path,
  in.prefix,
  outfolder,
  start_date,
  end_date,
  overwrite = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{in.path}{location on disk where inputs are stored}

\item{in.prefix}{prefix of input and output files}

\item{outfolder}{location on disk where outputs will be stored}

\item{start_date}{the start date of the data to be downloaded (will only use the year part of the date)}

\item{end_date}{the end date of the data to be downloaded (will only use the year part of the date)}

\item{overwrite}{should existing files be overwritten}

\item{verbose}{should the function be very verbose}

\item{...}{additional arguments, currently ignored}
}
\description{
met2model wrapper for LPJ-GUESS
}
\author{
Istem Fer
}
