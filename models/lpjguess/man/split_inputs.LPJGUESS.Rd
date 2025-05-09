% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_inputs.LPJGUESS.R
\name{split_inputs.LPJGUESS}
\alias{split_inputs.LPJGUESS}
\title{Split inputs for LPJ-GUESS model}
\usage{
split_inputs.LPJGUESS(
  settings,
  start.time,
  stop.time,
  inputs,
  overwrite = FALSE,
  outpath = NULL
)
}
\arguments{
\item{settings}{PEcAn settings object}

\item{start.time}{start date and time for each SDA ensemble}

\item{stop.time}{stop date and time for each SDA ensemble}

\item{inputs}{list of model inputs to use in write.configs.LPJGUESS}

\item{overwrite}{Default FALSE}

\item{outpath}{if specified, write output to a new directory. Default NULL writes back to the directory being read}
}
\value{
name of the split met file
}
\description{
Splits climate met for LPJGUESS
}
\author{
Istem Fer
}
