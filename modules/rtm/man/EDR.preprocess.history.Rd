% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edr.wrapper.R
\name{EDR.preprocess.history}
\alias{EDR.preprocess.history}
\title{Preprocess history file for EDR}
\usage{
EDR.preprocess.history(
  history.path,
  output.path,
  datetime,
  history.prefix = "history"
)
}
\arguments{
\item{history.path}{Path to directory containing history file.}

\item{output.path}{Directory in which to execute the run.}

\item{datetime}{POSIX date and time for run}

\item{history.prefix}{String describing the history file prefix in
\code{history.path}. Default = 'history'}
}
\description{
Locate history file based on path and prefix, copy to specified output
directory, and rename to correct time.
}
