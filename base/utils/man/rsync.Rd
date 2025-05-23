% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rsync}
\alias{rsync}
\title{R implementation of rsync}
\usage{
rsync(args, from, to, pattern = "")
}
\arguments{
\item{args}{rsync arguments (see man rsync)}

\item{from}{source}

\item{to}{destination}

\item{pattern}{file pattern to be matched}
}
\value{
nothing, transfers files as a side effect
}
\description{
rsync is a file copying tool in bash
}
\author{
David LeBauer

Shawn Serbin
}
