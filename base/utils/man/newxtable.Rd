% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{newxtable}
\alias{newxtable}
\title{New xtable}
\usage{
newxtable(
  x,
  environment = "table",
  table.placement = "ht",
  label = NULL,
  caption = NULL,
  caption.placement = NULL,
  align = NULL
)
}
\arguments{
\item{x}{data.frame to be converted to latex table}

\item{environment}{can be 'table'; 'sidewaystable' if using latex rotating package}

\item{table.placement, label, caption, caption.placement, align}{passed to \code{\link[xtable]{xtable}}}
}
\value{
Latex version of table, with percentages properly formatted
}
\description{
utility to properly escape the '\%' sign for latex
}
\author{
David LeBauer
}
