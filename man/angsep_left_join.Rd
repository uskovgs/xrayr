% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossmatch.R
\name{angsep_left_join}
\alias{angsep_left_join}
\title{Non-equi join tables by angular distance.}
\usage{
angsep_left_join(x, y, by = NULL, max_sep = 1, sep_col = NULL)
}
\arguments{
\item{x}{data.frame with \code{astro_radec} column.}

\item{y}{data.frame with \code{astro_radec} column.}

\item{by}{A character vector of variables to join by.}

\item{max_sep}{maximum separation in units of arcseconds. Default is 1 arcsec.

If \code{NULL}, the default, \code{angsep_join} will perform a join, using first \code{astro_radec}
variables in \code{x} and \code{y}.

To join by different specific variables on \code{x} and \code{y}, use a named vector. For example,
\code{by = c("coord1" = "coord2")} will cross-match \code{x$coord1} to \code{y$coord2}.}

\item{sep_col}{A name of the distance column. If \code{NULL}, the default, it drops
the \code{sep_col} column in the output data.frame.}
}
\value{
A data.frame containing all rows from \code{x}.
}
\description{
Non-equi join tables by angular distance.
}
