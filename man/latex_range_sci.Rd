% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex_table.R
\name{latex_range_sci}
\alias{latex_range_sci}
\title{Make latex notation for best value and confidence boundaries.}
\usage{
latex_range_sci(
  best,
  min,
  max,
  ndigits = "auto",
  na = "-",
  symmetry = FALSE,
  base_pow = NULL
)
}
\arguments{
\item{best}{best value}

\item{min}{lower boundary value}

\item{max}{upper boundary value}

\item{ndigits}{'auto' (default) or number of digits after decimal point}

\item{na}{show NA value as na. default na='-'}

\item{symmetry}{if errors are symmetric show as value+/-err}

\item{base_pow}{set to numeric if you want to make column with the same power.}
}
\value{
character
}
\description{
Make latex notation for best value and confidence boundaries.
}
\examples{

x <- c(NA, 1e21, 3e22, 1e23)
err <- c(NA, 0.1e21, 0.5e21, 0.1e23)
latex_range_sci(best = x, min = x - err, max = x + err, ndigits = 2)

latex_range_sci(x, x - err, x + err, ndigits = 2, na = '', symmetry = TRUE)

latex_range_sci(x, x - err, x + err, ndigits = 2, base_pow=21)

}
