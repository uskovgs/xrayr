% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/likelihoods.R
\name{loglikelihood_cstat}
\alias{loglikelihood_cstat}
\title{Likelihood for Poisson distributed data}
\usage{
loglikelihood_cstat(obs, model_rate, exp_time = 1)
}
\arguments{
\item{obs}{observed counts}

\item{model_rate}{predicted count rates based on the current model}

\item{exp_time}{exposure tim

see https://heasarc.gsfc.nasa.gov/xanadu/xspec/manual/XSappendixStatistics.html}
}
\value{
Stirling's approximation, numeric
}
\description{
Likelihood for Poisson distributed data
}
