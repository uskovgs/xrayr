#' Likelihood for Gaussian data
#'
#' @param obs observed data rates
#' @param model the values of the predicted data rates based on the model
#'
#' see https://heasarc.gsfc.nasa.gov/xanadu/xspec/manual/XSappendixStatistics.html
#'
#' @return
#' @export
#'
#' @examples
loglikelihood_chi <- function(obs, model) {
  sum((obs - model) ^ 2 / obs)
}

#' Likelihood for Poisson distributed data
#'
#' @param obs observed counts
#' @param model_rate predicted count rates based on the current model
#' @param exp_time exposure tim
#'
#' see https://heasarc.gsfc.nasa.gov/xanadu/xspec/manual/XSappendixStatistics.html
#'
#' @return Stirling's approximation, numeric
#' @export
#'
#' @examples
loglikelihood_cstat <- function(obs, model_rate, exp_time = 1) {
  cstat <- exp_time * model_rate - obs + obs * (log(obs) - log(exp_time * model_rate))
  2 * sum(cstat)
}
