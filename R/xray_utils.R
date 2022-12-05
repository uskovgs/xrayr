#' Energy Conversion Factor
#'
#' Convert energy flux to photon flux
#'
#' see https://hea-www.harvard.edu/~pgreen/figs/xray_cheat.pdf
#'
#' @param E1 energy min in keV
#' @param E2 energy max in keV
#' @param pho_ind Photon index, 2.0 default.
#'
#' @return numeric
#' @export
#'
#' @examples
#'
#' energy_conv_factor(0.5, 8.0, pho_ind = 1.7)
#'
energy_conv_factor <- function(E1, E2, pho_ind = 2.0) {
  h <- 6.626e-27 # erg s
  nu0 <- E1 * 1.602e-9 / h
  nu1 <- E2 * 1.602e-9 / h
  alpha <- 1 - pho_ind
  h*nu0 * ((nu1 / nu0) ^ (alpha + 1) -1) / ((nu1 / nu0)^alpha - 1) * alpha / (alpha+1)
}
