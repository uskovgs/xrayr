check_ndigits <- function(x, ndigits=0){
  if(any(round(x, ndigits) == 0))
    warning('Is argument `ndigits` too small?')
}

#' Make latex notation for best value and confidence boundaries.
#'
#' @param best best value
#' @param min lower boundary value
#' @param max upper boundary value
#' @param ndigits number of digits after decimal point
#' @param na show NA value as na. default na='-'
#'
#' @return character vector
#' @export
#'
#' @examples
#' x <- c(NA,1:10, NA)
#' x <- x + rnorm(length(x))
#' err <- rnorm(length(x))
#'
#' latex_range(best = x, min = x - err, max = x + err, ndigits = 2)
#'
#' latex_range(best = x, min = x - err, max = x + 2*err, ndigits = 2, na='')
#'
latex_range <- function(best, min, max, ndigits=0, na='-'){
  is_na <- is.na(best) | is.na(min) | is.na(max)
  n <- length(best)

  best <- best[!is_na]
  lo <- round(min[!is_na]-best, ndigits)
  up <- round(max[!is_na]-best, ndigits)


  check_ndigits(x=c(lo,up), ndigits)

  out <- character(n)

  fmt_par <- paste0('%.', ndigits, 'f')

  if(all(-lo == up)){
    out[!is_na] <- paste0("$",
                          sprintf(fmt = fmt_par, best),
                          "\\pm", sprintf(fmt=fmt_par, up),
                          "$")

  } else{
    out[!is_na] <- paste0("$",
                          sprintf(fmt = fmt_par, best),
                          "_{", sprintf(fmt=fmt_par, lo),
                          "}^{+", sprintf(fmt=fmt_par, up),
                          "}$")
  }


  out[is_na] <- paste0("$", na, '$')

  return(out)
}

#' Make latex notation for best value and confidence boundaries.
#'
#' @param best best value
#' @param min lower boundary value
#' @param max upper boundary value
#' @param ndigits number of digits after decimal point
#' @param na show NA value as na. default na='-'
#' @param symmetry if errors are symmetric show as value+/-err
#' @param base_pow set to numeric if you want to make column with the same power.
#'
#' @return character
#' @export
#'
#' @examples
#'
#' x <- c(NA, 1e21, 3e22, 1e23)
#' err <- c(NA, 0.1e21, 0.5e21, 0.1e23)
#' latex_range_sci(best = x, min = x - err, max = x + err, ndigits = 2)
#'
#' latex_range_sci(x, x - err, x + err, ndigits = 2, na = '', symmetry = TRUE)
#'
#' latex_range_sci(x, x - err, x + err, ndigits = 2, base_pow=21)
#'
latex_range_sci <- function(best, min, max, ndigits=0,
                            na='-', symmetry=FALSE, base_pow=NULL){
  # symmetry if available
  is_na <- is.na(best) | is.na(min) | is.na(max)
  n <- length(best)

  pow <- if(is.null(base_pow)) trunc(log10(abs(best))) else rep(base_pow, n)
  # pow <- ife(base_pow, log10(abs(best)), rep(base))
  #   base_pow %||% trunc(log10(abs(best)))

  best <- best / 10^pow
  min <- min / 10^pow
  max <- max / 10^pow

  best <- best[!is_na]
  lo <- round(min[!is_na]-best, ndigits)
  up <- round(max[!is_na]-best, ndigits)
  check_ndigits(c(lo, up), ndigits)

  out <- character(n)
  fmt_par <- paste0('%.', ndigits, 'f')

  if(all(-lo == up) & symmetry){
    out[!is_na] <- paste0("$(",
                          sprintf(fmt = fmt_par, best),
                          "\\pm", sprintf(fmt=fmt_par, up),
                          ")~\\times10^{",pow[!is_na], "}",
                          "$")
  } else{
    out[!is_na] <- paste0("$",
                          sprintf(fmt = fmt_par, best),
                          "_{", sprintf(fmt=fmt_par, lo),
                          "}^{+", sprintf(fmt=fmt_par, up),
                          "}~\\times10^{",pow[!is_na], "}",
                          "$")
  }

  out[is_na] <- paste0("$", na, '$')

  return(out)
}
