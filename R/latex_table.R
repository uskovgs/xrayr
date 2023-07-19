check_ndigits <- function(x, ndigits=0){
  if(any(round(x, ndigits) == 0))
    warning('Is argument `ndigits` too small?')
}

#' Make latex notation for best value and confidence boundaries.
#'
#' @param best best value
#' @param min lower boundary value
#' @param max upper boundary value
#' @param ndigits 'auto' (default) or number of digits after decimal point
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
latex_range <- function(best, min, max, ndigits = 'auto', na='-') {
  is_na <- is.na(best) | is.na(min) | is.na(max)
  n <- length(best)

  x <- best[!is_na]
  xmin <- min[!is_na]
  xmax <- max[!is_na]


  if (ndigits == 'auto') {
    err_min <- pmin(xmax - x, x - xmin)
    ndigits <- calc_signif_digits(err_min)
  }

  lo <- round(x - xmin, ndigits)
  up <- round(xmax - x, ndigits)

  if (length(ndigits) == 1L) check_ndigits(x = c(lo, up), ndigits)

  out <- character(n)
  # fmt_par <- paste0('%.', ndigits, 'f')


  if(all(lo == up)){
    out[!is_na] <- paste0("$",
                          # sprintf(fmt = fmt_par, x),
                          format_number(x, ndigits),
                          # "\\pm", sprintf(fmt=fmt_par, up),
                          "\\pm", format_number(lo, ndigits),
                          "$")

  } else{
    # out[!is_na] <- paste0("$",
    #                       sprintf(fmt = fmt_par, x),
    #                       "_{", sprintf(fmt=fmt_par, lo),
    #                       "}^{+", sprintf(fmt=fmt_par, up),
    #                       "}$")
    out[!is_na] <- paste0("$",
                          format_number(x, ndigits),
                          "_{-", format_number(lo, ndigits),
                          "}^{+", format_number(up, ndigits),
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
#' @param ndigits 'auto' (default) or number of digits after decimal point
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
latex_range_sci <- function(best, min, max, ndigits = 'auto',
                            na='-', symmetry=FALSE, base_pow=NULL){
  # symmetry if available
  is_na <- is.na(best) | is.na(min) | is.na(max)
  n <- length(best)

  x <- best[!is_na]
  xmin <- min[!is_na]
  xmax <- max[!is_na]
  delta_min <- x - xmin
  delta_max <- xmax - x

  if (is.null(base_pow)){
    pow <- trunc(log10(abs(xmin))) + 1
  } else {
    pow <- rep(base_pow, length(xmin))
  }

  if (ndigits == 'auto') {
    err_min <- pmin(xmax - x, x - xmin)
    ndigits <- calc_signif_digits(err_min / 10^pow)
  }

  x <- x / 10^pow
  xmin <- xmin / 10^pow
  xmax <- xmax / 10^pow



  lo <- round(x - xmin, ndigits)
  up <- round(xmax - x, ndigits)

  if (length(ndigits) == 1L) check_ndigits(x = c(lo, up), ndigits)

  out <- character(n)
  # fmt_par <- paste0('%.', ndigits, 'f')


  if(all(delta_min == delta_max) & symmetry){
    # out[!is_na] <- paste0("$(",
    #                       sprintf(fmt = fmt_par, best),
    #                       "\\pm", sprintf(fmt=fmt_par, up),
    #                       ")~\\times10^{",pow[!is_na], "}",
    #                       "$")
    out[!is_na] <- paste0("$(",
                          format_number(x, ndigits),
                          "\\pm", format_number(lo, ndigits),
                          ")~\\times10^{", pow , "}",
                          "$")
  } else{
    # out[!is_na] <- paste0("$",
    #                       sprintf(fmt = fmt_par, best),
    #                       "_{", sprintf(fmt=fmt_par, lo),
    #                       "}^{+", sprintf(fmt=fmt_par, up),
    #                       "}~\\times10^{",pow[!is_na], "}",
    #                       "$")
    out[!is_na] <- paste0("$",
                          format_number(x, ndigits),
                          "_{-", format_number(lo, ndigits),
                          "}^{+", format_number(up, ndigits),
                          "}~\\times10^{",pow, "}",
                          "$")
  }

  out[is_na] <- paste0("$", na, '$')

  return(out)
}

get_n_signif_digits <- function(x) {
  first_digit <- abs(10^round(abs(log10(abs(x))), 0) * x) %/% 1
  n_signif_digits <- abs(ceiling(-log10(abs(x))))
  n_signif_digits <- ifelse(first_digit == 1, n_signif_digits + 1, n_signif_digits)
  n_signif_digits[abs(x) >= 2] <- 0

  n_signif_digits
}

calc_signif_digits <- function(x) {
  n_signif_digits <- get_n_signif_digits(x)
  rounded_x <- round(x, n_signif_digits)
  n_signif_digits <- ifelse(
    get_n_signif_digits(rounded_x) < n_signif_digits,
    n_signif_digits - 1,
    n_signif_digits
  )

  n_signif_digits
}


format_number <- function(x, n_signif_digits) {
  # 0.198 -> 0.2, (NOT 0.20)
  not_na <- ! (is.na(x) | is.na(n_signif_digits))
  out <- character(length(x))
  if (length(n_signif_digits) > 1L) n_signif_digits <- n_signif_digits[not_na]
  rounded_x <- round(x[not_na], n_signif_digits)
  out[not_na] <- sprintf(fmt = glue::glue("%.{n_signif_digits}f"), rounded_x)
  out[!not_na] <- NA

  out
}

# numbers <- c(runif(11))
# errors <- c(runif(10, max = 0.1), 0.00123)
#
# tibble(
#   number = numbers,
#   error = errors,
#   fmt = paste0(format_number(numbers, n_signif_digits = get_n_signif_digits(errors)),
#                "+/-",
#                format_number(errors, get_n_signif_digits(errors))
#   )
# )

