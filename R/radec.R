
# This is the first try to


# Main function -----------------------------------------------------------

new_radec <- function(ra = double(), dec = double()){
  vctrs::vec_assert(ra, double())
  vctrs::vec_assert(dec, double())
  vctrs::new_rcrd(list(ra = ra, dec = dec), class = "astro_radec")
}



# Helpers -----------------------------------------------------------------

#' Title
#'
#' @param ra
#' @param dec
#'
#' @return
#' @export
#'
#' @examples
ra_dec <- function(ra=double(), dec = double()){
  ra <- vctrs::vec_cast(ra, double())
  dec <- vctrs::vec_cast(dec, double())
  new_radec(ra, dec)
}

#' Title
#'
#' @param radec
#'
#' @return
#' @export
#'
#' @examples
radec <- function(radec = character()) {
  radec <- vctrs::vec_cast(radec, character())

  radec_parsed <- stringr::str_match_all(radec, pattern = "^([\\d\\.]*)[\\s:h]?\\s?([\\d\\.]*)?[\\s:m]?\\s?([\\d\\.]*)?s?\\s([+-])?\\s?([\\d\\.]*)[\\s:d]?\\s?([\\d\\.]*)?[\\s:m]?\\s?([\\d\\.]*)?s?")
  ra <- purrr::map_dbl(radec_parsed, ~ (as.numeric(.x[1,2:4])/c(1, 60, 3600)) |> sum(na.rm = T))
  ra <- ra * 15 # 1 hour = 15 degrees

  dec_sign <- purrr::map_dbl(radec_parsed, ~ifelse(.x[5] == '+' | is.na(.x[5]), 1, -1))
  dec <- purrr::map_dbl(radec_parsed, ~ (as.numeric(.x[1,6:8])/c(1, 60, 3600)) |> sum(na.rm = T))
  dec <- dec_sign * dec

  new_radec(ra, dec)
}

# Get RA and DEC ----------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ra <- function(x){
  UseMethod('ra')
}
ra.default <- function(x, ...){
  x
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
ra.astro_radec <- function(x){
  stopifnot(inherits(x, 'astro_radec'))
  vctrs::vec_data(x)$ra
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dec <- function(x){
  UseMethod('dec')
}
dec.default <- function(x, ...){
  x
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
dec.astro_radec <- function(x){
  stopifnot(inherits(x, 'astro_radec'))
  vctrs::vec_data(x)$dec
}


# Separation --------------------------------------------------------------
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
separation <- function(x, y){
  UseMethod('separation')
}
separation.default <- function(x, y){
  stop('Not radec obj')
}


#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
separation.astro_radec <- function(x, y){
  # see https://en.wikipedia.org/wiki/Great-circle_distance
  ra_x <- ra(x) * pi / 180
  dec_x <- dec(x) * pi / 180

  ra_y <- ra(y) * pi / 180
  dec_y <- dec(y) * pi / 180

  numerator <- (cos(dec_y)*sin(ra_x-ra_y))^2 + (cos(dec_x)*sin(dec_y) - sin(dec_x)*cos(dec_y)*cos(ra_x-ra_y))^2
  denomin <- sin(dec_x)*sin(dec_y) + cos(dec_x)*cos(dec_y)*cos(ra_x - ra_y)

  return(atan(sqrt(numerator) / denomin) * 3600 * 180 / pi)
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_radec <- function(x){
  inherits(x, "astro_radec")
}



# Format and formatters ---------------------------------------------------


#' Title
#'
#' @param x
#' @param ...
#' @param formatter
#' @param nsec
#' @param isplain
#'
#' @return
#' @export
#'
#' @examples
format.astro_radec <- function(x,formatter = formatter_hmsdms, nsec=1, isplain=T, ...){
  x_valid <- which(!is.na(x))

  ra <- vctrs::field(x, 'ra')[x_valid]
  dec <- vctrs::field(x, 'dec')[x_valid]

  ret <- rep(NA_character_, vctrs::vec_size(x))
  ret[x_valid] <- paste0(formatter(ra, 'ra', nsec, isplain), " ", formatter(dec, 'dec', nsec, isplain))
  ret
}

#' Title
#'
#' @param x
#' @param ra_or_dec
#' @param nsec
#' @param isplain
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
formatter_hmsdms <- function(x, ra_or_dec, nsec=1, isplain=T, ...){

  if(ra_or_dec == 'ra') x <- x / 15


  ra_units <- c(pillar::style_subtle("h"),
                pillar::style_subtle("m"),
                pillar::style_subtle("s"))
  if(isplain) ra_units <- c('h', 'm', 's')

  dec_units <- c(pillar::style_subtle("°"),
                 pillar::style_subtle("'"),
                 pillar::style_subtle('"'))
  if(isplain) dec_units <- c("°", "'", '"')

  units <- if(ra_or_dec == 'ra') ra_units else dec_units

  sign_x <- if(ra_or_dec == 'dec') ifelse(sign(x) > 0, '+', '-')

  x0 <- abs(x)
  x1 <- as.integer(x0)
  x2_tmp <- x2_tmp <- sprintf("%.10f",(x0 - x1) * 60) |> as.numeric()
  x2 <- as.integer(x2_tmp)
  x3 <- (x2_tmp - x2) * 60

  ret <- paste0(sign_x, x1, units[1], x2, units[2], sprintf("%." %s% nsec %s% "f",x3), units[3])
  format(ret, justify='right')
}

#' Title
#'
#' @param x
#' @param ra_or_dec
#' @param nsec
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
formatter_deg <- function(x, ra_or_dec, nsec=4, ...){
  ret <- paste0(sprintf("%." %s% nsec %s% "f", x), pillar::style_subtle("°"))
  format(ret, justify='right')
}

#' Title
#'
#' @param x
#' @param ra_or_dec
#' @param nsec
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
formatter_latex <- function(x, ra_or_dec, nsec=1, ...){

  if(ra_or_dec == 'ra') x <- x / 15

  sign_x <- if(ra_or_dec == 'dec') ifelse(sign(x) > 0, '+', '-')

  x0 <- abs(x)
  x1 <- as.integer(x0)
  x2_tmp <- x2_tmp <- sprintf("%.10f",(x0 - x1) * 60) |> as.numeric()
  x2 <- as.integer(x2_tmp)
  x3 <- (x2_tmp - x2) * 60

  if(ra_or_dec == 'dec') ret <- paste0(sign_x, x1, ' ', x2, ' ', sprintf("%.0f", x3))
  if(ra_or_dec == 'ra') ret <- paste0(sign_x, x1, ' ', x2, ' ', sprintf("%." %s% nsec %s% "f",x3))

  format(ret, justify='right')
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @importFrom pillar pillar_shaft
#' @export
#'
#' @examples
pillar_shaft.astro_radec <- function(x, ...) {
  out <- format(x, formatter = formatter_hmsdms, nsec=1, isplain = F)
  pillar::new_pillar_shaft_simple(out, align = "left")
}


# Short type in tibble -------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
vec_ptype_abbr.astro_radec <- function(x) {
  "RA DEC"
}


# Concat and coarse -------------------------------------------------------

#' @export
vec_ptype2.astro_radec.astro_radec <- function(x, y, ...) new_radec()

#' @export
vec_ptype2.astro_radec.astro_radec <- function(x, y, ...) new_radec()

#' @export
vec_ptype2.astro_radec.character <- function(x, y, ...) new_radec()

#' @export
vec_ptype2.character.astro_radec <- function(x, y, ...) new_radec()

#' @export
vec_cast.astro_radec.astro_radec <- function(x, to, ...) x

#' @export
vec_cast.astro_radec.character <- function(x, to, ...) format(x)

#' @export
vec_cast.character.astro_radec <- function(x, to, ...) format(x)

