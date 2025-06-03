
# This is the first try to


# Main function -----------------------------------------------------------


#' @importFrom vctrs vec_assert
#' @importFrom vctrs new_rcrd
new_radec <- function(ra = double(), dec = double()){
  vec_assert(ra, double())
  vec_assert(dec, double())

  i_valid <- which(!is.na(ra) & !is.na(dec))


  validate_ra(ra[i_valid])
  validate_dec(dec[i_valid])

  new_rcrd(list(ra = ra, dec = dec), class = "astro_radec")
}

validate_ra <- function(ra = double()){
  if(any(ra > 360 | ra < 0)){
    stop("RA should fall into the range 0 to 360.",
         .call = FALSE)
  }
}
validate_dec <- function(dec = double()){
  if(any(dec > 90 | dec < -90)){
    stop("DEC should fall into the range -90 to +90.",
         .call = FALSE)
  }
}



# Helpers -----------------------------------------------------------------

#' Create RA DEC object
#'
#' @param ra in degrees
#' @param dec in degrees
#'
#' @return
#' @export
#'
#' @importFrom vctrs vec_cast
#'
#' @examples ra_dec(ra = 213.1, dec=65.3)
ra_dec <- function(ra=double(), dec = double()){
  ra <- vec_cast(ra, double())
  dec <- vec_cast(dec, double())
  new_radec(ra, dec)
}

#' Title
#'
#' @param radec in "hms dms"
#'
#' @return
#' @export
#'
#' @importFrom vctrs vec_cast
#' @importFrom purrr map_dbl
#' @importFrom stringr str_match
#' @importFrom stringr str_match_all
#'
#' @examples
#' radec('12 34 56 -76  54 3.210')
#' radec('12 34 56 -76  54 3.210') %>% tibble::tibble()
radec <- function(radec = character()) {

  is_na <- is.na(radec)
  N <- length(radec)

  radec <- vec_cast(radec, character())
  radec_orig <- radec
  radec <- radec_orig[!is_na]

  j <- grepl('^J', radec)
  if(any(j)){
    ra <- str_match(radec[j], 'J(.+)[+-]')[,2]
    ra1 <- paste0(substr(ra, 1,2),
                  ":",
                  substr(ra, 3,4),
                  ":",
                  substr(ra, 5, nchar(ra)))

    dec <- str_match(radec[j], "J.+([+-].+)")[,2]
    dec1 <- paste0(substr(dec, 1,3),
                   ":",
                   substr(dec, 4,5),
                   ":",
                   substr(dec,6, nchar(dec))
    )
    radec[j] <- paste(ra1, dec1)
  }

  radec_parsed <- str_match_all(radec, pattern = "^([\\d\\.]*)[\\s:h]?\\s?([\\d\\.]*)?[\\s:m]?\\s?([\\d\\.]*)?s?\\s([+-])?\\s?([\\d\\.]*)[\\s:d°]?\\s?([\\d\\.]*)?[\\s:m’]?\\s?([\\d\\.]*)?s?")
  ra <- map_dbl(radec_parsed, ~ (as.numeric(.x[1,2:4])/c(1, 60, 3600)) |> sum(na.rm = T))
  ra <- ra * 15 # 1 hour = 15 degrees

  dec_sign <- map_dbl(radec_parsed, ~ifelse(.x[5] == '+' | is.na(.x[5]), 1, -1))
  dec <- map_dbl(radec_parsed, ~ (as.numeric(.x[1,6:8])/c(1, 60, 3600)) |> sum(na.rm = T))
  dec <- dec_sign * dec

  ra1 <- vector("double", N)
  dec1 <- vector("double", N)

  ra1 <- NA
  dec1 <- NA

  ra1[!is_na] <- ra
  dec1[!is_na] <- dec

  new_radec(ra1, dec1)
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

# Get RA and DEC ----------------------------------------------------------


#' @export
ra <- function(x){
  UseMethod('ra')
}
ra.default <- function(x, ...){
  x
}

#' Get RA
#'
#' @param x
#'
#' @return
#' @export
#'
#' @importFrom vctrs vec_data
#'
#' @examples
ra.astro_radec <- function(x){
  stopifnot(inherits(x, 'astro_radec'))
  vctrs::vec_data(x)$ra
}


#' @export
dec <- function(x){
  UseMethod('dec')
}
dec.default <- function(x, ...){
  x
}
#' Get DEC
#'
#' @param x
#'
#' @return
#' @export
#'
#' @importFrom vctrs vec_data
#'
#' @examples
dec.astro_radec <- function(x){
  stopifnot(inherits(x, 'astro_radec'))
  vctrs::vec_data(x)$dec
}


# Separation --------------------------------------------------------------

#' @export
separation <- function(x, y, metric) {
  UseMethod('separation')
}
separation.default <- function(x, y, metric){
  stop('Not radec obj')
}


#' Angular distance between two points
#'
#' @param x
#' @param y
#' @param metric [distance_vincenty()] (default) or [distance_haversine()]
#'
#' @return
#' @export
#'
#' @examples
separation.astro_radec <- function(x, y, metric = distance_vincenty){
  metric(ra(x), dec(x), ra(y), dec(y))
}

#' Angular distance
#'
#' @param ra1,dec1 equatorial coordinates of object 1 in degrees
#' @param ra2,dec2 equatorial coordinates of object 2 in degrees
#'
#' @returns distance in arcsec
#'
#' @seealso [distance_vincenty()]
#' @export
#'
#' @examples
distance_haversine <- function(ra1, dec1, ra2, dec2) {

  to_rad <- pi / 180
  dphi   <- (dec2 - dec1) * to_rad
  dlambda <- (ra2 - ra1)  * to_rad

  dlambda <- abs(dlambda)
  dlambda[dlambda > pi] <- 2 * pi - dlambda[dlambda > pi]

  dec1 <- dec1 * to_rad
  dec2 <- dec2 * to_rad

  d <- sin(dphi / 2)^2 +
    cos(dec1) * cos(dec2) * sin(dlambda / 2)^2

  d <- 2 * asin(sqrt(d)) * 3600 / to_rad
  return(d)
}

#' Angular distance
#'
#' @param ra1,dec1 equatorial coordinates of object 1 in degrees
#' @param ra2,dec2 equatorial coordinates of object 2 in degrees
#'
#' @return distance in arcsec
#'
#' @seealso [distance_haversine()]
#' @export
distance_vincenty <- function(ra1, dec1, ra2, dec2) {

  to_rad <- pi / 180
  ra1  <- ra1 * to_rad
  ra2  <- ra2 * to_rad
  dec1 <- dec1 * to_rad
  dec2 <- dec2 * to_rad

  dlambda <- abs(ra1 - ra2)
  dlambda[dlambda > pi] <- 2 * pi - dlambda[dlambda > pi]

  num <- (cos(dec2) * sin(dlambda))^2 +
    (cos(dec1) * sin(dec2) -
       sin(dec1) * cos(dec2) * cos(dlambda))^2

  den <- sin(dec1) * sin(dec2) +
    cos(dec1) * cos(dec2) * cos(dlambda)

  d <- atan2(sqrt(num), den) * 3600 / to_rad
  return(d)
}


# Convert to galactic coords ----------------------------------------------

#' @export
to_galactic <- function(x, obs_epoch="J2000"){
  UseMethod('to_galactic')
}
to_galactic.default <- function(x, obs_epoch="J2000"){
  stop('Not radec obj')
}

#' Convert radec to galactic coords.
#'
#'
#' @param x `astro_radec` object
#' @param epoch "J2000" (default) or "B1950"
#'
#' @return list(l=longitude[deg], b = latitude[deg])
#' @export
#'
#' @examples
to_galactic.astro_radec <- function(x, obs_epoch = "J2000"){
  checkmate::assert_choice(obs_epoch, c("J2000", "B1950"))

  to_rad <- pi / 180
  ra <- ra(x) * to_rad
  dec <- dec(x) * to_rad

  v_eq <- rbind(cos(dec) * cos(ra), cos(dec) * sin(ra), sin(dec))

  R <- if (obs_epoch == "J2000") {
    # ESA 1997, Perryman & ESA (1997)
    matrix(c(
      -0.0548755604, -0.8734370902, -0.4838350155,
      0.4941094279, -0.4448296300,  0.7469822445,
      -0.8676661490, -0.1980763734,  0.4559837762),
      nrow = 3, byrow = TRUE)
  } else {
    # SLALIB / PAL (B1950)
    matrix(c(
      -0.066988739415, -0.872755765853, -0.483538914632,
      0.492728466075, -0.450346958020,  0.744584633283,
      -0.867600811151, -0.188374601722,  0.460199784784),
      nrow = 3, byrow = TRUE)
  }

  v_gal <- R %*% v_eq
  b  <- asin(v_gal[3, ])
  l  <- atan2(v_gal[2, ], v_gal[1, ])


  return(
    list(
      l = (l * 180 / pi) %% 360,
      b = b * 180 / pi
    )
  )
}



# Format and formatters ---------------------------------------------------


#' Title
#'
#' @param x
#' @param ...
#' @param formatter
#' @param nsec
#' @param color print color?
#' @param isPlain units - white spaces
#'
#' @return
#' @export
#'
#' @importFrom vctrs field
#' @importFrom vctrs vec_size
#'
#' @examples
format.astro_radec <- function(x, formatter = formatter_hmsdms, nsec=1, color=F,isPlain=F, ...){
  x_valid <- which(!is.na(x))

  ra <- field(x, 'ra')[x_valid]
  dec <- field(x, 'dec')[x_valid]

  ret <- rep(NA_character_, vec_size(x))
  ret[x_valid] <- paste0(formatter(ra, 'ra', nsec, color=color, isPlain=isPlain),
                         " ",
                         formatter(dec, 'dec', nsec, color=color, isPlain=isPlain))
  ret
}

#' Title
#'
#' @param x
#' @param ra_or_dec
#' @param nsec
#' @param color print with color?
#' @param isPlain units - white spaces
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom pillar style_subtle
#'
#' @examples
formatter_hmsdms <- function(x, ra_or_dec, nsec=1, color=FALSE, isPlain=F, ...){

  if(ra_or_dec == 'ra') x <- x / 15

  ra_units <- c("h","m","s")
  dec_units <- c("°", "'", '"')

  if(isPlain) {
    ra_units <- c(" ", " ", " ")
    dec_units <- c(" ", " ", " ")
  }


  x0 <- abs(x)
  x1 <- as.integer(x0) # h
  x2_tmp <- sprintf("%.10f",(x0 - x1) * 60) %>% as.numeric()
  x2 <- as.integer(x2_tmp) # m
  x3 <- round((x2_tmp - x2) * 60, nsec+1)

  if(ra_or_dec == 'ra'){
    units <- ra_units
    if(color) {
      units <- style_subtle(units)
    }
    # fmt_par <- paste0("%02d%s%02d%s%0", nsec+3, ".", nsec, "f%s")
    # ret <- sprintf(fmt = fmt_par,
    #                x1,
    #                units[1],
    #                x2,
    #                units[2],
    #                x3,
    #                units[3])
    fmt_par <- paste0("%0", (ifelse(nsec == 0, 2, 3) +nsec), ".", nsec, "f")
    ret <- paste0(sprintf('%02d', x1),
                  units[1],
                  sprintf('%02d', x2),
                  units[2],
                  sprintf(fmt = fmt_par, round(x3, nsec)),
                  units[3])

  }
  if(ra_or_dec == 'dec'){
    nsec <- ifelse(nsec > 0, nsec-1, nsec)
    sign_x <- ifelse(sign(x) > 0, '+', '-')
    units <- dec_units
    if(color) {
      units <- style_subtle(units)
      sign_x <- style_subtle(sign_x)
    }




    # ret <- sprintf(fmt = paste0("%s%02d%s%02d%s%0", nsec+3, ".", nsec, "f%s"),
    #                sign_x,
    #                x1,
    #                units[1],
    #                x2,
    #                units[2],
    #                x3,
    #                units[3])

    fmt_par <- paste0("%0", (ifelse(nsec==0, 2, 3) +nsec), ".", nsec, "f")

    ret <- paste0(sign_x,
                  sprintf('%02d', x1),
                  units[1],
                  sprintf('%02d', x2),
                  units[2],
                  sprintf(fmt = fmt_par, round(x3, nsec)),
                  units[3])


  }


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

  warning('Test function')

  if(ra_or_dec == 'ra') x <- x / 15

  sign_x <- if(ra_or_dec == 'dec') ifelse(sign(x) > 0, '+', '-')

  x0 <- abs(x)
  x1 <- as.integer(x0)
  x2_tmp <- sprintf("%.10f",(x0 - x1) * 60) |> as.numeric()
  x2 <- as.integer(x2_tmp)
  x3 <- (x2_tmp - x2) * 60

  fmt_par <- paste0("%0", (ifelse(nsec==0, 2, 3) +nsec), ".", (nsec), "f")

  if(ra_or_dec == 'dec') ret <- paste0('$',
                                       sign_x,
                                       sprintf('%02d', x1), '~',
                                       sprintf('%02d', x2), '~',
                                       sprintf(fmt = fmt_par, round(x3, nsec)),
                                       '$')

  # sprintf("%07.2f", 1.2)
  if(ra_or_dec == 'ra') ret <- paste0('$',
                                      sign_x,
                                      sprintf('%02d', x1), '~',
                                      sprintf('%02d', x2), '~',
                                      sprintf(fmt = fmt_par, round(x3, nsec)),
                                      '$')

  format(ret, justify='right')
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom pillar new_pillar_shaft_simple
#' @importFrom pillar pillar_shaft
#'
#' @examples
pillar_shaft.astro_radec <- function(x, ...) {
  out <- format(x, formatter = formatter_hmsdms, color=T, nsec=1)
  new_pillar_shaft_simple(out, align = "left")
}


# Short type in tibble -------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @importFrom vctrs vec_ptype_abbr
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

