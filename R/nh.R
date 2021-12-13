idw <- function(x,y,z, x_target, y_target, beta=2){
  # Inverse distance weighting
  # https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/geostatistics/Inverse-Distance-Weighting/index.html

  distance <- function(x1, y1, x2, y2){
    sqrt((x1-x2)^2+(y1-y2)^2)
  }

  w <- distance(x, y, x_target, y_target)^(-beta)
  w_sum <- sum(w)

  wz <- sum(w*z)

  return(wz / w_sum)
}


#' Calculate the HI Column Density for a Sky Position
#'
#'
#' nH is calculated using all points (from https://vizier.u-strasbg.fr site.) within `r_arcmin`
#'
#' @param ra RA in degrees
#' @param dec DEC in degrees
#' @param r_arcmin search radius in arcmin
#' @param showInfo show additional information ? Default=false
#'
#'
#' @importFrom attempt stop_if_any
#' @export
#'
#' @return weighted nH
#'
#' @examples
#' \dontrun{
#'  xrayr::nh(213.2, -65.4, r_arcmin = 30, showInfo = T)
#' }
nh <- function(ra=NULL, dec=NULL, r_arcmin = 10, showInfo = FALSE){

  stop_if_any(c(ra, dec), is.null, "You need to specify ra/dec")

  check_internet()

  target <- paste(sprintf('%.8f', c(ra[1], dec[1])),
                  collapse = ' ')


  base_url <- 'http://vizier.u-strasbg.fr/viz-bin/asu-tsv'
  args <- list('-source' = "J/A+A/594/A116/nhi_hpx", # nh_table
               '-c' = target,
               '-c.rm' = r_arcmin,
               '-out' = '_r RAJ2000 DEJ2000 NHI', #  get all columns
               '-sort' = '_r',
               '-mime' = '|'
               )



  # arguments for query you can find on webpage:
  # https://vizier.u-strasbg.fr/vizier/vizHelp/args.htx
  vr <- httr::GET(url = 'http://vizier.u-strasbg.fr/viz-bin/asu-tsv', # Astronomical Server URL
                  query = args)

  check_status(vr)

    v <- vr$content |>
      readr::read_delim(delim = '|',
                        comment = '#',
                        show_col_types = FALSE)


    v_units <- v[1, ] |> as.character()
    names(v_units) <- colnames(v)

    # remove 2 rows with units
    suppressMessages(
      v <- v[-c(1,2), ] |>
        readr::type_convert()
    )



    nh_average <- mean(v$NHI)
    nh_weighted <- idw(x = v$RAJ2000 %>% as.numeric(),
                       y = v$DEJ2000 %>% as.numeric(),
                       z = v$NHI %>% as.numeric(),
                       x_target = as.numeric(ra),
                       y_target = as.numeric(dec))

    if(showInfo){
      print(v_units)
      print(v, n=nrow(v))
      cat('----\n')
      cat('This value is derived from the 2D HI4PI map, \n a full-sky HI survey by the HI4PI collaboration 2016,\n Astronomy & Astrophysics, 594, A116.\n')
      cat('----\n')
      cat(paste0('*RA target: ', ra, '\n'))
      cat(paste0('*DEC target: ', dec, '\n'))
      ra_range <- range(v$RAJ2000)
      dec_range <- range(v$DEJ2000)
      cat(paste0('RA range [',
                 format(ra_range[1], digits=0, nsmall=4),
                 ', ',
                 format(ra_range[2], digits=0, nsmall=4), ']\n'))
      cat(paste0('DEC range [',
                 format(dec_range[1], digits=0, nsmall=4),
                 ', ',
                 format(dec_range[2], digits=0, nsmall=4), ']\n'))
      cat(paste('Average mean nH (cm**-2):', format(nh_average, scientific=T, digits=4), '\n'))
      cat(paste('Weighted average nH (cm**-2):', format(nh_weighted, scientific=T, digits=4), '\n'))
    }

    return(nh_weighted)

}
