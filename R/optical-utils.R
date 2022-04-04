
#' Make guidance image
#'
#' @param image image of class matrix...
#' @param hdr header with keywords...
#' @param pixel_scale scale in units arcsec/pixel; for ps1dr2 0.25"/pixel
#' @param r_arcsec  vector of radiuses of each circle
#' @param radec astro_radec vector
#' @param colors colors of circles
#' @param label main image label
#' @param label_size size of main label
#' @param level.lo level of image grayscale; default 0.6
#' @param level.hi level of image grayscale; default 0.99
#' @param show_wcs_compass If FALSE, do not show WCS compass on image. If TRUE show the WCS compass...
#' @param show_image_scale If FALSE, do not show scale legend on image. If TRUE show the legend of scale on image...
#' @param circle_lwd linewidth of circle regions
#' @param scale_lwd linewidth of scale line
#' @param scale_txt_size text size of scale
#' @param compass_lwd linewidth of compass lines
#' @param arrow_radec
#' @param arrow_size
#' @param arrow_lwd
#' @param arrow_color
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
#' plot_guidance2(f$imDat,
#' hdr = fix_header_ps1(f$hdr),
#' radec = c(radec("J010744.2+574428"),
#'           radec("J010745.2+574432")),
#' r_arcsec = c(30, 5),
#' colors = c('blue', 'red'),
#' pixel_scale = 0.25,label = "SRGA",
#' show_image_scale = TRUE,
#' show_wcs_compass = TRUE)
#' }
plot_guidance <- function(image,
                          hdr,
                          pixel_scale = NULL,
                          r_arcsec = NULL,
                          radec = NULL,
                          colors = NULL,
                          label = NULL,
                          label_size =2,
                          circle_lwd = 1,
                          scale_lwd = 1,
                          scale_txt_size=1,
                          compass_lwd = 1,
                          level.lo=0.6,
                          level.hi = 0.99,
                          arrow_radec = NULL,
                          arrow_size = 1,
                          arrow_lwd = 1,
                          arrow_color='red',
                          show_wcs_compass = TRUE,
                          show_image_scale = TRUE) {


  magicaxis::magimage(
    image,
    asp = 1,
    flip = T,
    stretch = 'log',
    type = 'quan',
    lo = level.lo,
    hi = level.hi,
    grid.lwd = 0,
    position = 'top',
    axes=FALSE
  )


  if (!is.null(radec)){
    checkmate::assert_class(radec, 'astro_radec')

    center_opt <- celestial::radec2xy(RA = ra(radec),
                                      Dec = dec(radec),
                                      header = hdr) %>%
      as_tibble()


    if(is.null(r_arcsec) || length(r_arcsec) != length(radec))
      r_arcsec <- rep(1, length(radec))
    checkmate::assert_numeric(r_arcsec)

    if(is.null(colors) || length(colors) != length(radec))
      colors <- rep('red', 1)
    checkmate::assert_character(colors)

    lapply(1:length(radec), \(i){
      plotrix::draw.circle(center_opt$x[i],
                           center_opt$y[i],
                           radius = r_arcsec[i] / pixel_scale,
                           border = colors[i], lwd = circle_lwd)
    })
  }

  if (!is.null(arrow_radec)) {
    center_arrow <- celestial::radec2xy(RA = ra(arrow_radec),
                                      Dec = dec(arrow_radec),
                                      header = hdr) %>%
      as_tibble()

    checkmate::assert_character(arrow_color)

    lapply(1:length(arrow_radec), \(i){



      plotrix::draw.radial.line(
        start = 0,
        end = 4 / pixel_scale * arrow_size,
        center = c(center_arrow$x[i], center_arrow$y[i]),
        deg = 80,
        lwd = arrow_lwd,
        col = arrow_color
      )

      plotrix::draw.radial.line(
        start = 0,
        end = 50 / pixel_scale * arrow_size,
        center = c(center_arrow$x[i], center_arrow$y[i]),
        deg = 60,
        lwd = arrow_lwd,
        col = arrow_color
      )

      plotrix::draw.radial.line(
        start = 0,
        end = 4 / pixel_scale * arrow_size,
        center = c(center_arrow$x[i], center_arrow$y[i]),
        deg = 40,
        lwd = arrow_lwd,
        col = arrow_color
      )

    })

  }



  if(!is.null(label)){
    checkmate::assert_character(label)
    text(x = ncol(image)/2,
         y = nrow(image)-50,
         label = label,
         cex = label_size)
  }



  if(show_wcs_compass)
    magicaxis::magimageWCSCompass(header = hdr, lwd=compass_lwd)

  if(show_image_scale) {
    ang_size_arcsec <- dim(image)[1] * pixel_scale
    scale_size_arcsec <- case_when(
      ang_size_arcsec < 900 ~ 60,
      dplyr::between(ang_size_arcsec, 900, 3600) ~ 120,
      T ~ 300
    )
    xy_center <- c(dim(image)[1]*0.75, dim(image)[1]*0.042)

    plotrix::draw.radial.line(0, scale_size_arcsec / pixel_scale,
                              center = xy_center,
                              col = "#0000f5",
                              lwd = scale_lwd)

    text(x = xy_center[1] + scale_size_arcsec/pixel_scale/2,
         y  = xy_center[2] * 1.5,
         label = paste0(scale_size_arcsec/60, "'"),
         cex = scale_txt_size,
         col = "#0000f5")
  }

}





#' Fix ps1 header
#'
#' @param header FITSio --> obj$hdr
#'
#' @return fixed_header
#' @export
#'
#' @examples
#' \dontrun{
#' f <- FITSio::readFITS("file.fits")
#' fix_header(f$hdr)
#' }
fix_header_ps1 <- function(header) {

  is_header_ok <- any(grepl("CD1_1", header))

  if (is_header_ok) {
    return(header)
  } else {
    PC001001 <- header[which(header == "PC001001") + 1] %>% as.numeric()
    CDELT1 <- header[which(header == "CDELT1") + 1] %>% as.numeric()
    CD1_1 <- (PC001001 * CDELT1) %>% sprintf(fmt = "%.14e", .)


    CD1_2 <- "0"
    CD2_1 <- "0"

    PC002002 <- header[which(header == "PC002002") + 1] %>% as.numeric()
    CDELT2 <- header[which(header == "CDELT2") + 1] %>% as.numeric()
    CD2_2 <- (PC002002 * CDELT2) %>% sprintf(fmt = "%.14e", .)

    start_i <- which(header == "CDELT1") - 1
    end_i <- length(header)


    header <- c(
      header,
      "CD1_1", CD1_1,
      "CD1_2", CD1_2,
      "CD2_1", CD2_1,
      "CD2_2", CD2_2
    )

    return(header)
  }
}
