
StatXrayStepCount <- ggplot2::ggproto("StatXrayStepCount", ggplot2::Stat,
                             compute_group = function(data, scales) {



                               data$x <- scales$x$trans$inverse(data$x)


                               data$xmin <- data$x - data$xwidth
                               data$xmax <- data$x + data$xwidth

                               data$x <- scales$x$trans$transform(data$x)
                               data$xmin <- scales$x$trans$transform(data$xmin)
                               data$xmax <- scales$x$trans$transform(data$xmax)

                               data
                             },
                             required_aes = c("x", "y", "xwidth")
)

#' Title
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param show.legend
#' @param outliers
#' @param inherit.aes
#' @param ... invisible args
#'
#' @return
#' @export
#'
#' @examples
stat_xray_stepcount <- function(mapping = NULL, data = NULL, geom = "xray_stepcount",
                                position = "identity", show.legend = NA,
                                outliers = TRUE, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    stat = StatXray,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, ...)
  )
}


GeomXrayStepCount <- ggplot2::ggproto("GeomXrayStepCount", ggplot2::Geom,
                             required_aes = c("x", "y", "xmin", 'xmax'),
                             default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,alpha = 1),
                             draw_key = ggplot2::draw_key_path,
                             draw_group = function(data, panel_scales, coord) {
                               ## Transform the data first
                               coords <- coord$transform(data, panel_scales)
                               n = nrow(data)

                               ## Construct a grid grob
                               segment_horizontal <- grid::segmentsGrob(
                                 x0 = coords$xmin,
                                 x1 = coords$xmax,
                                 y0 = coords$y,
                                 y1 = coords$y,
                                 gp = grid::gpar(lwd = coords$size,
                                                 col = coords$colour,
                                                 alpha = coords$alpha))

                               if (n > 1){
                                 diff_y <- diff(coords$y)
                                 coords <- coords[1:(n-1),]
                                 coords <- transform(coords, ynext = y + diff_y)
                                 segment_vertical <- grid::segmentsGrob(
                                   x0 = coords$xmax,
                                   x1 = coords$xmax,
                                   y0 = coords$y,
                                   y1 = coords$ynext,
                                   gp = grid::gpar(lwd = coords$size,
                                                   col = coords$colour,
                                                   alpha = coords$alpha))
                               } else {
                                 segment_vertical <- NULL
                               }


                               grid::gTree(children = grid::gList(segment_horizontal, segment_vertical))

                             })

#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ... invisible args
#'
#' @return
#' @export
#'
#' @examples
geom_xray_stepcount <- function(mapping = NULL, data = NULL, stat = "XrayStepCount",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomXrayStepCount, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
