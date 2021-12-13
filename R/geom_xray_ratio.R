

StatXrayRatio <- ggplot2::ggproto("StatXrayRatio", ggplot2::Stat,
                         compute_group = function(data, scales) {

                           # data$y <- scales$y$trans$inverse(data$y)
                           data$x <- scales$x$trans$inverse(data$x)



                           # positive_i <- (data$y - data$ywidth) > 0
                           # min_y <- min(data$y[positive_i] - data$ywidth[positive_i], na.rm = T)
                           # data$is_arrow <- !positive_i
                           # data$ymin <- ifelse(!data$is_arrow, data$y - data$ywidth, min_y)
                           # data$ymax <- ifelse(!data$is_arrow, data$y + data$ywidth, upper_nsigma*data$ywidth)

                           data$xmin <- data$x - data$xwidth
                           data$xmax <- data$x + data$xwidth

                           data$y <- data$obs / data$model
                           data$ymin <- (data$obs - data$obs_err) / data$model
                           data$ymax <- (data$obs + data$obs_err) / data$model

                           warning('***Warning: do not scale y axis, `xray_ratio`')



                           # data$y <- scales$y$trans$transform(data$y)
                           # data$ymin <- scales$y$trans$transform(data$ymin)
                           # data$ymax <- scales$y$trans$transform(data$ymax)

                           data$x <- scales$x$trans$transform(data$x)
                           data$xmin <- scales$x$trans$transform(data$xmin)
                           data$xmax <- scales$x$trans$transform(data$xmax)


                           data
                         },
                         required_aes = c("x", "xwidth", "obs", "obs_err", 'model')
)
#' Title
#'
#' @param mapping mapping
#' @param data data
#' @param geom geom
#' @param position position
#' @param show.legend show.legend
#' @param outliers outliers
#' @param inherit.aes ingherit.aes
#' @param ... invisible args
#'
#' @return
#' @export
#'
#' @examples
stat_xray_ratio <- function(mapping = NULL, data = NULL, geom = "xray_ratio",
                            position = "identity", show.legend = NA,
                            outliers = TRUE, inherit.aes = TRUE,  ...) {

  ggplot2::layer(
    stat = StatXrayRatio,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, ...)
  )
}



GeomXrayRatio <- ggplot2::ggproto("GeomXrayRatio", ggplot2::Geom,
                         required_aes = c("x", "y", "xmin", 'xmax', "ymin", "ymax"),
                         default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,alpha = 1),
                         draw_key = ggplot2::draw_key_path,
                         draw_group = function(data, panel_scales, coord) {
                           ## Transform the data first
                           coords <- coord$transform(data, panel_scales)

                           segment_horiz <- grid::segmentsGrob(
                             x0 = coords$xmin,
                             x1 = coords$xmax,
                             y0 = coords$y,
                             y1 = coords$y,
                             gp = grid::gpar(lwd = coords$size,
                                             col = coords$colour,
                                             alpha = coords$alpha))
                           segment_vert <- grid::segmentsGrob(
                             x0 = coords$x,
                             x1 = coords$x,
                             y0 = coords$ymin,
                             y1 = coords$ymax,
                             gp = grid::gpar(lwd = coords$size,
                                             col = coords$colour,
                                             alpha = coords$alpha))


                           ##
                           grid::gTree(children = grid::gList(segment_horiz, segment_vert))

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
geom_xray_ratio <- function(mapping = NULL, data = NULL, stat = "XrayRatio",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomXrayRatio, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
