StatXrayPointrange <- ggplot2::ggproto("StatXrayPointrange", ggplot2::Stat,
                                       compute_group = function(data, scales, upper_nsigma = 2, min_y = NULL) {

                                         data$y <- scales$y$trans$inverse(data$y)
                                         data$x <- scales$x$trans$inverse(data$x)



                                         positive_i <- (data$y - data$ywidth) > 0

                                         if(is.null(min_y))
                                           min_y <- min(data$y[positive_i] - data$ywidth[positive_i], na.rm = T)

                                         data$is_arrow <- !positive_i


                                         data$ymin <- ifelse(!data$is_arrow,
                                                             data$y - data$ywidth,
                                                             ifelse(upper_nsigma*data$ywidth < min_y,
                                                                    upper_nsigma*data$ywidth/5,
                                                                    min_y)
                                         )
                                         # data$ymin <- ifelse(!data$is_arrow, data$y - data$ywidth, min_y)
                                         data$ymax <- ifelse(!data$is_arrow, data$y + data$ywidth, upper_nsigma*data$ywidth)



                                         data$xmin <- data$x - data$xwidth
                                         data$xmax <- data$x + data$xwidth



                                         data$y <- scales$y$trans$transform(data$y)
                                         data$ymin <- scales$y$trans$transform(data$ymin)
                                         data$ymax <- scales$y$trans$transform(data$ymax)

                                         data$x <- scales$x$trans$transform(data$x)
                                         data$xmin <- scales$x$trans$transform(data$xmin)
                                         data$xmax <- scales$x$trans$transform(data$xmax)

                                         data


                                       },
                                       required_aes = c("x", "y", "xwidth", "ywidth")
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
#' @param upper_nsigma
#' @param ... invisible args
#' @param min_y
#'
#' @return
#' @export
#'
#' @examples
stat_xray_pointrange <- function(mapping = NULL, data = NULL, geom = "xray_pointrange",
                                 position = "identity", show.legend = NA,
                                 outliers = TRUE, inherit.aes = TRUE, upper_nsigma = 2, min_y = NULL, ...) {

  ggplot2::layer(
    stat = StatXrayPointrange,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(outliers = outliers, upper_nsigma = upper_nsigma, min_y = min_y, ...)
  )
}



GeomXrayPointrange <- ggplot2::ggproto("GeomXrayPointrange", ggplot2::Geom,
                              required_aes = c("x", "y", "xmin", 'xmax', "ymin", "ymax", 'is_arrow'),
                              default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,alpha = 1),
                              draw_key = ggplot2::draw_key_path,
                              draw_group = function(data, panel_scales, coord) {
                                ## Transform the data first
                                coords <- coord$transform(data, panel_scales)

                                coords_noarrow <- coords[!coords$is_arrow,]
                                coords_arrow <- coords[coords$is_arrow,]

                                # NoArrow grob

                                segment_arrow_horiz <- NULL
                                segment_arrow_vert <- NULL
                                segment_noarrow_horiz <- NULL
                                segment_noarrow_vert <- NULL

                                if(nrow(coords_noarrow) > 0){
                                  segment_noarrow_horiz <- grid::segmentsGrob(
                                    x0 = coords_noarrow$xmin,
                                    x1 = coords_noarrow$xmax,
                                    y0 = coords_noarrow$y,
                                    y1 = coords_noarrow$y,
                                    gp = grid::gpar(lwd = coords$size,
                                                    col = coords$colour,
                                                    alpha = coords$alpha))

                                  segment_noarrow_vert <- grid::segmentsGrob(
                                    x0 = coords_noarrow$x,
                                    x1 = coords_noarrow$x,
                                    y0 = coords_noarrow$ymin,
                                    y1 = coords_noarrow$ymax,
                                    gp = grid::gpar(lwd = coords$size,
                                                    col = coords$colour,
                                                    alpha = coords$alpha))
                                }

                                if(nrow(coords_arrow) > 0){
                                  # Arrow grob
                                  segment_arrow_horiz <- grid::segmentsGrob(
                                    x0 = coords_arrow$xmin,
                                    x1 = coords_arrow$xmax,
                                    y0 = coords_arrow$ymax,
                                    y1 = coords_arrow$ymax,
                                    gp = grid::gpar(lwd = coords$size,
                                                    col = coords$colour,
                                                    alpha = coords$alpha))

                                  segment_arrow_vert <- grid::segmentsGrob(
                                    x0 = coords_arrow$x,
                                    x1 = coords_arrow$x,
                                    y0 = coords_arrow$ymax,
                                    y1 = coords_arrow$ymin,
                                    arrow = arrow(type='closed',length = unit(0.025, 'npc')),
                                    gp = grid::gpar(lwd = coords$size,
                                                    col = coords$colour,
                                                    alpha = coords$alpha,
                                                    fill = coords$colour))
                                }

                                ##
                                grid::gTree(children = grid::gList(segment_noarrow_horiz, segment_noarrow_vert,
                                                                   segment_arrow_horiz, segment_arrow_vert))

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
#' @param upper_nsigma
#' @param ... invisible args
#' @param min_y
#'
#' @return
#' @export
#'
#' @examples
geom_xray_pointrange <- function(mapping = NULL, data = NULL, stat = "XrayPointrange",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE, upper_nsigma = 2, min_y = NULL, ...) {

  ggplot2::layer(
    geom = GeomXrayPointrange, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, upper_nsigma = upper_nsigma, min_y = min_y, ...)
  )
}

