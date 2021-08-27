#' Title
#'
#' @param is_transparent transparent background
#' @param ... invisible args
#'
#' @return
#' @export
#'
#' @examples
theme_xray_plot <- function(is_transparent = T, ...){

  if (is_transparent)
    ggplot2::theme_bw() %+replace% ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                legend.position = 'top',
                                legend.title = ggplot2::element_blank(),
                                plot.title.position = 'plot',
                                panel.background = ggplot2::element_blank(),
                                legend.background = ggplot2::element_blank(),
                                plot.background = ggplot2::element_blank(), ...)
  else{
    ggplot2::theme_bw() %+replace% ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                legend.position = 'top',
                                plot.title.position = 'plot',
                                legend.title = ggplot2::element_blank(), ...)
  }
}
