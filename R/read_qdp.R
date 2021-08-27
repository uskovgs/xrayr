#' Title
#'
#' @param filename name of the file
#' @param telescope_names vector of the telescope names
#' @param col_names default: c('energy', 'err_energy', 'flux_obs', 'err_obs', 'flux_model')
#'
#' @return
#' @export
#'
#' @examples
read_qdp <- function(filename,
                     telescope_names = NULL,
                     col_names = c('energy', 'err_energy', 'flux_obs', 'err_obs', 'flux_model')) {
  spec <- utils::read.table(file = filename,
                            header = F,
                            skip = 3)

  colnames(spec) <- col_names[1:ncol(spec)]

  spec$telescope <- cumsum(spec$energy == 'NO') + 1

  spec1 <- spec[spec$energy != 'NO', ]
  spec1$telescope <- as.factor(spec1$telescope)

  numeric_cols <- colnames(spec1) != 'telescope'
  spec1[numeric_cols] <- sapply(spec1[numeric_cols], as.numeric)

  if (!is.null(telescope_names)) {
    if (length(telescope_names) == length(unique(spec1$telescope))) {
      levels(spec1$telescope) <- telescope_names
    }
    else {
      warning("Number of `telescope_names` aren't equal to number of plots. Plot labels are default.")
    }
  }
  spec1
}
