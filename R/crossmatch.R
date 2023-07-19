
#' Catalog cross-matching.
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Deprecated, use instead [angsep_left_join].
#'
#' @param x data.frame with coulumn_src and column_coord
#' @param y data.frame with coulumn_src and column_coord
#' @param column_src name of the source
#' @param column_coord skycoord column
#' @param r_arcsec maximum angular distance in arcsec (default is 30 arcsec)
#'
#' @return data.frame
#' @export
#'
#' @examples
cross_match_r <- function(x, y, column_src, column_coord, r_arcsec = 30) {
  .Deprecated(new = "angsep_join")
  # при r < 162 arcsec у источников АРТ-ХС нет ближайшего соседа

  x <- x %>%
    dplyr::select({{ column_src }}, {{ column_coord }})

  y <- y %>%
    dplyr::rename(name_cross = {{ column_src }},
           coord_cross = {{ column_coord }}) %>%
    dplyr::select(name_cross, coord_cross)

  tidyr::expand_grid(x, y, .name_repair = "unique") %>%
    dplyr::mutate(sep = separation({{ column_coord }}, coord_cross)) %>%
    dplyr::filter(sep <= r_arcsec)
}


#' Non-equi join tables by angular distance.
#'
#' @param x data.frame with `astro_radec` column.
#' @param y data.frame with `astro_radec` column.
#' @param by A character vector of variables to join by.
#' @param max_sep maximum separation in units of arcseconds. Default is 1 arcsec.
#'
#' If `NULL`, the default, `angsep_join` will perform a join, using first `astro_radec`
#' variables in `x` and `y`.
#'
#' To join by different specific variables on `x` and `y`, use a named vector. For example,
#' `by = c("coord1" = "coord2")` will cross-match `x$coord1` to `y$coord2`.
#' @param sep_col A name of the distance column. If `NULL`, the default, it drops
#' the `sep_col` column in the output data.frame.
#'
#'
#' @return A data.frame containing all rows from `x`.
#' @export
#'
angsep_left_join <- function(x, y, by = NULL, max_sep = 1, sep_col = NULL) {
  # by = NULL
  # by = skycoord
  # by = c("skycoord1" = "coord1")

  # df_srga2 <- triton::get_program_from_triton(
  #   "uskov",
  #   keyring::key_get("PLAN_SRG"),
  #   program = "SRGA2"
  # )
  #
  # df_srga <- triton::get_program_from_triton(
  #   "uskov",
  #   keyring::key_get("PLAN_SRG"),
  #   program = "SRGA"
  # )
  #
  # df_srga <- df_srga %>% select(name, type, prog, skycoord)
  # df_srga2 <- df_srga2 %>% select(name, type, prog, skycoord)
  #
  # df_srga %>%
  #   xrayr::cross_match_r(df_srga2, name, skycoord, r_arcsec = 30)
  #
  # df_srga %>%
  #   angsep_join(df_srga2, by = "skycoord", max_sep = 30, sep_col = NULL)

  if (is.null(by)) {
    i1 <- which(lapply(x, \(col) class(col)[[1]]) == "astro_radec")[1]
    i2 <- which(lapply(y, \(col) class(col)[[1]]) == "astro_radec")[1]

    if (colnames(x)[i1] == colnames(y)[i2]) {
      cat(paste0("Joining, by = c(\"", colnames(x)[i1], "\") \n"))
    } else {
      cat(paste0("Joining, by = c(\"", colnames(x)[i1], "\" = \"", colnames(y)[i2], "\")\n"))
    }

    if (length(i1) == 0L || length(i2) == 0L) {
      stop("column with `astro_radec` coordinates in the `x` or `y` data.frames  doesn't exists")
    }

  } else if (is.character(by) && length(by) == 1L && is.null(names(by))) {
    if (by %in% colnames(x) && by %in% colnames(y)) {
      i1 <- which(by == colnames(x))
      i2 <- which(by == colnames(y))
    } else {
      stop(paste0("\"", by, "\" column doesn't exists in the `x` or `y` data.frames"))
    }
  } else if (is.character(by) && length(by) == 1L && ! is.null(names(by))) {
    # by = c("skycoord1" = "coord1")
    if (names(by) %in% colnames(x) && by %in% colnames(y)) {
      i1 <- which(names(by) == colnames(x))
      i2 <- which(by == colnames(y))
    } else {
      stop(paste0("\"", names(by), "\" or \"", by, "\" doesn't exists in the `x` or `y` data.frames respectively"))
    }
  } else {
    stop("Check `by` argument")
  }

  df_out <- tidyr::expand_grid(x, y, .name_repair = janitor::make_clean_names)
  i2 <- ncol(x) + i2

  if (is.null(sep_col))
    .sep_col <- "sep"
  else
    .sep_col <- sep_col

  df_out[[.sep_col]] <- xrayr::separation(df_out[[i1]], df_out[[i2]])

  i_start <- ncol(x) + 1L
  i_stop <- ncol(df_out)
  i_na <- which(df_out[[.sep_col]] > max_sep)
  df_out[i_na, i_start:i_stop] <- NA

  df_cross <- df_out[!is.na(df_out[[.sep_col]]), ]

  if (is.null(sep_col)) df_cross[[.sep_col]] <- NULL

  dplyr::left_join(x, df_cross, by = colnames(x))
}



