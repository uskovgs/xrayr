#' Parse xspec output: model, fit params and conf errors
#'
#' See vignette for more details
#'
#' @param xspec_lines xspec output
#'
#' @return list with components: model, fit_params, conf_errors
#' @export
#'
#' @examples
xspec_parse_output <- function(xspec_lines) {
  list(
    model = xspec_parse_model(xspec_lines),
    fit_params = xspec_parse_params(xspec_lines),
    conf_errors = xspec_parse_conf_errors(xspec_lines)
  )
}


#' Parse xspec output: model
#'
#' @param xspec_lines xspec output
#'
#' @return character string
#' @export
#'
#' @importFrom stringi stri_match
#'
#' @examples
xspec_parse_model <- function(xspec_lines) {
  mask <- which(grepl("^==", xspec_lines) & grepl("^Model ", lead(xspec_lines, default = "")))
  model_line <- xspec_lines[mask+1]
  stringi::stri_match(model_line, regex = "Model (.*) S")[,2]
}

#' Parse xspec output: fit params
#'
#' @param xspec_lines xspec output
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_dfr set_names
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na fill
#' @import dplyr
#'
#' @examples
xspec_parse_params <- function(xspec_lines) {
  i_end <- which(grepl("^[_]", xspec_lines)) - 1
  i_start <- which(grepl("Data group", xspec_lines))[1]
  xspec_lines <- xspec_lines[i_start:i_end]
  xspec_lines %>%
    gsub("+/-  ", "+/-",. , fixed = T) %>%
    gsub("= p", "=p",. , fixed = T) %>%
    stringr::str_squish() %>%
    strsplit(split = " ") %>%
    purrr::map_dfr(\(x) {
      if (length(x) == 7) {
        tibble(
          V1 = x[1],
          V2 = x[2],
          V3 = x[3],
          V4 = x[4],
          V5 = x[5],
          V6 = x[6],
          V7 = x[7]
        )
      } else if (length(x) == 6) {
        tibble(
          V1 = x[1],
          V2 = x[2],
          V3 = x[3],
          V4 = x[4],
          V5 = '',
          V6 = x[5],
          V7 = x[6]
        )
      } else {
        tibble(data_group = x[3])
      }
    }) %>%
    tidyr::fill(data_group) %>%
    tidyr::drop_na(V1) %>%
    purrr::set_names(c(
      "data_group",
      "par",
      "n_comp",
      "comp",
      "par_comp",
      "units",
      "value",
      "error"
    )) %>%
    dplyr::mutate(
      error = gsub(r"(+/-)", "", error),
      is_frozen = ifelse(error == 'frozen', '+', ''),
      tie = ifelse(grepl("=p", error), error, ""),
      error = ifelse(grepl("^(frozen|=p)", error), "", error)
    ) %>%
    readr::type_convert(col_types = "fiicccnncc")
}

#' Parse xspec output: confidence errors
#'
#' @param xspec_lines xspec output
#'
#' @return tibble
#' @export
#'
#' @examples
xspec_parse_conf_errors <- function(xspec_lines) {
  i_start <- which(grepl("Confidence Range", xspec_lines))[1] + 1
  xspec_lines[i_start:length(xspec_lines)] %>%
    readr::read_table(col_names = c("par","error_lo","error_hi","diff_err"))
}


