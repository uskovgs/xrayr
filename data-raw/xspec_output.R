## code to prepare `xspec_output` dataset goes here


xspec_output <- readr::read_lines("~/rrr/RDataScience/xspec_helpers/resources/original/xspec_output_fit.txt")
usethis::use_data(xspec_output, overwrite = TRUE)
