# Function to recode `leder_c`
#' Read and Distinct Data
#'
#' Reads data from a .sav file and applies distinct operation to remove
#' duplicates.
#'
#' @param path The path to the directory containing the data file.
#' @param file The filename of the data file to read.
#' @return A tibble with distinct rows from the read .sav file.
read_and_distinct <- function(path, file) {
  haven::read_sav(file.path(path, file)) %>% dplyr::distinct()
}