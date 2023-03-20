get_raw_data <- function(yrs) {
  raw_data_names <- c("data_raw_SVV_2021",
                      "data_raw_SVV_2022",
                      "data_raw_SVV_2023")
  id_yr <- grepl(as.character(yrs), x = raw_data_names)
  eval(parse(text = raw_data_names[id_yr]))
}
