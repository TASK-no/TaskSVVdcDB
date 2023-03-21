get_raw_data <- function(yrs) {
  raw_data_names <- c("data_raw_SVV_2021",
                      "data_raw_SVV_2022",
                      "data_raw_SVV_2023")
  id_yr <- grepl(as.character(yrs), x = raw_data_names)
  eval(parse(text = raw_data_names[id_yr]))
}
modal_download <- function(ns, session_passed) {
  shiny.semantic::create_modal(
    shiny.semantic::modal(
      shiny.semantic::action_button(ns("cancel"), "Avbryt/lukk."),
      id = ns("download_modal"),
      header = htmltools::h3("Download the file?", style = "text-align: center;"),
      footer = htmltools::tagList(
        shiny::downloadLink(
          outputId = ns("dataDownload"),
          label = "Yes"
        )
      ),
      class = "tiny"
    ),
    show = TRUE,
    session = session_passed
  )
}
download_handler_filename_01 <- function(input) {
  function() {
    extension <- switch(input$data_format,
                        "csv" = ".csv",
                        "xls" = ".xlsx",
                        "spss" = ".sav")
    if (input[["data_type"]] == "Rådata") {
      return(paste0("data_raw_SVV_", input[["year_taken"]], extension))
    }
  }
}
download_handler_content_01 <- function(file, input, data_set){
  function(file) {
    if (input$data_format == "csv") {
      write.csv(data_set(), file,
                row.names = FALSE, fileEncoding =  "utf-16")
    } else if(input$data_format == "xls") {
      xlsx::write.xlsx(data_set(), file, row.names = FALSE)
    } else if(input$data_format == "spss") {
      haven::write_sav(data_set(), file)
    }
  }
}
