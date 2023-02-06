mod_break_vspace <- function(size) {
  if (size == "small") {
    return(tagList(htmltools::tags$br()))
  } else if (size == "medium") {
    return(tagList(htmltools::tags$br(),
                   htmltools::tags$br(),
                   htmltools::tags$br()))
  } else if (size == "large") {
    return(tagList(htmltools::tags$br(), htmltools::tags$br(),
                   htmltools::tags$br(), htmltools::tags$br(),
                   htmltools::tags$br(), htmltools::tags$br()))
  } else {
    stop("Unknown size argument or typo!")
  }
}
