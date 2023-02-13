#' Generates vertical breaks of different sizes.
#'
#' A 'small' sized break is defined as a single \code{htmltools::tags$br}, a
#' 'medium' sized break are three \code{htmltools::tags$br}, and a 'large' break
#' are six \code{htmltools::tags$br}
#'
#' @param size a character of either 'small', 'medium', or 'large'
#'
#' @return a \code{htmltools::tagList} of \code{htmltools::tags$br()}, with the
#'   number of breaks specified in the Details section and depending on the
#'   argument to \code{size}
#' @export
break_vspace <- function(size) {
  if (size == "small") {
    return(htmltools::tagList(htmltools::tags$br()))
  } else if (size == "medium") {
    return(htmltools::tagList(htmltools::tags$br(),
                              htmltools::tags$br(),
                              htmltools::tags$br()))
  } else if (size == "large") {
    return(htmltools::tagList(htmltools::tags$br(), htmltools::tags$br(),
                              htmltools::tags$br(), htmltools::tags$br(),
                              htmltools::tags$br(), htmltools::tags$br()))
  } else {
    stop("Unknown size argument or typo!")
  }
}
#' Generate a title header or sub-title header in html-format
#'
#' Allows for underlying of text and/or emphasizing and different header sizes.
#' Currently, standard header sizes i.e. \code{htmltools::h1} -
#' \code{htmltools::h5} are supported!
#'
#' @param text the text to convert to html-format
#' @param size a number between 1 (\code{htmltools::h1}) and 5 (
#'   \code{htmltools::h5})
#' @param UNDERLINE logical; if \code{TRUE}, then \code{tags$u()} is executed on
#'   the argument \code{text} which gives an underlined text
#' @param EMPHASIZE logical; if \code{TRUE}, then \code{tags$em()} is executed
#'   on the argument \code{text} which gives an emphasized/italic text
#'
#' @return the corresponding html-format string
#' @export
add_header <- function(text, size = 1, UNDERLINE = FALSE, EMPHASIZE = FALSE) {
  # browser()
  text_out <- text
  if (EMPHASIZE) {
    text_out <- htmltools::tags$em(text_out)
  }
  if (UNDERLINE) {
    text_out <- htmltools::tags$u(text_out)
  }
  switch(size,
         `1` = {text_out <- htmltools::h1(text_out)},
         `2` = {text_out <- htmltools::h2(text_out)},
         `3` = {text_out <- htmltools::h3(text_out)},
         `4` = {text_out <- htmltools::h4(text_out)},
         `5` = {text_out <- htmltools::h5(text_out)})
  return(text_out)
}
get_segmentation_sub_settings <- function(sum_score_val_grun,
                                          sum_score_val_vide,
                                          sum_score_val_avan,
                                          type_val_grun,
                                          type_val_vide,
                                          type_val_avan) {
  list(sum_score_val_grun = sum_score_val_grun,
       sum_score_val_vide = sum_score_val_vide,
       sum_score_val_avan = sum_score_val_avan,
       type_val_grun = type_val_grun,
       type_val_vide = type_val_vide,
       type_val_avan = type_val_avan)
}
generate_plotly <- function(ggplot_to_use,
                            sttgs_legend = NULL,
                            sttgs_style = NULL) {
  plot_out <- plotly::ggplotly(ggplot_to_use)
  if (!is.null(sttgs_legend)) {
    plot_out <- plot_out %>% plotly::layout(legend = sttgs_legend)
  }
  if (!is.null(sttgs_style)) {
    plot_out <- plot_out %>% plotly::layout(style = sttgs_style)
  }
  plot_out
}
filter_for_samansi_leder <- function(ds, year, SAMANSI, LEDER) {
  data_out <- ds
  if (SAMANSI && year > 2021) {
    data_out <- data_out %>% dplyr::filter(.data$SamAnsi == 1)
  }
  if(LEDER) {
    data_out <- data_out %>% dplyr::filter(.data$leder_c == "Ja")
  }
  return(data_out)
}
