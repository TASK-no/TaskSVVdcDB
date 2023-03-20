#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  title_used <- shiny::div(shiny::img(src = "www/logoTASK.png",
                                      height = "10%",
                                      width = "10%",
                                      align = "right"),
                           add_header("Statens Vegvesen - Digital Kompetanse",
                                      size = 1))
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny.semantic::semanticPage(
      shiny::titlePanel(title = title_used),
      shiny.semantic::sidebar_layout(
        sidebar_panel = shiny.semantic::sidebar_panel(
          mod_01_seg_all_ui("segmentation"),
          width = 2
        ),
        main_panel = shiny.semantic::main_panel(
          break_vspace("small"),
          add_header("Kjennskap til 'Digitalt på vei'",
                     size = 3, EMPHASIZE = TRUE, UNDERLINE = TRUE),
          mod_cat_choices_A_ui("cat_inputs"),
          break_vspace("small"),
          mod_cat_choices_B_ui("cat_inputs"),
          break_vspace("small"),
          shiny.semantic::tabset(
            tabs =
              list(
                list(menu = "Preliminaer dataanalyse",
                     content = htmltools::div(
                       break_vspace("small"),
                       mod_plot_overall_ui("plot_01"),
                       break_vspace("small"),
                       mod_plot_subplot_ui("plot_02", "sub_01"),
                       break_vspace("small"),
                       mod_plot_subplot_ui("plot_03", "sub_02")
                       ),
                     id = "tab_1"),
                list(menu = "Binaer klassifisering / logistisk regresjon",
                     content = htmltools::div(
                       break_vspace("small"),
                       add_header("Legg til logistisk regresjonsspesifikasjon",
                                  size = 3, UNDERLINE = TRUE, EMPHASIZE = TRUE),
                       break_vspace("small"),
                       mod_logistic_regression_specs_01_A_ui("logistic_reg_01"),
                       break_vspace("small"),
                       mod_logistic_regression_specs_01_B_ui("logistic_reg_01"),
                       break_vspace("medium"),
                       mod_logistic_summary_ou("logistic_reg_01",
                                               "logistic_out_01"),
                       break_vspace("medium"),
                       mod_logistic_regression_specs_02_ui("logistic_prd_01",
                                                           200)
                     ),
                     id = "tab_2"),
                list(menu = "Datanedlasting",
                     content = htmltools::div(
                       break_vspace("small"),
                       add_header("Velg type data og datainnstillinger for nedlasting",
                                  size = 3, UNDERLINE = TRUE, EMPHASIZE = TRUE),
                       mod_08_data_download_ui("data_download"),
                       mod_08_data_table_ui("data_download")
                     ),
                     id = "tab_3")
              ),
            active = "tab_2",
            id = "tab_id"
          ),
          width = 5
        )
      )
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = " SVV_DC"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
