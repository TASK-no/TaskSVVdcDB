#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny.semantic::semanticPage(
      titlePanel(title = div(img(src = "www/logoTASK.png",
                                 height = "10%",
                                 width = "10%",
                                 align = "right"),
                             add_header("Statens Vegvesen - Digital Kompetanse",
                                        size = 1))),
      shiny.semantic::sidebar_layout(
        sidebar_panel = shiny.semantic::sidebar_panel(
          # shiny::tagList(
            add_header("Segmenteringsanalyse:", size = 2, UNDERLINE = TRUE, EMPHASIZE = TRUE),
            mod_seg_competence_ui("seg_inputs"),
            break_vspace("small"),
            mod_seg_q_ui("seg_inputs", num_q = 16,
                         title_text = "Kommunikasjon og samhandling",
                         settings_seg$q16),
            break_vspace("small"),
            mod_seg_q_ui("seg_inputs", num_q = 17,
                         title_text = "Informasjonssikkerhet og personvern",
                         settings_seg$q17),
            break_vspace("small"),
            mod_seg_q_ui("seg_inputs", num_q = 14,
                         title_text = "Bruk av programvare",
                         settings_seg$q14),
            break_vspace("small"),
            mod_seg_q_ui("seg_inputs", num_q = 19,
                         title_text = "Bruk av teknologi",
                         settings_seg$q19),
            break_vspace("small"),
            auth0::logoutButton(label = "Logg ut", id = "my_logout")
          # ),
          ,
          width = 2
        ),
        main_panel = shiny.semantic::main_panel(
          break_vspace("small"),
          add_header("Kjennskap til 'Digitalt på vei'",
                     size = 3, EMPHASIZE = TRUE, UNDERLINE = TRUE),
          mod_cat_choices_ui("cat_inputs"),
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
                       break_vspace("medium"),
                       mod_logistic_regression_specs_01_ui("seg_inputs"),
                       break_vspace("small"),
                       mod_logistic_summary_ou("logistic_reg_01",
                                               "logistic_out_01"),
                       # break_vspace("medium"),
                       # mod_logistic_regression_specs_02_ui("logistic_prd_01",
                       #                                     200)
                     ),
                     id = "tab_2")
              ),
            active = "tab_1",
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
