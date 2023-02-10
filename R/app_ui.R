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
                             htmltools::h1("Statens Vegvesen - Digital Kompetanse"))),
      shiny.semantic::sidebar_layout(
        sidebar_panel = shiny.semantic::sidebar_panel(
          # shiny::tagList(
            htmltools::h2(tags$u(tags$em("Segmenteringsanalyse:"))),
            mod_seg_competence_ui("segmentation_inputs"),
            mod_break_vspace("small"),
            mod_seg_q_ui("segmentation_inputs", num_q = 16,
                         title_text = "Kommunikasjon og samhandling",
                         settings_seg$q16),
            mod_break_vspace("small"),
            mod_seg_q_ui("segmentation_inputs", num_q = 17,
                         title_text = "Informasjonssikkerhet og personvern",
                         settings_seg$q17),
            mod_break_vspace("small"),
            mod_seg_q_ui("segmentation_inputs", num_q = 14,
                         title_text = "Bruk av programvare",
                         settings_seg$q14),
            mod_break_vspace("small"),
            mod_seg_q_ui("segmentation_inputs", num_q = 19,
                         title_text = "Bruk av teknologi",
                         settings_seg$q19),
            mod_break_vspace("small"),
            auth0::logoutButton(label = "Logg ut", id = "my_logout")
          # ),
          ,
          width = 2
        ),
        main_panel = shiny.semantic::main_panel(
          # shiny.semantic::flow_layout(
              # mod_seg_q_ui("segmentation_inputs", num_q = 16,
              #              title_text = "Kommunikasjon og samhandling",
              #              settings_seg$q16),
              # mod_seg_q_ui("segmentation_inputs", num_q = 17,
              #              title_text = "Informasjonssikkerhet og personvern",
              #              settings_seg$q17),
              # mod_seg_q_ui("segmentation_inputs", num_q = 14,
              #              title_text = "Bruk av programvare",
              #              settings_seg$q14),
              # mod_seg_q_ui("segmentation_inputs", num_q = 19,
              #              title_text = "Bruk av teknologi",
              #              settings_seg$q19),
            # min_cell_width = "75px",
            # max_cell_width = "250px"
          # ),
          shiny.semantic::tabset(
            tabs =
              list(
                list(menu = "Preliminaer dataanalyse",
                     content = div(
                       mod_break_vspace("small"),
                       mod_plot_overall_ui("plot01"),
                       mod_break_vspace("small"),
                       mod_plot_subplot_ui("plot02", 2021, "pie_2021"),
                       mod_break_vspace("small"),
                       mod_plot_subplot_ui("plot03", 2022, "pie_2022")
                       # mod_plot_overall_ui("plot01")
                       # mod_plot_overall_ui("plot01")
                       # shiny.semantic::flow_layout(
                       #   htmltools::tagList(
                       #     mod_plot_subplot_ui("plot02", 2021, "pie_2021")
                       #     # mod_plot_overall_ui("plot01")
                       #   ),
                       #   htmltools::tagList(
                       #     mod_plot_subplot_ui("plot03", 2022, "pie_2022")
                       #     # mod_plot_overall_ui("plot01")
                       #   ),
                       #   min_cell_width = "450px",
                       #   max_cell_width = "auto"
                       # )
                       # reactable::reactableOutput("table_2021")
                       ),
                     id = "tab_1"),
                list(menu = "Binaer klassifisering/ logistisk regresjon",
                     content = div(
                       shiny.semantic::flow_layout(
                         htmltools::tagList(
                           h3(tags$u(tags$em("Some header:")))
                         ),
                         htmltools::tagList(
                           h3(tags$u(tags$em("Some header:")))
                           ),
                         min_cell_width = "450px",
                         max_cell_width = "1fr"
                       )
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
