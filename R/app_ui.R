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
      titlePanel(title = div(img(src = "www/logo.png",
                                 height = "10%",
                                 width = "10%",
                                 align = "right"),
                             "SUB_TITLE_OF_PROJECT")),
      sidebar_layout(
        sidebar_panel = shiny.semantic::sidebar_panel(
          mod_seg_competence_ui("segmentation_inputs"),
          mod_seg_q_ui("segmentation_inputs", num_q = 16,
                       title_text = "Kommunikasjon og samhandling"),
          mod_seg_q_ui("segmentation_inputs", num_q = 17,
                       title_text = "Informasjonssikkerhet og personvern"),
          mod_seg_q_ui("segmentation_inputs", num_q = 14,
                       title_text = "Bruk av programvare"),
          mod_seg_q_ui("segmentation_inputs", num_q = 19,
                       title_text = "Bruk av teknologi")
          # conditionalPanel("input.analysis_all == 'tab_1'",
          #                  auth0::logoutButton(label = "Log out",
          #                                      id = "my_logout")),
          # conditionalPanel("input.analysis_all == 'tab_2'",
          #                  auth0::logoutButton(label = "Log out",
          #                                      id = "my_logout")
        ),
        main_panel = shiny.semantic::main_panel(
          shiny.semantic::tabset(
            tabs =
              list(
                list(menu = "First tab",
                     content = div(
                       mod_break_vspace("small")#,
                       # reactable::reactableOutput("table_2021")
                       ),
                     id = "tab_1"),
                list(menu = "Second tab",
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
          )
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
      app_title = "PROJECT_NAME"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
