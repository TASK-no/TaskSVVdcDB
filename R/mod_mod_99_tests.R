#' mod_99_tests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_99_tests_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_99_tests Server Functions
#'
#' @noRd 
mod_mod_99_tests_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_99_tests_ui("mod_99_tests_1")
    
## To be copied in the server
# mod_mod_99_tests_server("mod_99_tests_1")
