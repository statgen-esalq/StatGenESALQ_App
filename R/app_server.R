#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_assumptionsTest_server, "assumptionsTest_ui_1")
  callModule(mod_indices_server, "indices_ui_1")
  callModule(mod_METassumptionsTest_server, "METassumptionsTest_ui_1")
  callModule(mod_METindices_server, "METindices_ui_1")
  callModule(mod_met_server, "met_ui_1")
}
