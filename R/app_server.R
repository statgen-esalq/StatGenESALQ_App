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
  callModule(mod_METindices_server, "METindices_ui_1")
  callModule(mod_met_server, "met_ui_1")
  callModule(mod_MixedModel_server, "MixedModel_ui_1")
  callModule(mod_dic_server, "dic_ui_1")
  callModule(mod_splitPlot_server, "splitPlot_ui_1")
  callModule(mod_dbc_server, "dbc_ui_1")
}