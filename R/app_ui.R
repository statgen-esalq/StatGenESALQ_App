#' The application User-Interface 
#'  
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinymanager
#' @import markdown
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = div("DesignGen", style="text-align: middle;")),
      dashboardSidebar(
        sidebarMenu(
          menuItem("About", tabName = "about", icon = icon("bookmark")),
          menuItem("Single environment", tabName = "single", 
                   menuSubItem("Assumptions test", tabName = "assumptionsTest", icon = icon("minus")),
                   menuSubItem("Selection indices", tabName = "indices", icon = icon("minus"))
          ),
          menuItem("Multi environment", tabName = "multi", 
                   #menuSubItem("Assumptions test", tabName = "METassumptionsTest", icon = icon("crosshairs")),
                   menuSubItem("Selection indices", tabName = "METindices", icon = icon("minus")),
                   menuSubItem("Stability analysis", tabName = "MET_metan", icon = icon("minus"))
          ),
          # New menuItem 
          menuItem("Fixed Models", tabName = "fixed", 
                   menuSubItem("Completely randomized design", tabName = "DIC", icon = icon("minus")),
                   menuSubItem("Randomized block design", tabName = "DBC", icon = icon("minus")),
                   menuSubItem("Split plot design", tabName = "parc_sub", icon = icon("minus"))
          ),
          menuItem("Mixed Models", tabName = "mixedModel"
                   # , icon = icon("minus")
          ),
          tags$li(class = "dropdown",
                  tags$a(href="https://statgen-esalq.github.io/", target="_blank", 
                         tags$img(height = "60px", alt="Logo", src="www/logo.png")
                  ))
        )
      ),
      dashboardBody(
        # Lab colors
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #003350;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #003350;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #003350;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #003350;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #cc662f;
                              color: #000000;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #003350;
         }
                              
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#cc662f
                            }
        
        .box.box-solid.box-primary{
        border-bottom-color:#cc662f;
        border-left-color:#cc662f;
        border-right-color:#cc662f;
        border-top-color:#cc662f;
        }
        
        .box.box-solid.box-info>.box-header {
        color:#fff;
        background:#003350
        }
        
        .box.box-solid.box-info{
        border-bottom-color:#003350;
        border-left-color:#003350;
        border-right-color:#003350;
        border-top-color:#003350;
        }
        
        .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 34px;
        }
                              '))),
        
        
        tabItems(
          # First tab content
          tabItem(tabName = "about",
                  includeMarkdown(system.file("ext", "about.Rmd", package = "StatGenESALQ"))
          ),
          tabItem(tabName = "assumptionsTest",
                  mod_assumptionsTest_ui("assumptionsTest_ui_1")
          ),
          tabItem(tabName = "indices",
                  mod_indices_ui("indices_ui_1")
          ),
          # tabItem(tabName = "METassumptionsTest",
          #         mod_METassumptionsTest_ui("METassumptionsTest_ui_1")
          # ),
          tabItem(tabName = "METindices",
                  mod_METindices_ui("METindices_ui_1")
          ),
          tabItem(tabName = "MET_metan",
                  mod_met_ui("met_ui_1")
          ),
          tabItem(tabName = "mixedModel",
                  mod_MixedModel_ui("MixedModel_ui_1")
          ),
          # Third tab
          tabItem(tabName = "DIC",
                  mod_dic_ui("dic_ui_1")
          ),
          tabItem(tabName = "parc_sub",
                  mod_splitPlot_ui("splitPlot_ui_1")
          ),
          tabItem(tabName = "DBC",
                  mod_dbc_ui("dbc_ui_1")
                  
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'STATgen'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}