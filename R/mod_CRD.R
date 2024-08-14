#' assumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CRD_ui <- function(id) {
  fluidPage(
    
    # App title ----
    titlePanel("Analysis of Variance"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ";"),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Variáveis que são dependentes dos dados
        uiOutput("variable")
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Title
        h3("ANOVA Table"),
        
        # Output: Data file ----
        tableOutput("contents"),
        #tableOutput("aovSummary")
        verbatimTextOutput("aovSummary"),
      )
      
    )
  )
}

#' assumptionsTest Server Function
#'
#' @import ggfortify
#' @import ggplot2
#' @import lmtest
#' @import car
#' @import psych
#' @import multtest
#' 
#' @noRd 
mod_CRD_server <- function(input, output, session){
  # Define a reactive value for the data frame
  df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    #What's that? - Ensure that values are available ("truthy") before 
    #proceeding with a calculation or action.
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    #What's that? - These functions provide a mechanism for handling unusual
    #conditions, including errors and warnings.
    tryCatch(
      {
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
        #What's that? - This should be used when you want to let the user 
        #see an error message even if the default is to sanitize all errors.
        #If you have an error e and call stop(safeError(e)), then Shiny 
        #will ignore the value of getOption("shiny.sanitize.errors") and 
        #always display the error in the app itself.
      }
    )
  })
  
  # Output: Selection the Dependent and Independent Variable 
  output$variable <- renderUI({
    if(is.null(input$file1$datapath)){
      return()
    }else{
      list (radioButtons("dvar", "Please Pick The Dependent Variable", choices = names(df())),
            radioButtons("ivar", "Please Pick The Independent Variable", choices = names(df())),
            actionButton("submit", "Submit")
      )
    }
  })
  
  # Output: View Data File
  output$contents <- renderTable({
    req(df())
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  output$aovSummary = renderPrint({
    req(df())
    
    if(input$submit > 0){
      isolate(
        fit <- aov(df()[,input$dvar] ~ df()[,input$ivar], data = df())
      )
      return(summary(fit))
      #return(drop1(fit, ~ . , test = 'F'))
    }
  })
}

## To be copied in the UI
# mod_assumptionsTest_ui("assumptionsTest_ui_1")

## To be copied in the server
# callModule(mod_assumptionsTest_server, "assumptionsTest_ui_1")

