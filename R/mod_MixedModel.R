#' MixedModel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# ui part ----
mod_MixedModel_ui <- function(id){ 
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12, 
                 p(HTML("On this page, you can run analyses with mixed models. During this process, you can filter your data before the analysis and view previous results or export them afterward. 
                 Please keep the following points in mind: 
                 <ul>
                 <li>Follow each step and press the button at the end of each one.</li>
                 <li>If you select something incorrectly or wish to make a change, you can return to the specific section/step you need to modify and then proceed with the subsequent steps.</li>
                 <li>The 'sommer' package is used to perform the analysis, so its syntax should be considered.</li>
                        </ul>")),
             ),
             
             # Input the file
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("If you don't have any data, you can download an example input file here to see how the app works."),
                 downloadButton(ns("data_example")), hr(),
                 p("Upload your file here:"),
                 fileInput(ns("data_input"), label = h6("File: .csv .xls .txt"), multiple = F),
                 p("If you don't have a file to upload, you can still explore the app's features using our example file.
                   The example file will be automatically uploaded, simply press the 'Load file' button to proceed."),
                 
                 # Input Control
                 hr(),
                 box(width = 4,
                     radioButtons(ns("data_sep"), label = p("Choose the separator:"),
                                  choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                  selected = ",") 
                 ), 
                 box(width = 8, 
                     uiOutput(ns("data_dynamic_view")),
                 ),
                 
                 # Read the file
                 hr(),
                 actionButton(ns("data_load"), "Load file", icon("file")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             # Data Filtering
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary", title = "Data Filtering",
                 box(width = 12,
                     radioButtons(ns("filter_choice"), label = p("Do you need to filter your data?"),
                                  choices = c("Yes", "No"),
                                  selected = "No"),
                     p("If you don't need to filter your data, just press the 'Data Filters' button and continue with the next steps.")
                 ),
                 
                 uiOutput(ns("filter_dynamic_factor")),
                 br(),
                 uiOutput(ns("filter_dynamic_button")),
                 br(),
                 uiOutput(ns("filter_dynamic_level")),
                 
                 actionButton(ns("filter_ready"), "Filter Data", icon("filter")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             # Choose Parameters
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select parameters",
                 box(width = 4,
                     radioButtons(ns("trait"), label = p("Choose the trait to be evaluated:"),
                                  choices = "Press 'Filter Data' button to update",
                                  selected = "Press 'Filter Data' button to update"),
                 ),
                 box(width = 4,
                     radioButtons(ns("fixed_ef"), label = p("Choose the fixed effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 box(width = 4,
                     radioButtons(ns("random_ef"), label = p("Choose the random effect to be evaluated:"),
                                  choices = "Press 'Data Filters' button to update",
                                  selected = "Press 'Data Filters' button to update")
                 ),
                 hr(),
                 actionButton(ns("parameter_choice"), "Model Parameters", icon("check")),
                 br(),
                 h6("Click here to proceed to the next step")
             ),
             
             
             # Define the model
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Define the model",
                 p("If you want to consider a pedigree matrix, please upload an .csv file as the example:"),
                 downloadButton(ns("pedigree_example")),
                 hr(),
                 fileInput("pedigree", label = p("Pedigree matrix")),
                 hr(),
                 
                 p("Define the model expression bellow. Here we used 'sommer' package to perform the analysis, so consider its syntax.
                 If it has been uploaded above, you can include it in the model using the symbol A, according to this package syntax."),
                 p(HTML("For more information, consult the
                        <a href= 'https://www.rdocumentation.org/packages/sommer/versions/4.1.2/topics/mmer' target = '_blank' > R documentation </a>
                        and the tutorials by 'sommer' package. To access theses tutorials, run the following code in the R console:")),
                 verbatimTextOutput(ns("sommer_tutorial")), 
                 hr(),
                 p("When you call the variables in your model, use the same column names present in your data."),
                 textInput(ns("fixed"), label = p("Fixed:"), value = "Peso ~ Corte + Corte:Bloco"),
                 textInput(ns("random"), label = p("Random:"), value = "~ Genotipo + Corte:Genotipo"),
                 textInput(ns("rcov"), label = p("rcov:"), value = "~ units"), hr(),
                 
                 actionButton(ns("analysis_run"), "Run analysis",icon("refresh")), 
                 br(),
                 h6("Click here and expand the windows above to access the results")
             ), hr(),
             
             # Results
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Results",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Variance components",
                     DT::dataTableOutput(ns("varcomp_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "AIC and BIC",
                     DT::dataTableOutput(ns("aic_bic_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "BLUPs",
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Table Visualization",
                         DT::dataTableOutput(ns("blups_table_out")),
                     ),
                     box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Graph Visualization",
                         plotOutput(ns("blups_graph_out")),
                     ),
                 ),
                 # Download
                 p("Click here to download the complete analysis data in '.RData' format.  
                   Once you import this into R or RStudio, an object named 'mixedmodel' will be created, enabling you to work with it."),
                 downloadButton(ns('download_rdata'), "Download .RData", class = "butt") 
             )
    )
  )
}

#' MixedModel Server Function
#'
#' @import sommer
#' 
#' @noRd 
#' 
# server part ----
mod_MixedModel_server <- function(input, output, session){
  ns <- session$ns
  
  # Download input
  output$data_example <- downloadHandler(
    filename =  function() {
      paste("example_data.csv")
    },
    # Content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_split.csv", package = "StatGenESALQ"))
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  # Observe the file input and display the data
  observeEvent(input$data_sep, {
    output$data_dynamic_view <- renderUI({
      if (is.null(input$data_input)) {
        output$dataview <- renderUI({
          return(p("Please upload your file to update this section."))
        })
      } else {
        dat <- read.csv(input$data_input$datapath, sep = input$data_sep)
        output$dataview <- renderTable({
          return(head(dat))
        })
      }
    })
  })
  
  # Download example pedigree data
  output$pedigree_example <- downloadHandler(
    filename =  function() {
      paste("pedigree.csv")
    },
    # Content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_pedigree.csv", package = "StatGenESALQ"), row.names = 1, header = T)
      write.csv(dat, file = file)
    }
  )
  
  # Data Loading
  button1 <- eventReactive(input$data_load, {
    if (is.null(input$data_input$datapath)) {
      dat <- read.csv(system.file("ext","example_inputs/example_split.csv", package = "StatGenESALQ"))
    } else {
      dat <- read.csv(input$data_input$datapath, sep = input$data_sep)
    }
    dat
  })
  
  # Dynamic UI for filtering data
  observeEvent(input$data_load, {
    output$filter_dynamic_factor <- renderUI({
      req(input$filter_choice == "Yes")
      data_names <- colnames(button1())
      
      box(width = 12,
          checkboxGroupInput(ns("factor"), label = p("Choose the factors to be filtered:"),
                             choices = data_names,
                             selected = data_names[1])
      )
    })
    showNotification("Data loaded")
  })
  
  observeEvent(input$data_load, {
    output$filter_dynamic_button <- renderUI({
      req(input$filter_choice == "Yes")
      dat <- button1()
      
      actionButton(ns("filter_in_process"), "Select levels", icon("plus"))
    })
  })
  
  # Reactive filter data
  observeEvent(input$data_load, {
    output$filter_dynamic_level <- renderUI({
      req(input$filter_choice == "Yes")
      
      num <- length(input$factor)
      col_names <- input$factor
      
      lapply(seq_len(num), function(i) {
        box(width = 12,
            checkboxGroupInput(ns(paste0("filter", i)),
                               label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                               choices = "Press 'Select levels' button to update")
        )
      })
    })
  })
  
  observeEvent(input$filter_in_process, {
    req(input$filter_choice == "Yes")
    
    dat <- button1()
    if (length(input$factor) > 0) {
      n <- length(input$factor)
      for (i in 1:n) {
        dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])
      }
    }
    
    num <- length(input$factor)
    col_names <- input$factor
    
    lapply(seq_len(num), function(i) {
      if(is.factor(dat[[input$factor[i]]])) {
        box(width = 12,
            updateCheckboxGroupInput(session, paste0("filter", i),
                                     label = paste0("Choose the levels from '", col_names[i], "' to be filtered:"),
                                     choices = unique(dat[[input$factor[i]]]))
        )
      }
    })
  })
  
  # Data Filtering
  button2 <- eventReactive(input$filter_ready, {
    dat <- button1()
    if (length(input$factor) > 0) {
      n <- length(input$factor)
      for (i in 1:n) {
        dat[[input$factor[i]]] <- as.factor(dat[[input$factor[i]]])
      }
    }
    
    num <- length(input$factor)
    col_names <- input$factor
    
    for (i in 1:num) {
      dat <- dat %>%
        filter(dat[[input$factor[i]]] %in% c(input[[paste0("filter", i)]])) %>%
        droplevels()
    }
    dat
  })
  
  # Update choices for analysis
  observeEvent(input$filter_ready, {
    data_names <- colnames(button1())
    
    updateRadioButtons(session, "trait",
                       label="Choose the trait to be evaluated:",
                       choices = data_names,
                       selected = unlist(data_names)[1])
    
    updateRadioButtons(session, "fixed_ef",
                       label="Choose the fixed effect to be evaluated:",
                       choices = data_names)
    
    updateRadioButtons(session, "random_ef",
                       label="Choose the random effect to be evaluated:",
                       choices = data_names)
  })
  
  observeEvent(input$parameter_choice, {
    showNotification("Parameters chose")
  })
  
  # Analysis function
  button3 <- eventReactive(input$analysis_run, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      req(input$parameter_choice)
      dat <- button2()
      
      if (ncol(dat) > 0) {
        n <- ncol(dat)
        for (i in 1:n) {
          if (colnames(dat)[i] == input$trait) {
            dat[, i] <- as.double(dat[, i])
          } else {
            dat[, i] <- as.factor(dat[, i])
          }
        }
      } 
      if(!is.null(input$pedigree)) A <- read.csv(input$pedigree$datapath, row.names = 1, header = T)
      
      # Input the model
      mod <- mmer(fixed = as.formula(input$fixed), 
                  random = as.formula(input$random), 
                  rcov = as.formula(input$rcov),
                  data = dat)
      
      # Results
      summary_mod <- summary(mod)
      aic_bic <- data.frame(AIC = mod$AIC, BIC = mod$BIC)
      BLUPs <- data.frame(ID = levels(dat[[input$random_ef]]), BLUPs = mod$U[[input$random_ef]])
      rownames(BLUPs) <- NULL
      
      incProgress(0.25, detail = paste("Doing part", 2))
      list(mod, summary_mod, aic_bic, BLUPs)
    })
  })
  
  output$sommer_tutorial <- renderPrint({
    cat("> vignette('v1.sommer.quick.start')",
        "> vignette('v2.sommer.changes.and.faqs')",
        "> vignette('v3.sommer.qg')",
        "> vignette('v4.sommer.gxe')", sep = "\n")
  })
  
  # Output for variance components
  output$varcomp_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[2]]$varcomp)
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 1:3) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centralize
                  ),
                  class = "display")
  })
  
  # Output for AIC and BIC
  output$aic_bic_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[3]])
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 1:2) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Brt',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centralize
                  ),
                  class = "display")
  })
  
  # Output for BLUPs - Table
  output$blups_table_out <- DT::renderDataTable({
    dat <- data.frame(button3()[[4]])
    
    # Rounding of numbers
    decimal_places <- 2  
    for (col in 2) {
      dat[[col]] <- round(as.numeric(dat[[col]]), decimal_places)
    }
    
    DT::datatable(dat,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Centralize
                  ),
                  class = "display")
  })
  
  # Output for BLUPs - Graph
  output$blups_graph_out <- renderPlot({
    dat <- data.frame(button3()[[4]])
    dat[,1] <- as.factor(dat[,1])
    dat[,2] <- as.numeric(dat[,2])
    
    ggplot(dat, 
           aes(x = dat[,1], y = dat[,2])) +
      geom_point(color = "#cc662f", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = input$random_ef,
           y = "BLUP") +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  # Download results
  r_downloadname <- reactive({
    seed <- sample(1:10,1)
    filename <- paste0("mixedmodel","_",seed,".RData")
    return(filename)
  })
  
  r_download <- function() {
    mixedmodel <- button3()[[1]]
    save(mixedmodel, file = r_downloadname())
  }
  
  # download handler
  observeEvent(input$analysis_run, {
    output$download_rdata <- downloadHandler(
      filename = r_downloadname,
      content = function(file) {
        r_download()
        file.copy(r_downloadname(), file, overwrite=T)
        file.remove(r_downloadname())
      }
    )
  })
}

## To be copied in the UI
# mod_MixedModel_ui("MixedModel_ui_1")

## To be copied in the server
# callModule(mod_MixedModel_server, "MixedModel_ui_1")