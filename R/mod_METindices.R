#' METindices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_METindices_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12, 
                 p("Here you can obtain the variance/covariance matrix, herdability, selection gain and 
                   selection indices for Randomized complete block and 
                   alpha lattice experiments in a single experiment. The models applied here considered only fixed effects.")
             ),
             box(width = 12,
                 selectInput(ns("indice_design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block", "Alpha lattice" = "lattice"), 
                             selected = "block")
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("indice_input_exemple")), hr(),
                 p("Upload here your file:"),
                 fileInput(ns("data_indice"), label = h6("File: data.txt"), multiple = F),
                 p("If you do not have an file to be upload you can still check this app features with our example 
                   file. The example file is automatically upload, you just need to procedure to the other buttons."),
                 br(),
                 actionButton(ns("indice1"), "Read the file",icon("refresh")), hr()
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 6,
                     checkboxGroupInput(ns("indice2"), label = p("Choose the traits to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("indice3"), label = p("Choose the locations to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update"),
                     hr(),
                     actionButton(ns("indice5"), "Run analysis",icon("refresh")), br(),
                     
                 ),
                 p("Expand the windows above to access the results")
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="info", title = "Genetic parameters tables",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genetic parameters",
                     p("pheno: phenotype ID"),
                     p("pheno: genetic variance"),
                     p("pheno: phenotypic variance"),
                     p("pheno: herdability"),
                     p("pheno: variation coefficient (%)"),
                     p("Missing data (NA) in the results can be consequence of assumptions break 
                       in the phenotype distribuition. Check screen 'Assumptions test'."),
                     DT::dataTableOutput(ns("gen_par_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Adjusted means",
                     DT::dataTableOutput(ns("adj.means_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genetic covariance matrix",
                     DT::dataTableOutput(ns("gen_cov_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genetic correlation matrix",
                     DT::dataTableOutput(ns("gen_cor_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Phenotypic covariance matrix",
                     DT::dataTableOutput(ns("phen_cov_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Phenotypic correlation matrix",
                     DT::dataTableOutput(ns("phen_cor_out")),
                 )
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="info", title = "Selection indices",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Multiplicative index (Elston)",
                     textInput(ns("k"), label = "Type the minimum/maximum value for each evaluated trait:", value = "Ex: 12,45,123"),
                     textInput(ns("increasing"), label = "Type which traits are better when the value is low:", value = "Ex: alt, fert"),
                     actionButton(ns("elston_button"), "Run analysis",icon("refresh")), hr(),
                     p("This index requires homocedascity of the trait values. If you find many 0 (zeros) index values check the 'Assumptions test' screen."),
                     DT::dataTableOutput(ns("elston_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Mulamba and Mock (1978) selection index",
                     textInput(ns("weights"), label = "Type the weights for each trait:", value = "Ex: 1,1,2"),
                     textInput(ns("increasing1"), label = "Type which traits are better when the value is low:", value = "Ex: alt, fert"),
                     actionButton(ns("mulamba_button"), "Run analysis",icon("refresh")), hr(),
                     DT::dataTableOutput(ns("mulamba_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Smith index",
                     textInput(ns("weights1"), label = "Type the weights for each trait:", value = "Ex: 1,1,2"),
                     actionButton(ns("smith_button"), "Run analysis",icon("refresh")), hr(),
                     p("If you find missing data in the index value, check the covariance matrix and the 'Assumptions test' screen."),
                     DT::dataTableOutput(ns("smith_out")),
                 ),
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="info", title = "Selection gain",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Select the genotypes",
                     checkboxGroupInput(ns("genotypes"), label = p("Select the genotypes:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update"),  
                 ),
                 radioButtons(ns("indice4"), label = p("Choose the trait to be evaluated:"),
                              choices = "Press 'Read the file' button to update",
                              selected = "Press 'Read the file' button to update"),   
                 actionButton(ns("SG_button"), "Run analysis",icon("refresh")), hr(),
                 DT::dataTableOutput(ns("selection_gain_out")),
             )
             
    )
  )
}

#' METindices Server Function
#'
#' @noRd 
mod_METindices_server <- function(input, output, session){
  ns <- session$ns
  output$indice_input_exemple <- downloadHandler(
    filename =  function() {
      paste("example_indice.txt")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$indice_design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  button_indice1 <- eventReactive(input$indice1, {
    if(is.null(input$data_indice)){
      if(input$indice_design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
    } else {
      dat <- read.csv(input$data_indice$datapath)
    }
    dat
  })
  
  observe({
    
    if(any(colnames(button_indice1()) %in% "rep"))
      choices_trait_temp <- colnames(button_indice1())[-c(1:4)] else
        choices_trait_temp <- colnames(button_indice1())[-c(1:3)]
      
      choices_trait <- choices_trait_temp
      names(choices_trait) <- choices_trait_temp
      
      choices_locations_temp <- unique(button_indice1()[,"local"])
      choices_locations <- choices_locations_temp
      names(choices_locations) <- choices_locations_temp
      
      updateCheckboxGroupInput(session, "indice2",
                               label="Choose the traits to be evaluated:",
                               choices = choices_trait,
                               selected = unlist(choices_trait)[1])
      
      updateCheckboxGroupInput(session, "indice3",
                               label="Choose the locations to be evaluated:",
                               choices = choices_locations,
                               selected = unlist(choices_locations)[1:2])
      
  })
  
  button_indice2 <- eventReactive(input$indice5, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button_indice1()
      dat$block <- as.factor(dat$block)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)
      
      if(input$indice_design == "block"){
        if(!all(c("local", "block", "gen") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized complete block design should have columns 'local', 'block' and 'gen'."))
        dat <- dat %>% select(c("local", "gen", "block",input$indice2)) %>%
          filter(local %in% input$indice3) %>% droplevels()
        incProgress(0.5, detail = paste("Doing part", 2))
        results <- analysis_fixed_effects(dat, design = "DBC", multi_env = T)
        
      } else {
        if(!all(c("local", "block", "gen", "rep") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","rep",input$indice2)) %>%
          filter(local %in% input$indice3) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
        results <- analysis_fixed_effects(dat, design = "lattice", multi_env = T)
      }
      
      incProgress(0.75, detail = paste("Doing part", 3))
      results
    })
  })
  
  output$gen_par_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()[[1]]),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$adj.means_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()$adjusted_means),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$gen_cov_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()$genetic_covariance),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$phen_cov_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()$phenotypic_covariance),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  output$gen_cor_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()$genetic_correlation),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$phen_cor_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice2()$phenotypic_correlation),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  button_indice3 <- eventReactive(input$elston_button, {
    if(input$k == "Ex: 12,45,123"){
      k <- NULL
    } else {
      k <- unlist(strsplit(input$k, ","))
      if(length(k) != length(colnames(button_indice2()$adjusted_means)[-1]))
        stop(safeError(paste0("You must provide a minimum/maximum value for every evaluated trait.
                       You provided ", length(k), " values and ", length(colnames(button_indice2()$adjusted_means)[-1]),
                              " are required.")))
    }
    if(input$increasing == "Ex: alt, fert"){
      increasing <- NULL
    } else {
      increasing <- unlist(srtsplit(input$increasing, ","))
    }
    elston_index(button_indice2()$adjusted_means, k, increasing)
  })
  
  output$elston_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice3()),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  button_indice4 <- eventReactive(input$mulamba_button, {
    if(input$weights == "Ex: 1,1,2"){
      weights <- NULL
    } else {
      weights <- unlist(strsplit(input$weights, ","))
      if(length(weights) != length(colnames(button_indice2()$adjusted_means)[-1]))
        stop(safeError(paste0("You must provide a minimum/maximum value for every evaluated trait.
                       You provided ", length(weights), " values and ", length(colnames(button_indice2()$adjusted_means)[-1]),
                              " are required.")))
    }
    
    if(input$increasing == "Ex: alt, fert"){
      increasing <- NULL
    } else {
      increasing <- unlist(srtsplit(input$increasing, ","))
    }
    mulamba_index(button_indice2()$adjusted_means, weights = weights, increasing = increasing)
  })
  
  output$mulamba_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice4()),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  button_indice5 <- eventReactive(input$smith_button, {
    if(input$weights1 == "Ex: 1,1,2"){
      weights <- NULL
    } else {
      weights <- unlist(strsplit(input$weights1, ","))
      if(length(weights) != length(colnames(button_indice2()$adjusted_means)[-1]))
        stop(safeError(paste0("You must provide a minimum/maximum value for every evaluated trait.
                       You provided ", length(weights), " values and ", length(colnames(button_indice2()$adjusted_means)[-1]),
                              " are required.")))
    }
    smith_hazel(adj.means = button_indice2()$adjusted_means,
                cvg = button_indice2()$genetic_covariance, 
                cvf = button_indice2()$phenotypic_covariance, weights = weights)
  })
  
  output$smith_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice5()),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  button_indice6 <- eventReactive(input$SG_button, {
    pheno <- button_indice2()$adjusted_means[,c(1,which(colnames(button_indice2()$adjusted_means) == input$indice4))]
    herdability <- button_indice2()$genetic_parameters[which(button_indice2()$genetic_parameters$pheno == input$indice4),"h2"]
    result <- selection_gain(pheno = pheno, herdability,selected_geno = input$genotypes)
    result
  })
  
  observe({
    choices_trait <- colnames(button_indice2()$adjusted_means)[-1]
    names(choices_trait) <- choices_trait
    
    choices_geno <- unique(button_indice2()$adjusted_means[,1])
    names(choices_geno) <- choices_geno
    
    updateRadioButtons(session, "indice4",
                       label="Choose the trait to be evaluated:",
                       choices = choices_trait,
                       selected = unlist(choices_trait)[1])
    
    updateCheckboxGroupInput(session, "genotypes",
                             label="Select the genotypes:",
                             choices = choices_geno,
                             selected = unlist(choices_geno)[1])
  })
  
  output$selection_gain_out <- DT::renderDataTable(
    DT::datatable(data.frame(button_indice6()),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
}

## To be copied in the UI
# mod_METindices_ui("METindices_ui_1")

## To be copied in the server
# callModule(mod_METindices_server, "METindices_ui_1")

