#' METassumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_METassumptionsTest_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:6000px",
             box(width = 12, 
                 p("Here we present several tests for checking model assumptions for single trait and multiple environment.")
             ),
             box(width = 12,
                 selectInput(ns("assum_design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block", "Alpha lattice" = "lattice"), 
                             selected = "block")
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("assum_input_exemple")), hr(),
                 p("Upload here your file:"),
                 fileInput(ns("data_assum"), label = h6("File: data.txt"), multiple = F),
                 p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                 br(),
                 actionButton(ns("assum1"), "Read the file",icon("refresh")), hr()
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 6,
                     radioButtons(ns("assum4"), label = p("Select the transformation type:"),
                                  choices = list("none" = "none","log" = "log", "sqrt(x + 0.5)" = "sqrt(x + 0.5)", "boxcox" = "boxcox"),
                                  selected = "none"), hr(),
                     p("If your data still not present the assumptions safter transformation, we suggest the usage of Generalized Linear Models (still not implemented in this app).")
                 ),
                 box(width = 6,
                     radioButtons(ns("assum2"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("assum3"), label = p("Choose the location to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update"),
                     hr(),
                     actionButton(ns("assum5"), "Run analysis",icon("refresh")), br(),
                     
                 ), hr(),
                 box(width = 12,
                     p("Expand the windows above to access the results"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Plots",
                 
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Residuals vs fitted values",
                     p("Checking the linearity of the model: if the blue line is close to horizontal line."),
                     p("Checking the homoscedasticity of variances: retangular draw of the dots (not triangular)."),
                     plotOutput(ns("assum1_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Normal Q-Q",
                     p("Checking the residuals normal distribution: dots should overlap the dashed line."),
                     plotOutput(ns("assum2_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Scale-Location",
                     p("Checking the homoscedasticity or homogeneity of variances: line should be close to horizontal line and dots should present a retangular draw"),
                     plotOutput(ns("assum3_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Residuals vs factor levels",
                     p("Checking outilers: red line should overlay the dashed line and the dots should not be out of a determined range (e.g. [-2,2])."),
                     plotOutput(ns("assum4_plot_out")),
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Histogram of residuals",
                     plotOutput(ns("assum5_plot_out")),
                 )
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Anova",
                 DT::dataTableOutput(ns("assum_anova_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Shapiro-Wilk normality test",
                 p("H0: normal distribuition (p-value > 0.05)."),
                 p("H1: we can't consider that it is a normal distribuition (p-value < 0.05)."),
                 DT::dataTableOutput(ns("assum_sha_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Durbin-Watson Test for Autocorrelated Errors",
                 p("Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values."),
                 p("Used for normal distribuited longitudinal datasets. Usually, values of D-W between 1.5 and 2.5 indicate residual independence."),
                 DT::dataTableOutput(ns("assum_dur_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Breusch-Pagan Test",
                 p("Performs the Breusch-Pagan test against heteroskedasticity."),
                 p("Has as assumption normal distribuition."),
                 p("H0: there is homocedascity of variances (p-value > 0.05)."),
                 p("H1: we can't consider that there is homocedascity of variances (p-value < 0.05)."),
                 DT::dataTableOutput(ns("assum_bp_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Bonferroni-Holm tests for the adjusted p-values",
                 tableOutput(ns("assum_out_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Variance Inflation Factors",
                 p("Checking multicollinearity: VIF value higher than 10 indicates multicollinearity."),
                 tableOutput(ns("assum_vif_out"))
             )
    )
  )
}

#' METassumptionsTest Server Function
#'
#' @noRd 
mod_METassumptionsTest_server <- function(input, output, session){
  ns <- session$ns
  ## download input
  output$assum_input_exemple <- downloadHandler(
    filename =  function() {
      paste("example_assum.txt")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$assum_design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  button_assum1 <- eventReactive(input$assum1, {
    if(is.null(input$data_assum)){
      if(input$assum_design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
    } else {
      dat <- read.csv(input$data_assum$datapath)
    }
    dat
  })
  
  observe({
    
    if(any(colnames(button_assum1()) %in% "rep"))
      choices_trait_temp <- colnames(button_assum1())[-c(1:4)] else
        choices_trait_temp <- colnames(button_assum1())[-c(1:3)]
      
      choices_trait <- choices_trait_temp
      names(choices_trait) <- choices_trait_temp
      
      choices_locations_temp <- unique(button_assum1()[,"local"])
      choices_locations <- choices_locations_temp
      names(choices_locations) <- choices_locations_temp
      
      updateRadioButtons(session, "assum2",
                         label="Choose the trait to be evaluated:",
                         choices = choices_trait,
                         selected = unlist(choices_trait)[1])
      
      updateCheckboxGroupInput(session, "assum3",
                               label="Choose the locations to be evaluated:",
                               choices = choices_locations,
                               selected = unlist(choices_locations)[1:2])
  })
  
  button_assum2 <- eventReactive(input$assum5, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button_assum1()
      dat$block <- as.factor(dat$block)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)
      
      if(input$assum_design == "block"){
        if(!all(c("local", "block", "gen") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized complete block design should have columns 'local', 'block' and 'gen'."))
        dat <- dat %>% select(c("local", "gen", "block",input$assum2)) %>%
          filter(local %in% input$assum3) %>% droplevels()
        
        if(input$assum4 == "none"){
          mod <- run_models(df = dat, pheno = dat[,input$assum2] ,design = "DBC", multi_env = T)
        } else if(input$assum4 == "log"){
          mod <- run_models(df = dat, pheno = log(dat[,input$assum2]) ,design = "DBC", multi_env = T)
          
        } else if(input$assum4 == "sqrt(x + 0.5)"){
          mod <- run_models(df = dat, pheno = sqrt(dat[,input$assum2] + 0.5) ,design = "DBC", multi_env = T)
          
        } else if(input$assum4 == "boxcox"){
          mod <- run_models_boxcox(df = dat, pheno = dat[,input$assum2] ,design = "DBC", multi_env = T)
        } 
        
        incProgress(0.5, detail = paste("Doing part", 2))
        
      } else {
        if(!all(c("local", "block", "gen", "rep") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","rep",input$assum2)) %>%
          filter(local %in% input$assum3) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
        
        if(input$assum4 == "none"){
          mod <- run_models(df = dat, pheno = dat[,input$assum2] ,design = "lattice", multi_env = T)
        } else if(input$assum4 == "log"){
          mod <- run_models(df = dat, pheno = log(dat[,input$assum2]) ,design = "lattice", multi_env = T)
          
        } else if(input$assum4 == "sqrt(x + 0.5)"){
          mod <- run_models(df = dat, pheno = sqrt(dat[,input$assum2] + 0.5) ,design = "lattice", multi_env = T)
          
        } else if(input$assum4 == "boxcox"){
          mod <- run_models_boxcox(df = dat, pheno = dat[,input$assum2] ,design = "lattice", multi_env = T)
        } 
      }
      
      sha <- shapiro.test(mod$residuals)
      df <- data.frame(W = sha$statistic,
                       `p-value` = sha$p.value)
      dur <-durbinWatsonTest(mod)
      dur_df <- data.frame(r = dur$r,
                           dw = dur$dw,
                           p = dur$p,
                           alternative = dur$alternative)
      bp <- bptest(mod)
      bp_df <- data.frame(statistic = bp$statistic,
                          parameter = bp$parameter,
                          method = bp$method,
                          `p-value` = bp$p.value)
      
      incProgress(0.25, detail = paste("Doing part", 2))
      list(mod,df,dur_df,bp_df, dat)
    })
  })
  
  output$assum1_plot_out <- renderPlot({
    autoplot(button_assum2()[[1]], which = 1, data = button_assum2()[[5]],
             colour = "#CC662f", smooth.colour = "#003350") 
  })
  output$assum2_plot_out <- renderPlot({
    autoplot(button_assum2()[[1]], which = 2, data = button_assum2()[[5]],
             colour = "#CC662f", smooth.colour = "#003350") 
  })
  output$assum3_plot_out <- renderPlot({
    autoplot(button_assum2()[[1]], which = 3, data = button_assum2()[[5]],
             colour = "#CC662f", smooth.colour = "#003350") 
  })
  output$assum4_plot_out <- renderPlot({
    plot(button_assum2()[[1]],5)
  })
  
  output$assum5_plot_out <- renderPlot({
    ggplot(button_assum2()[[1]], aes(x = button_assum2()[[1]]$residuals)) +
      geom_histogram(bins = 11, colour = "black", fill = "#CC662f", ) +
      labs(title = "Histogram of Residuals",x = "Residuals", y = "Frequency")
  })
  
  output$assum_anova_out <- DT::renderDataTable(
    DT::datatable(data.frame(round(anova(button_assum2()[[1]]),2)),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$assum_sha_out <- DT::renderDataTable(
    DT::datatable(button_assum2()[[2]],  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  output$assum_dur_out <- DT::renderDataTable(
    DT::datatable(button_assum2()[[3]],  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$assum_bp_out <- DT::renderDataTable(
    DT::datatable(button_assum2()[[4]],  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  )
  
  output$assum_out_out <- renderTable({
    as.data.frame(outlier(button_assum2()[[1]]$residuals))
  })
  
  output$assum_vif_out <- renderTable({
    as.data.frame(vif(button_assum2()[[1]]))
  })
}

## To be copied in the UI
# mod_METassumptionsTest_ui("METassumptionsTest_ui_1")

## To be copied in the server
# callModule(mod_METassumptionsTest_server, "METassumptionsTest_ui_1")

