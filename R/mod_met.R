#' met UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_met_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12, 
                 p("Here we present a graphic interface for the R package 'metan' functions. The package presents other features and options not implemented here.")
             ),
             box(width = 12,
                 selectInput(ns("assum_design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block"), 
                             selected = "block")
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, other called 'gen' defining the genotypes and other called 'block' defining the block number. The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("met_input_exemple")), hr(),
                 p("Upload here your file:"),
                 fileInput(ns("data_met"), label = h6("File: data.txt"), multiple = F),
                 p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                 br(),
                 actionButton(ns("met1"), "Read the file",icon("refresh")), hr()
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 6,
                     radioButtons(ns("met2"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("met3"), label = p("Choose the locations to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update"),
                     hr(),
                     actionButton(ns("met5"), "Run analysis",icon("refresh")), br(),
                    
                 ),
                 p("Expand the windows above to access the results")
             ),
             
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = F, status="info", title = "Graphical analysis of genotype-vs-environment interaction",
                 radioButtons(ns("met4"), label = p("Graphic type:"),
                              choices = list("Heatmap" = 1, "Line plot" = 2),
                              selected = unlist(list("Heatmap" = 1, "Line plot" = 2))[1]), 
                 hr(),
                 plotOutput(ns("ge_plot_out")),
             ),
             
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Annicchiarico's genotypic confidence index",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T,
                     title="Contains the mean, environmental index and classification as favorable and unfavorable environments",
                     DT::dataTableOutput(ns("met_environments_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T,
                     title="Contains the genotypic confidence index considering all environments",
                     DT::dataTableOutput(ns("met_general_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T,
                     title="Contains the genotypic confidence index considering favorable environments",
                     DT::dataTableOutput(ns("met_favorable_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T,
                     title="Contains the genotypic confidence index considering unfavorable environments",
                     DT::dataTableOutput(ns("met_unfavorable_out"))
                 ), hr()
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Shukla's stability variance parameter",
                 box(wwidth = 12, solidHeader = TRUE, 
                     p("ShuklaVar: The Shukla's stability variance parameter"),
                     p("rMean: The rank for the response variable mean"),
                     p("GEN: the genotype's code"),
                     p("rShukaVar: The rank for ShukaVar"), hr(),
                     DT::dataTableOutput(ns("met_ShuklaVar_out"))
                 )
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Eberhart and Russell's regression model",
                 radioButtons(ns("met6"), label = p("The type of plot to show. type = 1 produces a plot with the environmental index in the x axis and the genotype mean yield in the y axis. type = 2 produces a plot with the response variable in the x axis and the slope of the regression in the y axis"),
                              choices = list("1" = 1, "2" = 2),
                              selected = unlist(list("1" = 1, "2" = 2))[1]), 
                 plotOutput(ns("met_reg_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Additive Main effects and Multiplicative Interaction",
                 p("Type of biplot to produce"),
                 p("*type = 1 Produces an AMMI1 biplot (Y x PC1) to make inferences related to stability and productivity."),
                 p("*type = 2 The default, produces an AMMI2 biplot (PC1 x PC2) to make inferences related to the interaction effects. Use the arguments first or second to change the default IPCA shown in the plot."),
                 p("*type = 3 Valid for objects of class waas or waasb, produces a biplot showing the GY x WAASB."),
                 p("*type = 4 Produces a plot with the Nominal yield x Environment PC"),
                 radioButtons(ns("met7"), label = p("Type:"),
                              choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                              selected = unlist(list("1" = 1, "2" = 2))[1]), 
                 
                 numericInput(ns("met8"), label = h3("The variable to plot:"), value = 1),
                 plotOutput(ns("met_plot_scores_out"))
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Stability analysis based on Wricke's model",
                 DT::dataTableOutput(ns("met_eco_out"))
             )
    )
  )
}

#' met Server Function
#'
#' @import ggplot2
#' @import metan
#' @import dplyr
#' 
#' @noRd 
mod_met_server <- function(input, output, session){
  ns <- session$ns
  ## download input
  output$met_input_exemple <- downloadHandler(
    filename =  function() {
      paste("example_met.txt")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      write.csv(dat, file = file)
    } 
  )
  
  button_met1 <- eventReactive(input$met1, {
    if(is.null(input$data_met)){
      dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
    } else {
      dat <- read.csv(input$data_met$datapath)
    }
    dat
  })
  
  observe({
    choices_trait_temp <- colnames(button_met1())[-c(1:3)]
    choices_trait <- choices_trait_temp
    names(choices_trait) <- choices_trait_temp
    
    choices_locations_temp <- unique(button_met1()[,1])
    choices_locations <- choices_locations_temp
    names(choices_locations) <- choices_locations_temp
    
    updateRadioButtons(session, "met2",
                       label="Choose the trait to be evaluated:",
                       choices = choices_trait,
                       selected = unlist(choices_trait)[1])
    
    updateCheckboxGroupInput(session, "met3",
                             label="Choose the locations to be evaluated:",
                             choices = choices_locations,
                             selected = unlist(choices_locations))
  })
  
  button_met2 <- eventReactive(input$met5, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button_met1() %>% select(c("local", "gen", "block",input$met2)) %>% 
        filter(local %in% input$met3) %>% droplevels() %>%
        mutate_if(is.character, as.factor)
      
      Ann <- Annicchiarico(dat,
                           env = local,
                           gen = gen,
                           rep = block,
                           resp = input$met2,
                           prob = 0.25)
      incProgress(0.25, detail = paste("Doing part", 2))
      Shuk <- Shukla(dat,
                     env = local,
                     gen = gen,
                     rep = block,
                     resp = input$met2)
      incProgress(0.5, detail = paste("Doing part", 3))
      Shuk_df <- data.frame(ShuklaVar = Shuk[[input$met2]]$ShuklaVar,
                            GEN = Shuk[[input$met2]]$GEN,
                            rMean = Shuk[[input$met2]]$rMean,
                            rShukaVar= Shuk[[input$met2]]$rShukaVar
      )

      incProgress(0.75, detail = paste("Doing part", 4))
      missing <- colnames(table(dat$gen, dat$local)[,which(table(dat$gen, dat$local) == 0, arr.ind = T)[,2]])
      if(length(missing) > 0){
        jra <- missing
        class(jra) <- "missing"
      } else {
        jra <- ge_reg(dat, env = local, gen = gen, rep = block, resp = input$met2)
      }
      
      incProgress(0.8, detail = paste("Doing part", 5))
      # AMMI
      Ammi <- performs_ammi(dat, env = local, gen = gen, rep = block, resp = input$met2)
      
      # ecovalence (Wricke, 1965)
      eco <- ecovalence(dat,
                        env = local,
                        gen = gen,
                        rep = block,
                        resp = input$met2)
      
      list(dat, Ann, Shuk_df, jra, Ammi, eco)
    })
  })
  
  output$ge_plot_out <- renderPlot({
    metan::ge_plot(button_met2()[[1]], gen = gen, env = local, 
                   resp = button_met2()[[1]][,input$met2], 
                   type = input$met4) + labs(fill = "Pheno") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$met_environments_out <- DT::renderDataTable(
    DT::datatable(button_met2()[[2]][[input$met2]]$environments,  
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  output$met_general_out <- DT::renderDataTable(
    DT::datatable(button_met2()[[2]][[input$met2]]$general,  
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  output$met_favorable_out <- DT::renderDataTable(
    DT::datatable(button_met2()[[2]][[input$met2]]$favorable,  
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  output$met_unfavorable_out <- DT::renderDataTable(
    DT::datatable(button_met2()[[2]][[input$met2]]$unfavorable,  
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  
  output$met_ShuklaVar_out <- DT::renderDataTable(
    DT::datatable({button_met2()[[3]]}, 
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  
  output$met_reg_out <- renderPlot({
    if(class(button_met2()[[4]]) == "missing"){
      stop(safeError(paste("Environments", paste(button_met2()[[4]], collapse = " "), "do not present all genotypes. We suggest the removal these environments from the analysis.")))
    } else {
      plot(button_met2()[[4]], type = input$met6)
    }
  })
  
  output$met_plot_scores_out <- renderPlot({
    plot_scores(button_met2()[[5]], var = input$met8, type = input$met7)
  })
  
  output$met_eco_out <- DT::renderDataTable(
    DT::datatable(button_met2()[[6]][[1]],  
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel')
                  ),
                  class = "display")
  )
  
}

## To be copied in the UI
# mod_met_ui("met_ui_1")

## To be copied in the server
# callModule(mod_met_server, "met_ui_1")

