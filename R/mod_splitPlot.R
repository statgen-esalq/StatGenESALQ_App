#' assumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_splitPlot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:8000px",
             box(width = 12,
                 p("Here we present several tests for checking model assumptions for single trait and environment.")
             ),
             box(width = 12,
                 selectInput(ns("assum_design"), label = h4("Experiment design - Split Plot"), 
                             choices = list("Completely randomized (CRSP)" = "crsp" ,"Randomized complete block (RBSP)" = "rbsp", 
                                            selected = "rbsp")
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                     p("The input file is a tab delimited table with a column called 'local' defining the environment and 
                   other called 'gen' defining the genotypes. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                     downloadButton(ns("assum_input_exemple")), hr(),
                     p("Upload here your file:"),
                     fileInput(ns("data_assum"), label = h6("File: data.txt"), multiple = F),
                     p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                     hr(),
                     p("Data View:"),
                     box(width = 4,
                         radioButtons(ns("assum6"), label = p("Select the separator"),
                                      choices = list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                                      selected = ";")
                     ),
                     box(width = 8,  
                         #Visualização dos dados
                         tableOutput(ns("dataview"))
                     ),
                     hr(),
                     actionButton(ns("assum1"), "Read the file",icon("refresh")), hr()
                 ),
                 
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                     box(width = 12,
                         radioButtons(ns("assum4"), label = p("Select the transformation type:"),
                                      choices = list("none" = "none","log" = "log", "sqrt(x + 0.5)" = "sqrt(x + 0.5)", "boxcox" = "boxcox"),
                                      selected = "none"), hr(),
                         p("If your data still not present the assumptions safter transformation, we suggest the usage of Generalized Linear Models (still not implemented in this app).")
                     ),
                     hr(),
                     box(width = 6,
                         radioButtons(ns("assum7"), label = p("Choose the whole-plot factor to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     box(width = 6,
                         radioButtons(ns("assum8"), label = p("Choose the split-plot factor to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     hr(),
                     box(width = 6,
                         radioButtons(ns("assum2"), label = p("Choose the traits to be evaluated"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update"),
                     ),
                     box(width = 6,
                         radioButtons(ns("assum3"), label = p("Choose the location to be evaluated:"),
                                      choices = "Press 'Read the file' button to update",
                                      selected = "Press 'Read the file' button to update")
                         # ,
                         # hr(),
                         # actionButton(ns("assum5"), "Run analysis",icon("refresh")), br(),
                     ),
                     
                     actionButton(ns("assum5"), "Run analysis", icon("refresh")), hr(),
                     p("Expand the windows above to access the results")
                     
                     # box(width = 12,
                     #     p("Expand the windows above to access the results")
                     #     )
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
#' @import GAD
#' @import dplyr
#' 
#' @noRd 
mod_splitPlot_server <- function(input, output, session){
  ns <- session$ns
  library(GAD)
  ## download input
  output$assum_input_exemple <- downloadHandler(
    filename =  function() {
      paste("example_assum.txt")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # ARRUMAR OS EXEMPLOS
      if(input$assum_design == "rbsp"){
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  #Tratamento de dados possibilitando a utilização de .txt e .csv
  observeEvent(input$assum6, {
    observeEvent(input$data_assum, {
      if (is.null(input$data_assum)) {
        output$dataview <- renderTable({
          return(p("Upload your data"))
        })
      } else {
        df <- read.csv(input$data_assum$datapath, sep = input$assum6)
        output$dataview <- renderTable({
          return(head(df))
        })
      }
    })
  })
  
  button_assum1 <- eventReactive(input$assum1, {
    str(input$data_assum)
    #Aqui esta pegando os exemplos
    if(is.null(input$data_assum)){
      if(input$assum_design == "rbsp"){
        # ARRUMAR OS EXAMPLOS
        dat <- read.csv(system.file("ext","example_inputs/example_blocks.csv", package = "StatGenESALQ"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/example_lattice.csv", package = "StatGenESALQ"))
      }
    } else {
      #Aqui entra o upload
      dat <- read.csv(input$data_assum$datapath,
                      sep = input$assum6)
    }
    cat(colnames(dat))
    dat
  })
  
  observe({
    #I need change that, it's the problem
    choices_trait_temp <- colnames(button_assum1())[-c(1:4)]
    choices_trait <- choices_trait_temp
    names(choices_trait) <- choices_trait_temp
    
    #Veremos no que dá
    choices_factor_temp <- colnames(button_assum1()[,3:4])

    # to_remove <- c('local', 'gen')
    # choices_factor_temp[ , !(names(choices_factor_temp) %in% to_remove)]
    
    # subset(choices_factor_temp, select = -c(local, gen))
    
    # choices_factor_temp <- as.character(choices_factor_temp)
    
    # choices_factor_temp %>% 
    #   select(-c("local", "gen"))
    
    choices_factor <- choices_factor_temp
    names(choices_factor) <- choices_factor_temp
    
    # to_remove <- c("local", "gen")
    # choices_factor[ , !(names(choices_factor) %in% to_remove)]
    
    # choices_factor %>%
    #   select(-c("local", "gen"))
    
    # subset(choices_factor, select = -c(local, block))
    
    choices_locations_temp <- unique(button_assum1()[,"local"])
    choices_locations <- choices_locations_temp
    names(choices_locations) <- choices_locations_temp
    
    updateRadioButtons(session, "assum7",
                       label="Choose the whole-plot factor to be evaluated:",
                       choices = choices_factor,
                       selected = unlist(choices_factor)[1])
    
    updateRadioButtons(session, "assum8",
                       label="Choose the split-plot factor to be evaluated:",
                       choices = choices_factor,
                       selected = unlist(choices_factor)[1])
    
    updateRadioButtons(session, "assum2",
                       label="Choose the trait to be evaluated:",
                       choices = choices_trait,
                       selected = unlist(choices_trait)[1])
    
    updateRadioButtons(session, "assum3",
                       label="Choose the locations to be evaluated:",
                       choices = choices_locations,
                       selected = unlist(choices_locations)[1])
  })
  
  button_assum2 <- eventReactive(input$assum5, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button_assum1()
      dat$time <- as.factor(dat$time)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)

      
      if(input$assum_design == "rbsp"){
        if(!all(c("local", "block", "time", "gen") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized block split plot design should have columns 'local', 'block', 'time', and 'gen'."))
        dat$block <- as.factor(dat$block)
        #Mutio importante lembrar que é aqui onde os dados serão filtrados, disponibilizando apenas aqueles que serão analisados
        dat <- dat %>% select(c("local", "block", input$assum7, input$assum8, input$assum2)) %>%
          filter(local == input$assum3) %>% droplevels() #droplevels() - It's useful to remove unneeded factor levels
        
        dat$block <- as.random(dat$block)
        
        if(input$assum4 == "none"){
          mod <- run_models_sp(df = dat, pheno = dat[,input$assum2] , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] , design = "RBSP", multi_env = F)
        } else if(input$assum4 == "log"){
          mod <- run_models_sp(df = dat, pheno = log(dat[,input$assum2]) , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "RBSP", multi_env = F)
        } else if(input$assum4 == "sqrt(x + 0.5)"){
          mod <- run_models_sp(df = dat, pheno = sqrt(dat[,input$assum2] + 0.5) , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "RBSP", multi_env = F)
        } else if(input$assum4 == "boxcox"){
          mod <- run_models_boxcox_sp(df = dat, pheno = dat[,input$assum2] , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "RBSP", multi_env = F)
        } 
        
        incProgress(0.5, detail = paste("Doing part", 2))
        
      } else {
        if(!all(c("local", "block", "gen", "rep") %in% colnames(dat)))
          stop(safeError("Completely randomized split plot design should have columns 'local', 'rep', 'time', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        #Just remember that it's an important step in the analysis's process
        dat <- dat %>% select(c("local", "rep", input$assum7, input$assum8, input$assum2)) %>%
          filter(local == input$assum3) %>% droplevels()

        dat$rep <- as.random(dat$rep)
        
        if(input$assum4 == "none"){
          mod <- run_models_sp(df = dat, pheno = dat[,input$assum2] , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "CRSP", multi_env = F)
        } else if(input$assum4 == "log"){
          mod <- run_models_sp(df = dat, pheno = log(dat[,input$assum2]) , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "CRSP", multi_env = F)
        } else if(input$assum4 == "sqrt(x + 0.5)"){
          mod <- run_models_sp(df = dat, pheno = sqrt(dat[,input$assum2] + 0.5) , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "CRSP", multi_env = F)
        } else if(input$assum4 == "boxcox"){
          mod <- run_models_boxcox_sp(df = dat, pheno = dat[,input$assum2] , whole_f = dat[,input$assum7] , split_f = dat[,input$assum8] ,design = "CRSP", multi_env = F)
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
  
  # output$assum1_plot_out <- renderPlot({
  #   # Crie o gráfico
  #   plot <- autoplot(button_assum2()[[1]], which = 1, data = button_assum2()[[5]],
  #                    colour = "#CC662f", smooth.colour = "#003350")
  #   
  #   # Ajuste o tamanho do gráfico para preencher a janela
  #   plot + theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  #     coord_cartesian(clip = "on")  # Evita que elementos do gráfico sejam cortados
  # })
  

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
  
  # Here, I use the function gad() because those design have two experimental units, so there're two expetimental error terms 
  # output$assum_anova_out <- DT::renderDataTable({
  #   DT::datatable(data.frame(gad(button_assum2()[[1]])),
  #                 rownames = c("block", paste(input$assum7), paste(input$assum8), paste("block:", input$assum7), paste(input$assum7, input$assum8, sep = ":"), "residual"),
  #                 extensions = 'Buttons',
  #                 options = list(
  #                   dom = 'Bfrtlp',
  #                   buttons = c('copy', 'csv', 'excel', 'pdf')
  #                   ),
  #                 class = "display") %>%
  #     DT::formatStyle(columns = 2:4, decimalPlaces = 4)
  #   })
  # 
  output$assum_anova_out <- DT::renderDataTable({
    # Obtenha o conjunto de dados
    data <- gad(button_assum2()[[1]])
    
    # Especifique as colunas que deseja arredondar e o número de casas decimais
    # columns_to_round <- c("Sum.Sq", "Mean.Sq", "F.value", "Pr..F.", "outra_coluna1", "outra_coluna2")
    decimal_places1 <- 2  # Especifique o número de casas decimais
    
    # Arredonde as colunas selecionadas
    for (col in 2:4) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    decimal_places1 <- 5
    for (col in 5) {
      data[[col]] <- round(as.numeric(data[[col]]), decimal_places1)
    }
    
    # Crie a tabela DataTable
    DT::datatable(data,
                  rownames = c("block", paste(input$assum7), paste(input$assum8), paste("block:", input$assum7,  "\n(residual a)"), paste(input$assum7, input$assum8, sep = ":"), "residual"),
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ),
                  class = "display")
  })
  
  
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

##' Check presence of outliers using Bonferroni-Holm tests for the adjusted p-values
##' @param resid residuals (e.g. lm.object$residuals)
##' 
##' @import multtest
outlier <- function(resid, alpha=0.05){
  # Produce externally studentized residuals
  studresid <- resid/sd(resid, na.rm=TRUE)
  # Calculate adjusted p-values
  rawp.BHStud = 2 * (1 - pnorm(abs(studresid)))
  #Produce a Bonferroni-Holm tests for the adjusted p-values
  #The output is a list
  test.BHStud<-mt.rawp2adjp(rawp.BHStud,proc=c("Holm"),alpha = alpha)
  #Create vectors/matrices out of the list of the BH tests
  adjp = cbind(test.BHStud[[1]][,1])
  bholm = cbind(test.BHStud[[1]][,2])
  index = test.BHStud$index
  # Condition to flag outliers according to the BH test
  out_flag = ifelse(bholm<alpha, "OUTLIER ", ".")
  #Create a matrix with all the output of the BH test
  BHStud_test = cbind(adjp,bholm,index,out_flag)
  #Order the file by index
  BHStud_test2 = BHStud_test[order(index),]
  #Label colums
  names = c("rawp","bholm","index","out_flag")
  colnames(BHStud_test2) <- names
  #Create a final file, with the data and the test and the labels for the outliers
  
  # Take a look at the outliers
  outliers_BH <- as.numeric(BHStud_test2[which(BHStud_test2[,"out_flag"]!="."),"index"])
  if(length(outliers_BH)==0){ 
    outliers_BH_df <- data.frame(value = "-", status = "no outliers detected")
  } else { 
    outliers_BH_df <- data.frame(value = outliers_BH, status = "outlier")
  }
  return(outliers_BH_df)
}


##' Utilities
##' 
##' @import MASS
run_models_boxcox_sp <-function(pheno, whole_f, split_f, df, design, multi_env){
  if(design == "CRD"){
    if(multi_env){
      bc <- boxcox(pheno ~ df$gen + df$local/df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      mod <- lm(pheno_trans ~ df$gen + df$local/df$local + df$gen*df$local)
      
    } else {
      bc <- boxcox(pheno ~ df$gen, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      mod <- lm(pheno_trans ~ df$gen)
    }
  } else if(design == "DBC"){
    if(multi_env){
      bc <- boxcox(pheno ~ df$gen + df$local/df$block + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      mod <- lm(pheno_trans ~ df$gen + df$local/df$block + df$local + df$gen*df$local)
      
    } else {
      bc <- boxcox(pheno ~ df$gen + df$block, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      
      mod <- lm(pheno_trans ~ df$gen + df$block)
    }
  } else if(design == "lattice") {
    if(multi_env){
      bc <- boxcox(pheno ~ df$gen + df$local/df$rep/df$block +
                     df$local/df$rep + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      
      mod <- lm(pheno_trans ~ df$gen + df$local/df$rep/df$block +
                  df$local/df$rep + df$local + df$gen*df$local)
    } else {
      bc <- boxcox(pheno ~ df$gen + df$rep/df$block, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      
      mod <- lm(pheno_trans ~ df$gen + df$rep/df$block)
    }
  } else if(design == "RBSP") {
    
    block <- as.random(df$block)
    whole_f <- as.fixed(whole_f)
    split_f <- as.fixed(split_f)
    
    # if(multi_env){
    #   bc <- boxcox(pheno ~ df$gen + df$local/df$rep/df$block +
    #                  df$local/df$rep + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
    #   lambda <- bc$x[which.max(bc$y)]
    #   pheno_trans <- ((pheno^lambda-1)/lambda)
    #   
    #   mod <- lm(pheno_trans ~ df$gen + df$local/df$rep/df$block +
    #               df$local/df$rep + df$local + df$gen*df$local)
    # } else {
    bc <- boxcox(pheno ~ block + whole_f + whole_f/block + split_f + whole_f:split_f, plotit=F, lam=seq(-3, 3, 1/20))
    lambda <- bc$x[which.max(bc$y)]
    pheno_trans <- ((pheno^lambda-1)/lambda)
    
    mod <- lm(pheno_trans ~ block + whole_f + whole_f/block + split_f + whole_f:split_f)
    #   }
  }
  else if(design == "CRSP") {
    
    rep <- as.random(df$rep)
    whole_f <- as.fixed(whole_f)
    split_f <- as.fixed(split_f)
    
    # if(multi_env){
    #   bc <- boxcox(pheno ~ df$gen + df$local/df$rep/df$block +
    #                  df$local/df$rep + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
    #   lambda <- bc$x[which.max(bc$y)]
    #   pheno_trans <- ((pheno^lambda-1)/lambda)
    #   
    #   mod <- lm(pheno_trans ~ df$gen + df$local/df$rep/df$block +
    #               df$local/df$rep + df$local + df$gen*df$local)
    # } else {
    bc <- boxcox(pheno ~ whole_f + whole_f/rep + split_f + whole_f:split_f, plotit=F, lam=seq(-3, 3, 1/20))
    lambda <- bc$x[which.max(bc$y)]
    pheno_trans <- ((pheno^lambda-1)/lambda)
    
    mod <- lm(pheno_trans ~ whole_f + whole_f/rep + split_f + whole_f:split_f)
    #   }
  }
  return(mod)
}


## To be copied in the UI
# mod_assumptionsTest_ui("assumptionsTest_ui_1")

## To be copied in the server
# callModule(mod_assumptionsTest_server, "assumptionsTest_ui_1")
