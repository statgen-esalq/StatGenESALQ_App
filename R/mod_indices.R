#' indices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_indices_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:5000px",
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
                     radioButtons(ns("indice3"), label = p("Choose the locations to be evaluated:"),
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

#' indices Server Function
#'
#' @import emmeans
#' @import metan
#' 
#' @noRd 
mod_indices_server <- function(input, output, session){
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
      
      updateRadioButtons(session, "indice3",
                         label="Choose the locations to be evaluated:",
                         choices = choices_locations,
                         selected = unlist(choices_locations)[1])
      
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
          filter(local == input$indice3) %>% droplevels()
        incProgress(0.5, detail = paste("Doing part", 2))
        results <- analysis_fixed_effects(dat, design = "DBC", multi_env = F)
        
      } else {
        if(!all(c("local", "block", "gen", "rep") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","rep",input$indice2)) %>%
          filter(local == input$indice3) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
        results <- analysis_fixed_effects(dat, design = "lattice", multi_env = F)
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
      k <- unlist(strplit(input$k, ","))
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
      weights <- unlist(strplit(input$weights, ","))
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
      weights <- unlist(strplit(input$weights1, ","))
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
    result <- selection_gain(pheno = pheno, herdability,selected_ind = input$genotypes)
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

##' Calculates covariance/variance matrix for DBC and alpha lattice design in
##' single environment
##' 
##' @param design character defining "DBC" do "lattice"
##' @param df data.frame
##' @param multi_env logica TRUE/FALSE indicating multienvironment analysis
##' 
analysis_fixed_effects <- function(df, design, multi_env){
  if(design == "DBC"){
    pheno <- colnames(df)[-c(1:3)]
  } else {
    pheno <- colnames(df)[-c(1:4)]
  }
  vg <- vf <- vga <- h2 <- CV <- vector()
  for(i in 1:length(pheno)){
    temp <- df[,pheno[i]]
    mod <- run_models(temp, df, design, multi_env)
    
    # médias ajustadas
    log <- capture.output({
    emm <- print(emmeans(mod, specs = ~ gen))
    })
    emm_vec <- emm$emmean
    if(i == 1) df_emm <- data.frame(gen = emm$gen, emm_vec) else df_emm <- cbind(df_emm, emm_vec)
    
    mod_anova <- anova(mod)
    
    if(design == "DBC"){
      n_rep <- length(unique(df$block))
    } else {
      n_rep <- length(unique(df$rep))
    }
    
    if(multi_env){
      n_local <- length(unique(df$local))
      # variância genetica
      vg[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["df$gen:df$local","Mean Sq"])/n_rep*n_local
      
      # variancia da interação genotipo x ambiente
      vga[i] <- (mod_anova["df$gen:df$local",'Mean Sq'] - mod_anova["Residuals",'Mean Sq'])/n_rep*n_local
      
      # variância fenotipica
      vf[i] <- mod_anova["df$gen","Mean Sq"]/n_rep*n_local
      
    } else {
      # variância genetica
      vg[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["Residual","Mean Sq"])/n_rep
      
      # variância fenotipica
      vf[i] <- mod_anova["df$gen","Mean Sq"]/n_rep
    }
    
    # not normal distribuition
    if(vg[i] < 0) {
      warning("Negative variances in phenotype ", pheno[which(vg < 0)], ". It can indicat lack of normality in its distribution.")
      vf[i] <- NA
      vg[i] <- NA
      h2[i] <- NA
      CV[i] <- NA
      if(multi_env) vga[i] <- NA
    } else {
      # herdabilidade
      h2[i] <- vg[i]/(vg[i] + ((mod_anova["Residuals",'Mean Sq'])/n_rep))
      
      # CV
      # sqrt(qme)/med*100
      CV[i] <- (sqrt(mod_anova["Residuals",'Mean Sq'])/mean(emm_vec))*100 
    }
  }
  
  # Modelos pheno dois a dois
  combin <- expand.grid(pheno,pheno)
  cvg <- cvf <- v2g <- v2f <- corrg <-  corrf <- vector()
  for(i in 1:dim(combin)[1]){
    temp1 <- df[,as.character(combin[i,1])] 
    temp2 <- df[,as.character(combin[i,2])]
    temp <- temp1 + temp2
    
    mod <- run_models(temp, df, design, multi_env)
    
    mod_anova <- anova(mod)
    if(design == "DBC"){
      n_rep <- length(unique(df$block))
    } else {
      n_rep <- length(unique(df$rep))
    }
    
    if(multi_env){
      # Genetic variance 
      v2g[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["df$gen:df$local","Mean Sq"])/n_rep*n_local
      
      # Phenotypic variance
      v2f[i] <- mod_anova["df$gen","Mean Sq"]/n_rep*n_local
      
    } else {
      # variância genetica
      v2g[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["Residual","Mean Sq"])/n_rep
      
      # variância fenotipica
      v2f[i] <- mod_anova["df$gen","Mean Sq"]/n_rep
    }
    
    # Genetic Covariance 
    cvg[i] <- (v2g[i] - vg[combin[i,1]] - vg[combin[i,2]])/2
    
    # Genetic correlation
    corrg[i] <- cvg[i]/sqrt(vg[combin[i,1]]*vg[combin[i,2]])
    
    # Phenotypic covariance
    cvf[i] <- (v2f[i] - vf[combin[i,1]] - vf[combin[i,2]])/2
    
    # Phenotypic correlation
    corrf[i] <- cvf[i]/sqrt(vf[combin[i,1]]*vf[combin[i,2]])
  }
  
  cvg <- matrix(round(cvg,2), nrow = length(pheno))
  cvf <- matrix(round(cvf,2), nrow = length(pheno))
  corrg <- matrix(round(corrg,2), nrow = length(pheno))
  corrf <- matrix(round(corrf,2), nrow = length(pheno))

  colnames(cvg) <- rownames(cvg) <- rownames(corrg) <- rownames(corrf) <-  pheno
  colnames(cvf) <- rownames(cvf) <- colnames(corrg) <- colnames(corrf) <- pheno
  
  # Results
  if(multi_env){
    param <- data.frame(pheno, var_g = vg, var_pheno = vf, h2 = h2, cv = CV, var_gen_env = vga)
  } else {
    param <- data.frame(pheno, var_g = vg, var_pheno = vf, h2 = h2, cv = CV)
  }
  colnames(df_emm) <- c("gen", pheno)
  results <- list(genetic_parameters = param, 
                  adjusted_means = df_emm, 
                  genetic_covariance = cvg, 
                  phenotypic_covariance = cvf, 
                  genetic_correlation = corrg, 
                  phenotypic_correlation = corrf)
  
  return(results)
}

##' Run linear models
run_models <-function(pheno, df, design, multi_env){
  if(design == "DBC"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$block + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$block)
    }
  } else {
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
                  df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$rep/df$block)
    }
  }
  return(mod)
}

##' Elston selection index
##' 
##' @param df data.frame with first column called 'gen' with genotypes identification and follow columns with adjusted mean for each phenotype. 
##' @param k vector with minimum or maximum value for each phenotype evaluated. If NULL the minimum/maximum value is considered.
##' @param increasing vector with column names of phenotypes that low value is advantageous
##' 
elston_index <- function(df, k=NULL, increasing=NULL){
  pheno <- colnames(df)[-1]
  gen <- df[,1]
  df <- df[,-1]
  
  if(is.vector(df)) df <- matrix(t(df))

  # define k
  if(is.null(k)){
    for(i in 1:length(pheno)){
      if(pheno[i] %in% increasing){
        k[i] <- max(df[,i]) # worse value
      } else {
        k[i] <- min(df[,i])
      }
    }
  }
  
  df_elson <- vector()
  for(i in 1:length(pheno)){
    if(pheno[i] %in% increasing){
      df_elson <- c(df_elson, k[i] - df[,i]) 
    } else {
      df_elson <- c(df_elson, df[,i] - k[i])
    }
  }
  
  #df_elson[which(df_elson < 0)] <- 0
  df_elson <- matrix(df_elson, ncol = length(pheno), byrow = F)
  colnames(df_elson) <- pheno
  
  df_index <- data.frame(gen, elson_idx = apply(df_elson, 1, prod)) 
  
  return(df_index)
}

##' Mulamba and Mock (1978) selection index
##' 
##' @param df data.frame with first column called 'gen' with genotypes identification and follow columns with adjusted mean for each phenotype. 
##' @param increasing vector with column names of phenotypes that low value is advantageous
##' @param weights vector of economic weight for each phenotype. If df has 8 columns ('gen' + 7 phenotypes), this vector should have length 7.
mulamba_index <- function(df, increasing = NULL, weights = NULL){
  pheno <- colnames(df)[-1]
  
  if(is.null(weights)) weights <- rep(1,length(pheno))
  
  decreasing <- pheno[!pheno %in% increasing]
  
  # 1 receives best value (higher value)
  for(i in decreasing)
    df[order(df[,i], decreasing = T),i] <- 1:dim(df)[1]
  
  if(!is.null(increasing)){
    # 1 receives best value (low value)
    for(i in increasing)
      df[order(df[,i], decreasing = F),i] <- 1:dim(df)[1]
  }
  
  ranks <- t(t(df[,-1])*weights)
  mulamba_sum <- ranks %>% as.data.frame %>% mutate_if(is.character, as.numeric) %>% rowSums 
  df_mulamba <- data.frame(gen=as.character(df[,1]), mulamba_index = mulamba_sum)
  df_mulamba <- df_mulamba[order(df_mulamba$mulamba_index, decreasing = T),]
  
  return(df_mulamba)
}

##' Smith Hazel index
##' 
##' Is = wYik + wYij
##' 
##' @param adj.means data.frame with first column as the genotypes 
##' names and following columns the adjusted means for each phenotype
##' 
##' @param cvf phenotypic covariance matrix
##' @param cvg genotypic covariance matrix
##' @param weights vector with weight of each phenotype
##' 
smith_hazel <- function(adj.means, cvf, cvg, weights = NULL){
  if(is.null(weights)) weights <- rep(1,dim(adj.means)[2]-1) 
  w <- solve(cvf)%*%cvg%*%weights
  Is = as.matrix(adj.means[,-1])%*%w
  smith_df <- data.frame(gen = adj.means[,1], smith_hazel_index = Is)
  head(smith_df)
}

##' Calculates selection gain
##' 
##' @param herdability number
##' @param pheno data.frame with first column as the genotypes 
##' names and following columns the adjusted means for the evaluated phenotype
##' @param selected_geno genotypes selected
##' 
selection_gain <- function(pheno, selected_geno, herdability){
  selected <- pheno[which(pheno[,1] %in% selected_geno),]
  SD <- mean(selected[,2]) - mean(pheno[,2])
  SG <- herdability*SD
  I <- (length(selected[,1])/length(pheno[,1]))*100
  result <- data.frame(`Selection gain` = SG, `Selection intensity_percentage` = I)
  return(result)
}

## To be copied in the UI
# mod_indices_ui("indices_ui_1")

## To be copied in the server
# callModule(mod_indices_server, "indices_ui_1")

