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
  ## round
  param[,-1] <- round(param[,-1],2)
  df_emm[,-1] <- round(df_emm[,-1],2)
  
  results <- list(genetic_parameters = param, 
                  adjusted_means = df_emm, 
                  genetic_covariance = round(cvg,2), 
                  phenotypic_covariance = round(cvf,2), 
                  genetic_correlation = round(corrg,2), 
                  phenotypic_correlation = round(corrf,2))
  
  return(results)
}

##' Run linear models
run_models <-function(pheno, df, design, multi_env){
  if(design == "CRD"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen/df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen)
    }
  } else if(design == "DBC"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$block + df$local + df$gen*df$local)
    } else {
      # mod <- lm(pheno ~ df$gen + df$block)
      mod <- lm(pheno ~ df$block + df$gen)
    }
  } else if(design == "lattice"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
                  df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$rep/df$block)
    }
  }
  else if(design == "RBSP"){
    
    df$block <- as.random(df$block)
    df$time <- as.fixed(df$time)
    df$gen <- as.fixed(df$gen)
    
    if(multi_env){
      # mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
      #             df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$block + df$time + df$time/df$block + df$gen + df$time:df$gen)
      # mod <- lm(pheno ~ block + time + block/time + gen + time:gen)
    }
  }
  else if(design == "CRSP"){
    
    df$rep <- as.random(df$rep)
    df$time <- as.fixed(df$time)
    df$gen <- as.fixed(df$gen)
    
    if(multi_env){
      # mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
      #             df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$time + df$time/df$rep + df$gen + df$time:df$gen)
    }
  }
  return(mod)
}


run_models_sp <-function(pheno, whole_f, split_f, df, design, multi_env){
  if(design == "CRD"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen/df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen)
    }
  } else if(design == "DBC"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$block + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$block)
    }
  } else if(design == "lattice"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
                  df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$rep/df$block)
    }
  }
  else if(design == "RBSP"){
    
    block <- as.random(df$block)
    whole_f <- as.fixed(whole_f)
    split_f <- as.fixed(split_f)
    
    if(multi_env){
      # mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
      #             df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ block + whole_f + whole_f/block + split_f + whole_f:split_f)
      # mod <- lm(pheno ~ block + time + block/time + gen + time:gen)
    }
  }
  else if(design == "CRSP"){
    
    rep <- as.random(df$rep)
    whole_f <- as.fixed(whole_f)
    split_f <- as.fixed(split_f)
    
    if(multi_env){
      # mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
      #             df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ whole_f + whole_f/rep + split_f + whole_f:split_f)
    }
  }
  return(mod)
}


##' Utilities
##' Run linear models - boxcox
#' @import MASS
run_models_boxcox <-function(pheno, df, design, multi_env){
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
    
    df$block <- as.random(df$block)
    df$time <- as.fixed(df$time)
    df$gen <- as.fixed(df$gen)
    
    # if(multi_env){
    #   bc <- boxcox(pheno ~ df$gen + df$local/df$rep/df$block +
    #                  df$local/df$rep + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
    #   lambda <- bc$x[which.max(bc$y)]
    #   pheno_trans <- ((pheno^lambda-1)/lambda)
    #   
    #   mod <- lm(pheno_trans ~ df$gen + df$local/df$rep/df$block +
    #               df$local/df$rep + df$local + df$gen*df$local)
    # } else {
      bc <- boxcox(pheno ~ df$block + df$time + df$block/df$time + df$gen + df$time:df$gen, plotit=F, lam=seq(-3, 3, 1/20))
      lambda <- bc$x[which.max(bc$y)]
      pheno_trans <- ((pheno^lambda-1)/lambda)
      
      mod <- lm(pheno_trans ~ df$block + df$time + df$block/df$time + df$gen + df$time:df$gen)
      #   }
  }
  else if(design == "CRSP") {
    
    df$rep <- as.random(df$rep)
    df$time <- as.fixed(df$time)
    df$gen <- as.fixed(df$gen)
    
    # if(multi_env){
    #   bc <- boxcox(pheno ~ df$gen + df$local/df$rep/df$block +
    #                  df$local/df$rep + df$local + df$gen*df$local, plotit=F, lam=seq(-3, 3, 1/20))
    #   lambda <- bc$x[which.max(bc$y)]
    #   pheno_trans <- ((pheno^lambda-1)/lambda)
    #   
    #   mod <- lm(pheno_trans ~ df$gen + df$local/df$rep/df$block +
    #               df$local/df$rep + df$local + df$gen*df$local)
    # } else {
    bc <- boxcox(pheno ~ df$time + df$time/df$rep + df$gen + df$time:df$gen, plotit=F, lam=seq(-3, 3, 1/20))
    lambda <- bc$x[which.max(bc$y)]
    pheno_trans <- ((pheno^lambda-1)/lambda)
    
    mod <- lm(pheno_trans ~ df$time + df$time/df$rep + df$gen + df$time:df$gen)
    #   }
  }
  return(mod)
}


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
  
  k <- as.numeric(k)
  
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
  weights <- as.numeric(weights)
  
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
  weights <- as.numeric(weights)
  w <- solve(cvf)%*%cvg%*%weights
  Is = as.matrix(adj.means[,-1])%*%w
  smith_df <- data.frame(gen = round(adj.means[,1],2), smith_hazel_index = round(Is,2))
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


##' Safety-first index
##'
##' @param df data.frame  
##' @param trait Random variable 
##' @param design block or lattice
##' @param alpha numeric
##' 
safety_first <- function(df, pheno, design, alpha = 0.95) {
  trait <- parse(text = pheno)
  Envs <- levels(df$local)
  results <- vector("list", length(Envs))
  # Loops Envs
  for (i in Envs) {
    Edat <- droplevels(subset(df, local == i))
    if (design == "block") {
      # design
      mod <- aov(eval(trait) ~ gen + block,
                 data = Edat)
    } else {
      mod <- aov(eval(trait) ~ gen + rep + block,
                 data = Edat)
    }
    temp = data.frame(lsmeans(mod, ~ gen))
    results[[i]] <- data.frame(
      gen = temp$gen,
      trait = temp$lsmean,
      local = factor(levels(Edat$local)),
      local_mean = mean(temp$lsmean)
    )
    rm(Edat, mod, temp)
  }
  # Adj. means
  StageI <- do.call(rbind, results)
  colnames(StageI)[2] <- pheno
  StageI_H <-
    matrix(StageI[,pheno], nlevels(df$gen), nlevels(df$local)) # n_g x n_l
  # Yi. - Z(1 - \alpha)(Vi)^0.5
  Vii <- apply(StageI_H, 1, sd)
  Yi. <- apply(StageI_H, 1, mean)
  # Z tab
  Z <- qnorm(p = alpha)
  # Safety-first index
  Risk_F <- round(Yi. - (Z * Vii), 2)
  Risk_F <- data.frame(Risk_F, ID = levels(df$gen))
  Risk_F <- Risk_F[order(Risk_F$Risk_F, decreasing = TRUE),]
  Risk_F$ID = factor(Risk_F$ID, levels = Risk_F$ID)
  return(Risk_F)
}


##' Reliability index
##' 
##' @param df data.frame
##' @param Ann Annicchiarico output
##' @param pheno character with trait ID
##' 
reliability_index <- function(dat, Ann, pheno){
  # Pij = Yij/Y.j X 100
  envs <- levels(dat$local)
  Pij <- vector("list", length(envs))
  # loop
  for(i in 1:length(envs)){
    edat = droplevels(subset(dat, local == envs[i]))
    mean = mean(edat[,pheno])
    Pij[[i]] = round((tapply(edat[,pheno], edat$gen, mean)/mean)*100,0)
  }
  
  # Pij
  Pij = t(do.call(rbind,Pij))
  
  # Reliability index 
  # Ii = pi. - ZxSi  
  Z = qnorm(p = 0.75)
  I = round(apply(Pij, 1, mean) - (apply(Pij, 1, sd)*Z), 0)
  idx <- data.frame(ID= names(I), index = I)
  rownames(idx) <- NULL
  return(idx)
}
