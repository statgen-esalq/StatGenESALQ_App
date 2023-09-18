####Split Plot####  

## CRD within Split Plot - CRSPD

library(daewr)
data(splitPdes)
View(splitPdes)
library(GAD)
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
TrayT <- as.fixed(splitPdes$trayT)
model <- aov(y ~ Short + Short%in%Batch + BakeT 
             + TrayT + Short*BakeT + Short*TrayT + BakeT*TrayT 
             + Short*BakeT*TrayT, data = splitPdes)
gad(model)

library(GAD)
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.gad <- aov(y ~ Short + Short%in%Batch + BakeT 
             + Short*BakeT, data = splitPdes)
gad(model.gad)

Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.aov <- aov(y ~ Short + BakeT + Short:BakeT + Error(Short/Batch), data = splitPdes)
summary(model.aov)

Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.lm <- lm(y ~ Short + Short/Batch + BakeT + Short:BakeT, data = splitPdes)
summary(model.lm)
aov(model.lm)
anova(model.lm)
gad(model.lm)


## RBD within Split Plot - RBSPD

library(daewrG)
data(splitPdes)
View(splitPdes)
library(GAD)
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.gad <- aov(y ~ Batch + Short + Short%in%Batch + BakeT 
                 + Short*BakeT, data = splitPdes)
gad(model.gad)

Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.aov <- aov(y ~ Short + BakeT + Short:BakeT + Error(Batch + Batch:Short), data = splitPdes)
summary(model.aov)

Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
model.lm <- lm(y ~ Batch + Short + Batch/Short + BakeT + Short:BakeT, data = splitPdes)
summary(model.lm)
aov(model.lm)
anova(model.lm)
gad(model.lm)
model.lm


run_models_sp <-function(pheno, f_whole, f_split, df, design){
  if(design == "RBSP"){
    mod <- lm(pheno ~ df$block + f_whole + df$block/f_whole + f_split + f_whole:f_split)
  } else if(design == "CRSP"){
    mod <- lm(pheno ~ f_whole + f_whole/df$rep + f_split + f_whole:f_split)
  } 
  return(mod)
}

Time <- as.fixed(Splitplot_02$time)
Block <- as.random(Splitplot_02$block)
Gen <- as.fixed(Splitplot_02$gen)
model.lm1 <- lm(Splitplot_02$pheno1 ~ Block + Time + Block/Time + Gen + Time:Block, data = Splitplot)
summary(model.lm)
aov(model.lm)
anova(model.lm)
gad(model.lm1)

Time <- as.fixed(df$time)
Block <- as.random(df$block)
Gen <- as.fixed(df$gen)
mod <- lm(pheno ~ Block + Time + Block/Time + df$gen + Time:df$gen)

