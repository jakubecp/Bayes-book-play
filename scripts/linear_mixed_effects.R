rm(list = ls())
#Checkpoint
library(checkpoint)
checkpoint (snapshotDate = "2016-04-01", use.knitr = TRUE, 
            auto.install.knitr = TRUE)

#packages 
library(devtools)
library(arm)
library(blmeco) #data for periparusater and others
library(ggplot2)

#Fitting
data(cortbowl)
dat <- cortbowl
mod <- lme4::lmer(log(totCort)~Implant+ days + Implant:days+(1|Ring),data=dat,REML=TRUE) # use REML if you are interested in random effects and ML for fixed effects
mod
round(fixef(mod),3)

lme4::ranef(mod)
