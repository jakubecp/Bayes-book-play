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
mod <- lme4::lmer(log(totCort)~Implant+ days + Implant:days+(1|Ring),data=dat,REML=TRUE) # use REML if you are interested in random effects and ML for fixed effects (REML=FALSE)
mod
round(fixef(mod),3)

lme4::ranef(mod)

#Assessing model assumptions
X11()
par(mfrow=c(2,2))
scatter.smooth(fitted(mod), resid(mod))
abline(h=0,lty=2)
title("Tukey-Anscombe Plot") #residual vs fitted

qqnorm(resid(mod), main="normal QQ-plot, residuals")
qqline(resid(mod))
scatter.smooth(fitted(mod),sqrt(abs(resid(mod))))

qqnorm(lme4::ranef(mod)$Ring[,1], main="normal QQ-plot, random effects")
qqline(lme4::ranef(mod)$Ring[,1])

#Drawing conclusions
nsim <- 2000
bsim <- sim(mod,n.sim=nsim)
str(bsim)
round(apply(bsim@fixef,2, quantile,prob=c(0.025,0.5,0.975)),3)

newdat <- expand.grid(Implant=factor(c("C","P"),
                                     levels=levels(dat$Implant)),
                      days=factor(c(1,3,21),levels=levels(dat$days)))
Xmat <- model.matrix(~Implant+days+Implant:days,data=newdat)
fitmat <- matrix(ncol=nsim,nrow=nrow(newdat))
#SOME PROBLEM FROM HERE DOWN
for(i in 1:nsim) fitmat[,i] <- Xmat%*%bsim@fixef[i,]
newdat$lower <- apply(fitmat,1,quantile,prob=0.025)
newdat$upper <- apply(fitmat,1,quantile,prob=0.975)
newdat$fit <- Xmat %*%fixef(mod)
