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

# newdat <- data.frame (x=seq(10,30,0.1))
# newmodmat <- model.matrix (~x,data=newdat)
# fitmat <- matrix (ncol=nsim,nrow=nrow(newdat))
# for (i in 1:nsim) fitmat[,i] <- newmodmat %*% coef(bsim)[i,]
# plot(x,y)
# abline(mod,lwd=2)
# lines (newdat$x, apply(fitmat,1,quantile, prob=0.025), lty=3)
# lines (newdat$x, apply(fitmat,1,quantile, prob=0.975), lty=3)

newdat <- expand.grid(Implant=levels(dat$Implant),
  days=levels(dat$days))
Xmat <- model.matrix(~Implant+days+Implant:days,data=newdat)
fitmat <- matrix(ncol=nsim,nrow=nrow(newdat))
for (i in 1:nsim) fitmat[,i] <- Xmat%*%bsim@fixef[i,]
newdat$lower <- apply(fitmat,1,quantile,prob=0.025)
newdat$upper <- apply(fitmat,1,quantile,prob=0.975)
newdat$fit <- Xmat %*%fixef(mod)

#MAKE A Plot of newdat$lower + upper + fit + data

#Random intercept and Random slope
rm(list = ls())
data (wingbowl)
dat <- wingbowl
dat$Age.z <- scale (dat$Age)
mod <- lmer(Wing~Age.z+Implant+Age.z:Implant+(Age.z|Ring), data=dat, REML=FALSE)
mod
qqnorm(lme4::ranef(mod)$Ring[,1], main="normal QQ-plot, random effects")
qqline(lme4::ranef(mod)$Ring[,1])

qqnorm(lme4::ranef(mod)$Ring[,2], main="normal QQ-plot, random effects")
qqline(lme4::ranef(mod)$Ring[,2])

nsim <- 2000
bsim <- sim(mod,n.sim=nsim)
apply(bsim@fixef,2, quantile, prob=c(0.025,0.975))
quantile(bsim@fixef[,"Age.z:ImplantP"]/sd(dat$Age), prob=c(0.025,0.975))

newdat <- expand.grid(Age=seq(23,45,length=100),
  Implant=levels(dat$Implant))
newdat
newdat$Age.z <- (newdat$Age-mean(dat$Age))/sd(dat$Age)
Xmat <- model.matrix(~Age.z+Implant+Age.z:Implant,data=newdat)
fitmat <- matrix(ncol=nsim,nrow=nrow(newdat))
for (i in 1:nsim) fitmat[,i] <- Xmat%*%bsim@fixef[i,]
newdat$lower <- apply (fitmat,1, quantile, prob=0.025)
newdat$upper <- apply(fitmat,1,quantile, prob=0.975)

