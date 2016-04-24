rm(list = ls())
#Checkpoint
library(checkpoint)
checkpoint (snapshotDate = "2016-04-01", use.knitr = TRUE, 
  auto.install.knitr = TRUE)

#packages 
library(devtools)
library(arm)
library(blmeco) #data for periparusater
library(ggplot2)
library(nlme)

##Assessing Model Assumptions (Chapter 6)
data(periparusater)
dat <- periparusater
mod <- lm(wing~sex+age,data=dat)
par (mfrow=c(2,2))
plot(mod)
names (dat)

plot(resid(mod)~country,data=dat)
plot(resid(mod)~age,data=dat)
plot(resid(mod)~sex,data=dat)
plot(resid(mod)~weight,data=dat)

table(dat$country)
par (mfrow=c(3,3))
X11()
compareqqnorm(mod)

#Temporal autocorrelation

data(parusmajor)
dat <- parusmajor
str(dat)
dat <- dat[order(dat$year,dat$julian),]
t.poly.jul <- poly(dat$julian,degree=2)
dat$julian.l <- t.poly.jul[,1]
dat$julian.q <- t.poly.jul[,2]
mod <- lm(count~year+julian.l+julian.q,data=dat)
par(mfrow=c(1,2))
acf(resid(mod))
acf(resid(mod),type="p")

mod <- nlme::gls(count~year+julian.l+julian.q,data=dat,correlation=corAR1())
summary(mod)

#Spatial autocorrelation

data(frogs)
frogs$year.z <- scale(frogs$year)
frogs$elevation.z <- scale(frogs$elevation)
frogs$waterarea[frogs$waterarea==0] <- 0.25
frogs$waterarea.sqrt.l <- log(sqrt(frogs$waterarea))
frogs$waterarea.sqrt.l.z <- scale(frogs$waterarea.sqrt.l)

mod <- MASS::glm.nb (count2~elevation.z+year.z+fish+vegetation+waterarea.sqrt.l.z+fish:vegetation,data=frogs)
par (mfrow=c(2,2))
plot(mod)
