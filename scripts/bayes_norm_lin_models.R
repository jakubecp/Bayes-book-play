rm(list = ls())
#Checkpoint
checkpoint (snapshotDate = "2016-04-01", use.knitr = TRUE, 
  auto.install.knitr = TRUE)

#packages 
library(devtools)
library(arm)
library(blmeco) #data for periparusater
library(ggplot2)
library(checkpoint)

## Bayes play
## Fitting a linear regression
rm(list = ls())
n <- 50
# set.seed(500) #in case you need same result every time
sigma <-  5
b0 <- 2
b1 <- 0.7
x <- runif (n,10,30)
yhat <- b0 + b1*x
y <- rnorm(n,yhat,sd=sigma)
plot(x,y) #drawing simulated data points
abline(lm(y~x)) # fitting the mean line
segments (x, fitted(lm(y~x)), x, y) #drawing lines between data points and mean line
mod <- lm(y~x)
mod
summary(mod)$sigma

#drawing conclusions
nsim <- 5000
bsim <- arm::sim(mod, n.sim=nsim)
apply (coef(bsim), 2, quantile, prob=c(0.025,0.975))
quantile (bsim@sigma, prob = c(0.025,0.975))
sum(coef(bsim)[,2]>1)/nsim
sum(coef(bsim)[,2]>0.5)/nsim

plot (x,y, pch=16, las=1, cex.lab=1.2)
for (i in 1:nsim) abline (coef(bsim)[i,1], coef(bsim)[i,2], col=rgb(0,0,0,0.05))

newdat <- data.frame (x=seq(10,30,0.1))
newmodmat <- model.matrix (~x,data=newdat)
fitmat <- matrix (ncol=nsim,nrow=nrow(newdat))
for (i in 1:nsim) fitmat[,i] <- newmodmat %*% coef(bsim)[i,]
plot(x,y)
abline(mod,lwd=2)
lines (newdat$x, apply(fitmat,1,quantile, prob=0.025), lty=3)
lines (newdat$x, apply(fitmat,1,quantile, prob=0.975), lty=3)

#prepare matrix for simulated data
newy <- matrix (ncol=nsim, nrow=nrow (newdat))
#for each simulated fitted value, simulate one new y-value
for (i in 1:nsim) newy[,i] <- rnorm(nrow(newdat), mean=fitmat[,i], sd=bsim@sigma[i])
lines (newdat$x, apply(newy,1,quantile, prob=0.025), lty=3)
lines (newdat$x, apply(newy,1,quantile, prob=0.975), lty=3)


##One-Way ANOVA
rm(list = ls())
mu <- 12
sigma <- 2
b1 <- 3
b2 <- -5
n <- 90
group <- factor(rep(c(1,2,3), each=30))

simresid <- rnorm(n, mean=0, sd=sigma)
y <- mu+as.numeric(group=="2")*b1+as.numeric(group=="3")*b2+simresid

group <- factor(group)
mod <- lm(y~group)
summary(mod)$sigma

bsim <- arm::sim(mod, n.sim=1000)
m.g1 <- coef(bsim)[,1]
m.g2 <- coef(bsim)[,1]+coef(bsim)[,2]
m.g3 <- coef(bsim)[,1]+coef(bsim)[,3]

hist (coef(bsim),breaks= 100)

# Two-way ANOVA
data(periparusater)
dat <- periparusater

#model without interactions
mod <- lm(wing~sex+age, data=dat)
mod
summary(mod)$sigma
newdat <- expand.grid(sex=factor(c(1,2)), age=factor(c(3,4)))
newdat$fit <- predict (mod, newdata=newdat) 
newdat$fit <- model.matrix (~sex+age, data=newdat)%*% coef(mod) # or with matrix multiplication

nsim <- 2000
bsim <- arm::sim (mod, n.sim=nsim)
fitmat <- matrix (ncol=nsim, nrow=nrow(newdat))
xmat <- model.matrix (formula (mod)[c(1,3)], data=newdat)
for (i in 1:nsim) fitmat[,i] <- xmat%*% bsim@coef[i,]

newdat$lower <- apply (fitmat, 1, quantile, prob = 0.025)
newdat$upper <- apply (fitmat, 1, quantile, prob = 0.975)

#model with interactions
mod2 <- lm(wing~sex*age, data=dat) #alternative writing wing~sex+age+sex:age or wing~(sex+age)^2

bsim2 <- sim(mod2, n.sim=nsim)
quantile (bsim2@coef[,4], prob=c(0.025,0.5,0.975))
summary(mod2)$sigma #individual variation in wing length

mean(abs(bsim2@coef[,4])>0.3)

coef(mod2)
apply (bsim2@coef,2, quantile, prob=c(0.025,0.975))

quantile (bsim2@coef[,2], prob=c(0.025,0.5,0.975))

quantile (bsim2@coef[,2]+bsim2@coef[,4], prob=c(0.025,0.5,0.975))

sum(bsim2@coef[,2]<0)/nsim #difference of wing length between sexes for juveniles
sum(bsim2@coef[,2]+bsim2@coef[,4]<0)/nsim #difference of wing length between sexes for adults

hist(coef(bsim2), breaks=100)
#use ggplot2 to do the same and colour the factors

## Multiple comparisons nad Post Hoc tests

#follow the guidlines of Geldman and Hill (2007)

## Analysis of Covariance (ANCOVA)
rm(list = ls())
data (ellenberg)
index <- is.element (ellenberg$Species, c("Ap", "Dg"))
dat <- ellenberg[index,]
dat <- droplevels(dat)
str(dat)

mod <- lm (log(Yi.g)~Species+Water, data=dat)

head(model.matrix(mod))

summary(mod)

mod2 <- lm(log(Yi.g)~Species*Water, data=dat)

summary(mod2)

#simulation
nsim <- 2000
bsim <- sim(mod2,n.sim=nsim)
xatcross <- blmeco::crosspoint (coef(bsim)[,1], coef(bsim)[,3], coef(bsim)[,1]+coef(bsim)[,2], coef(bsim)[,3]+coef(bsim)[,4])[,1]
xatcross[xatcross<(-5)] <- -5
th <- hist(xatcross, breaks=seq(-5.5,140.5,by=5))
plot(th$mids,cumsum(th$counts)/nsim, type="l",lwd=2, las=1,
     ylim=c(0,1), ylab="P(Dg>Ap | data", xlab="Average distatnce to ground water(cm)")

## Multiple Regression and Collinearity
rm(list = ls())
data(mdat)
mod <- lm(y~x1+x2,data=mdat)
summary(mod)
cor(mdat[,2:6])

own.graph <- function(x,y) {
  points(x,y,pch=16,col=rgb(1,0.5,0,0.8))
  abline(lm(y~x))
}
pairs(mdat,panel=own.graph)

##Ordered Factors and Contrasts

