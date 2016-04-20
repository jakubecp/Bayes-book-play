#bayes play
## Fitting a linear regression
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
library(arm)
nsim <- 1000
bsim <- sim(mod, n.sim=nsim)
apply (coef(bsim), 2, quantile, prob=c(0.025,0.975))
quantile (bsim@sigma, prob = c(0.025,0.975))
sum(coef(bsim)[,2]>1)/nsim
sum(coef(bsim)[,2]>0.5)/nsim

plot (x,y, pch=16, las=1, cex.lab=1.2)
for (i in 1:nsim) abline (coef(bsim)[i,1], coef(bsim)[i,2], col=rgb(0,0,0,0.05))

str(bsim)
