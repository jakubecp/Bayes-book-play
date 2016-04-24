##Likelihood(Chapter 5)
rm(list = ls())
dnorm(x=0.8, mean=1,sd=0.2)*dnorm(x=1.2,mean=1,sd=0.2)*dnorm(x=1.1,mean=1,sd=0.2)

dnorm(x=0.8, mean=1.2,sd=0.4)*dnorm(x=1.2, mean=1.2,sd=0.4)*dnorm(x=1.1, mean=1.2,sd=0.4)

x <- seq(0,2,length=100)
dx <- dnorm(x,mean=1,sd=0.2)
plot(x,dx,type="l",xlab="Weight(kg)",ylab="Density", lwd=2,las=1)

rnorm(5,1,0.2)
pnorm(q=0.8,1,0.2)

qnorm(0.1,1,0.2)

y <- c(0.8,1.2,1.1)
lf <- function(mu,sigma)prod(dnorm(y,mu,sigma))
lf(1,0.2)

##Maximum likelihood method

mu <- seq(0.6,1.4,length=100)
sigma <- seq(0.05,0.6,length=100)
lik <- matrix(nrow=length(mu), ncol=length(sigma))
for (i in 1:length(mu)){
  for (j in 1:length(sigma)){
    lik[i,j] <- lf(mu[i], sigma[j])
  }
}
contour(mu,sigma, lik, nlevels=20,xlab=expression(mu),ylab=expression(sigma), las=1,cex.lab=1.4)

neglf <- function(x)-prod(dnorm(y,x[1],x[2]))
Mlest <- optim(c(1,0.2),neglf)
Mlest$par
points(Mlest$par[1], Mlest$par[2],pch=4)

##The Log Pointwise Predictive Density

mod <- lm(y~1)
nsim <- 10000
bsim <- arm::sim(mod, n.sim=nsim)

pyi <- matrix(nrow=length(y), ncol=nsim)
for(i in 1:nsim) pyi[,i] <- dnorm(y,mean=bsim@coef[i,1],sd=bsim@sigma[i])
mpyi <- apply(pyi,1,mean)
sum(log(mpyi))
