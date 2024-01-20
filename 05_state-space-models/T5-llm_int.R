## SS local level model with intervention

## clear data & close graphs
rm(list = ls())
graphics.off()

## Install packages
# install.packages('dlm', repos='https://cran.rstudio.com/', dependencies=TRUE)
# devtools::install_github("KevinKotze/tsm")

## We can now make use of the library
library(tsm)
library(dlm)
library(tidyverse)
library(lubridate)
library(sarb2020q1)

# use data from the sarb2020q1 package
dat <- sarb_quarter %>%
  select(date, KBP6006L, KBP6006D) %>%
  mutate(defl = c(0, diff(log(KBP6006L / KBP6006D) * 100, lag = 1))) %>%
  drop_na()

y <- dat$defl
plot.ts(y)

# intervention
lambda <- rep(0,length(y))
lambda[79:80] <- 1
lambda <- ts(lambda, start=c(1960,2), frequency=4)
plot(lambda)

## Build Model
fn <- function(parm){
  mod=dlmModPoly(order=1)+dlmModReg(lambda, addInt=FALSE)
  V(mod)=exp(parm[1])
  diag(W(mod))[1]=exp(parm[2])
  return(mod)
}

## Estimate parameters & generate statistics
fit <- dlmMLE(y, rep(0,2), build=fn, hessian=TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(1)+dlmModReg(lambda, addInt=FALSE))
n.coef <- 2
r.aic <- (2*(loglik)) + 2*(sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2*(loglik)) + (log(length(y)))*(n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

## Kalman Filter & Smoother
filtered <- dlmFilter(y, mod=mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered,sd=FALSE)
mu <- dropFirst(smoothed$s[,1])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]

# Draw graph
par(mfrow=c(2,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(y, col="darkgrey", xlab="", ylab="", lwd=2)
lines(mu , col="black")
legend("topright", legend=c("Observed Deflator","Stochastic level"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")

plot.ts(resids, ylab="", xlab="", col="darkgrey", lwd=2)
abline(h=0)
legend("topright", legend="Residuals", lwd=2, col="darkgrey", bty="n")

# Print results
cat("AIC",r.aic)
cat("BIC",r.bic)
cat("V.variance",obs.error.var)
cat("W.variance",state.error.var)

# Diagnostics
par(mfrow=c(1,1),mar=c(2.2,2.2,1,1),cex=0.8)
hist(resids,prob=TRUE,col="grey",main="",breaks=seq(-4.5,7,length.out=30))
ac(resids)  # acf
Box.test(resids, lag=12, type="Ljung", fitdf=2)  # joint autocorrelation
# No autocorrelation when p-val bigger than 5%
shapiro.test(resids)  # normality
# Residuals normal when p-val bigger than 5%
