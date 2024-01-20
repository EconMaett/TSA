## SS local level trend model

## clear data & close graphs
rm(list=ls())
graphics.off()

## Install packages
# install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)
# install.packages('dlm', repos='https://cran.rstudio.com/', dependencies=TRUE)
# library(devtools)
# devtools::install_github("KevinKotze/tsm")

## We can now make use of the library
library('tsm')
library('dlm')

## set datapath and get data
dat <- sarb_quarter$KBP6006L/sarb_quarter$KBP6006D
dat.tmp <- diff(log(na.omit(dat))*100, lag=1)
head(dat)
inf <- ts(dat.tmp, start=c(1960,2), frequency=4)
plot.ts(inf)

## Build Model
fn <- function(parm){
      dlmModPoly(order=2, dV=exp(parm[1]), dW=exp(parm[2:3]))
      }

## Estimate parameters & generate statistics
fit <- dlmMLE(inf, rep(0,3), build=fn, hessian=TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(inf, dlmModPoly(2))
n.coef <- 3
r.aic <- (2*(loglik))+2*(sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2*(loglik)) + (log(length(inf)))*(n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))

## Kalman Filter & Smoother
filtered <- dlmFilter(inf, mod=mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered,sd=FALSE)
mu <- dropFirst(smoothed$s[,1])
upsilon <- dropFirst(smoothed$s[,2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
ups.1 <- upsilon[1]
ups.end <- upsilon[length(mu)]


# Draw graph
par(mfrow=c(3,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(inf, col="darkgrey", xlab="", ylab="", lwd=2)
lines(mu , col="black")
legend("topright", legend=c("Observed Deflator","Stochastic level"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")

plot.ts(upsilon, col="darkgrey", xlab="", ylab="", lwd=2)
legend("topright", legend="Slope", lwd=2, col="darkgrey", bty="n")

plot.ts(resids, ylab="", xlab="", col="darkgrey", lwd=2)
abline(h=0)
legend("topright", legend="Residuals", lwd=2, col="darkgrey", bty="n")

# Print results
cat("AIC",r.aic)
cat("BIC",r.bic)
cat("V.variance",obs.error.var)
cat("W.variance",state.error.var)

# Diagnostics
ac(resids)  # acf
Box.test(resids, lag=12, type="Ljung", fitdf=2)  # joint autocorrelation
# No autocorrelation when p-val bigger than 5%
shapiro.test(resids)  # normality
# Residuals normal when p-val bigger than 5%
