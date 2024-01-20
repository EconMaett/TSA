## SS local level model with seasonal

## clear infa & close graphs
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
      mod=dlmModPoly(order=1)+dlmModSeas(frequency=4)
      V(mod)=exp(parm[1])
      diag(W(mod))[1:2]=exp(parm[2:3])
      return(mod)
      }

## Estimate parameters & generate statistics
fit <- dlmMLE(inf, rep(0,3), build=fn, hessian=TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(inf, dlmModPoly(1)+dlmModSeas(4))

n.coef <- 3
r.aic <- (2*(loglik)) + 2*(sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2*(loglik)) + (log(length(inf)))*(n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))

## Kalman Filter & Smoother
filtered <- dlmFilter(inf, mod=mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered,sd=FALSE)
mu <- dropFirst(smoothed$s[,1])
gammas <- dropFirst(smoothed$s[,2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
gammas.1 <- gammas[1]
gammas.end <- gammas[length(mu)]


# Draw combined graph
alpha <- mu+gammas
par(mfrow=c(1,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(inf, col="darkgrey", xlab="", ylab="", lwd=2)
lines(alpha , col="black")
legend("topright", legend=c("Observed Deflator","State Components"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")

# Draw separate graphs
par(mfrow=c(3,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(inf, col="darkgrey", xlab="", ylab="", lwd=2)
lines(mu , col="black")
legend("topright", legend=c("Observed Deflator","Stochastic level"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")

plot.ts(gammas, col="darkgrey", xlab="", ylab="", lwd=2)
legend("topright", legend="Seasonal", lwd=2, col="darkgrey", bty="n")

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
