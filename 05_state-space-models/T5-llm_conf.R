## SS local level model with confidence intervals

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
   dplyr::filter(date > '1981-01-01')

y <- dat$defl
plot.ts(y)

## Build Model
fn <- function(parm) {
   dlmModPoly(order = 1,
              dV = exp(parm[1]),
              dW = exp(parm[2]))
}

## Estimate parameters & generate statistics
fit <- dlmMLE(y, rep(0, 2), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

## Kalman Filter & Smoother
filtered <- dlmFilter(y, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]

## Confidence Intervals
conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(as.numeric(conf.tmp)[-1],
           start = c(1960, 2),
           frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

## Prediction errors
mu.f <- dropFirst(filtered$a)
cov.tmp <- unlist(dlmSvd2var(filtered$U.R, filtered$D.R))
if (sum(dim(mod$FF)) == 2) {
   variance <- cov.tmp + as.numeric(V(mod))
} else {
   variance <-
      (sapply(cov.tmp, function(x)
         mod$FF %*% x %*% t(mod$FF))) + V(mod)
}

# Draw graph - confidence intervals
par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
   y,
   col = "darkgrey",
   xlab = "",
   ylab = "",
   lwd = 1.5
)
lines(mu, col = "black")
lines(as.numeric(conf.pos) , col = "red")
lines(as.numeric(conf.neg), col = "red")
legend(
   "topright",
   legend = c("Observed Deflator", "Stochastic level", "Confidence Interval"),
   lwd = c(1.5, 1, 1),
   col = c("darkgrey", "black", "red"),
   bty = "n"
)

# Draw graph - prediction errors and variances
par(mfrow = c(2, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
   mu.f,
   col = "darkgrey",
   xlab = "",
   ylab = "",
   lwd = 1.5
)
lines(mu , col = "black")
legend(
   "topright",
   legend = c("Filtered State", "Smoothed State"),
   lwd = c(1.5, 1),
   col = c("darkgrey", "black"),
   bty = "n"
)

plot.ts(
   variance,
   col = "darkgrey",
   xlab = "",
   ylab = "",
   lwd = 1.5
)
legend(
   "topright",
   legend = "Variance",
   lwd = 2,
   col = "darkgrey",
   bty = "n"
)
