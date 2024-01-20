## SS local level model with forecast

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

## Confidence Intervals
conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(conf.tmp[-1], start = c(1960, 2), frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

comb.state <- cbind(mu, conf.pos, conf.neg)

## Forecast
forecast <- dlmForecast(filtered, nAhead = 12)
var.2 <- unlist(forecast$Q)
wid.2 <- qnorm(0.05, lower = FALSE) * sqrt(var.2)
comb.fore <- cbind(forecast$f, forecast$f + wid.2, forecast$f - wid.2)

result <-
  ts(rbind(comb.state, comb.fore),
     start = c(1960, 2),
     frequency = 4)

# Draw graph
par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  result,
  col = c("black", "red", "red"),
  plot.type = "single",
  xlab = "",
  ylab = "",
  lty = c(1, 2, 2),
  ylim = c(-2, 8)
)
lines(y , col = "darkgrey", lwd = 1.5)
abline(
  v = c(2019, 4),
  col = 'blue',
  lwd = 1,
  lty = 3
)
legend(
  "topleft",
  legend = c("Observed Deflator", "Stochastic level"),
  lwd = c(1.5, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)
