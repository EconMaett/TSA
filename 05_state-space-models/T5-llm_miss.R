## SS local level model with missing data

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

## Remove some data
y.mis <- y
y.mis[70:82] <- NA
plot(y.mis)


## Build Model
fn <- function(parm) {
  dlmModPoly(order = 1,
             dV = exp(parm[1]),
             dW = exp(parm[2]))
}

## Estimate parameters & generate statistics
fit <- dlmMLE(y.mis, rep(0, 2), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y.mis, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

## Kalman Filter & Smoother
filtered <- dlmFilter(y.mis, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)

## Confidence Intervals
conf.tmp <- dlmSvd2var(smoothed$U.S, smoothed$D.S)
conf <- ts(as.numeric(conf.tmp)[-1],
           start = c(1960, 2),
           frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

# Print results
cat("AIC", r.aic)
cat("BIC", r.bic)
cat("V.variance", obs.error.var)
cat("W.variance", state.error.var)

comb.state <- cbind(mu, conf.pos, conf.neg, y.mis)

# Draw graph
par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  comb.state,
  col = c("black", "red", "red", "grey"),
  plot.type = "single",
  xlab = "",
  ylab = "",
  lty = c(1, 2, 2, 3),
  ylim = c(-2, 8)
)
legend(
  "topright",
  legend = c("Observed Deflator", "Stochastic level", "Confidence"),
  lwd = c(1.5, 1),
  col = c("darkgrey", "black", "red"),
  bty = "n"
)
