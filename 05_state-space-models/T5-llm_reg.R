## SS local level model with seasonal

## clear data & close graphs
rm(list = ls())
graphics.off()

## We can now make use of the library
library(tsm)
library(dlm)
library(tidyverse)
library(lubridate)

## change directory

## set datapath and get data
dat_tmp <- read_csv(file = "spirits.csv") # for excel data use `readxl` package

spirit <-
  ts(
    dat_tmp$spirits,
    start = c(1871, 1),
    end = c(1939, 1),
    frequency = 1
  )

price <-
  ts(
    dat_tmp$price,
    start = c(1871, 1),
    end = c(1939, 1),
    frequency = 1
  )

plot(ts.union(spirit, price), plot.type = "single")
lines(price, col = "red")

## Build Model
fn <- function(parm) {
  mod = dlmModPoly(order = 1) + dlmModReg(price, addInt = FALSE)
  V(mod) = exp(parm[1])
  diag(W(mod))[1:2] = exp(parm[2:3])
  return(mod)
}

## Estimate parameters & generate statistics
fit <- dlmMLE(spirit, rep(0, 3), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <-
  dlmLL(spirit, dlmModPoly(1) + dlmModReg(price, addInt = FALSE))

n.coef <- 3
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(spirit))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))

## Kalman Filter & Smoother
filtered <- dlmFilter(spirit, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s[, 1])
betas <- dropFirst(smoothed$s[, 2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
beta.1 <- betas[1]
beta.end <- betas[length(mu)]

# Draw separate graphs
par(mfrow = c(3, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  ts.union(spirit, mu),
  col = c("darkgrey", "black"),
  plot.type = "single",
  xlab = "",
  ylab = "",
  lwd = c(1.5, 1)
)
legend(
  "right",
  legend = c("Observed Deflator", "Stochastic level"),
  lwd = c(2, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)

plot.ts(
  betas,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 1.5
)
legend(
  "topright",
  legend = "Coefficient",
  lwd = 1.5,
  col = "darkgrey",
  bty = "n"
)

plot.ts(
  resids,
  ylab = "",
  xlab = "",
  col = "darkgrey",
  lwd = 1.5
)
abline(h = 0)
legend(
  "topright",
  legend = "Residuals",
  lwd = 1.5,
  col = "darkgrey",
  bty = "n"
)

# Print results
print(r.aic)
print(r.bic)
print(obs.error.var)
print(state.error.var)

# Diagnostics
ac(resids)  # acf
Box.test(resids,
         lag = 12,
         type = "Ljung",
         fitdf = 2)  # joint autocorrelation
# No autocorrelation when p-val bigger than 5%
shapiro.test(resids)  # normality
# Residuals normal when p-val bigger than 5%
