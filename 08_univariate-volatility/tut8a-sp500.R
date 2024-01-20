# Conditional heteroskedastic processes
# Using S&P 500 excess returns
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(tidyverse)
library(fGarch)
#devtools::install_github("KevinKotze/tsm")
library(tsm)

# load data and create time series object
# sp500 monthly excess returns from 1926 to 1991
dat <- read_csv(file="sp500.csv")

sp500 <- ts(dat$sp500, start = c(1926, 1), freq = 12)

plot(sp500)
plot(sp500^2)

# Investigate mean (serial correlation)
res1 <- ac(sp500) # some possible persistence - AR(3) or MA(3)?
Box.test(sp500, lag = 12, type = 'Ljung')

# Investigate volatility (serial correlation in residual square/hetros)
res2 <- ac(abs(sp500)) # definitely some persistence
res3 <- ac(sp500^2)

# Test for significant mean
t.test(sp500)  # there is a mean value
at <- sp500 - mean(sp500)
Box.test(abs(at), lag = 12, type = 'Ljung') # similar results
archTest(at, 12) # F-stat and p-value confirm ARCH effects

# Model Estimation
# PACF on mean suggests AR(3) or MA(3) possibly
m1 <- arima(sp500, order = c(0, 0, 3))
m1 # MA(3) specification

m2 <- arima(sp500, order = c(3, 0, 0))
m2 # AR(3) specification

# results look similar - more formal testing would be helpful
# proceed with AR(3)
m3 <- garchFit(sp500 ~ arma(3, 0) + garch(1, 1),
               data = sp500,
               trace = F)
summary(m3) # ar(1) - ar(3) appear to be insignificant

m4 <- garchFit(
  sp500 ~ garch(1, 1),
  data = sp500,
  cond.dist = "std",
  trace = F
)
summary(m4) # coefficients significant

check1 <- ac(residuals(m4, standardize = T)) # mean looks fairly good
check2 <- ac((residuals(m4, standardize = T) ^ 2) ^ 0.5) # vol looks good
check3 <- ac((residuals(m4, standardize = T) ^ 2)) # confirm it is good

par(mfrow = c(1, 1))
plot.ts(residuals(m4, standardize = T)) # much improved

predict(m4, 5)
# Forecasts of actual volatility may not be appropriate (Andersen & Bollerslev - 1998)
