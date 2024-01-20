# Tut 2
# Applying the Box-Jenkins approach to inflation data

# preliminaries ----

# clear data & close graphs
rm(list = ls())
graphics.off()

# load packages
library(fArma)
library(forecast)
library(astsa)
library(strucchange)
library(tidyverse)
library(lubridate)
library(tsm)
library(sarb2020q1)

# use data from package ----
dat <- sarb_month %>%
  select(date, KBP7170N) %>%
  mutate(inf_yoy = 100 * ((KBP7170N / lag(KBP7170N, n = 12)) - 1),
         infl_lag = lag(inf_yoy)) %>%
  drop_na()

# plot data
dat %>%
  pull(inf_yoy) %>%
  plot.ts()

# lets start in the year 2000
dat <- dat %>%
  filter(date >= ymd("2000-01-01"))

# plot data
dat %>%
  pull(inf_yoy) %>%
  plot.ts()

# Check for break point ----

# generate BP statistic
sa_bp <- breakpoints(inf_yoy ~ infl_lag, data = dat, breaks = 5)
summary(sa_bp)  # where the breakpoints are
plot(sa_bp, breaks = 5)

# date that relates to the observation number of the break
dat %>%
  slice(sa_bp$breakpoint)

# Set new sample
y <- dat %>%
  filter(date > ymd('2006-12-01')) %>%
  pull(inf_yoy)

# Model selection ----

# look at ACF and PACF
y %>% ac()

# Look at the first autocorrelation coefficient +0.97?
# This could be a unit root, which we would need to test for using ADF, etc.
# Since we are only going to cover these later we are going to proceed as if stationary

# Fit model & check AIC
arma_res <- rep(0, 8)

arma_res[1]  <- arima(y, order = c(2, 0, 4))$aic
arma_res[2]  <- arima(y, order = c(2, 0, 3))$aic
arma_res[3]  <- arima(y, order = c(2, 0, 2))$aic
arma_res[4]  <- arima(y, order = c(2, 0, 1))$aic
arma_res[5]  <- arima(y, order = c(2, 0, 0))$aic
arma_res[6]  <- arima(y, order = c(1, 0, 2))$aic
arma_res[7]  <- arima(y, order = c(1, 0, 1))$aic
arma_res[8] <- arima(y, order = c(1, 0, 0))$aic

# arma_res
# min(arma_res)
which(arma_res == min(arma_res))

# smallest value provided by ARMA(2,3)

# Now you could work with the standard errors to derive t-stats
# But I'm lazy and know that they are conveniently provided in another package

# Diagnostic checking ----

# Estimate model again
arma <- y %>%
  arima(., order = c(2, 0, 3))

# look at the coefficient values - are they significant
arma_fit <- fArma::armaFit(~ arma(2, 3), data = y)
summary(arma_fit)

# Consider residuals
par(mfrow = c(1, 1))
arma$residuals %>%
  plot()

arma$residuals %>%
  ac() # look at lag 12 - this is monthly data for y-o-y inflation

# seasonal arma model ----

sarima_fit <-
  astsa::sarima(y, 2, 0, 3, 1, 0, 1, 12) # sarima(x,p,d,q,P,D,Q,S) will fit SARIMA(2,0,3)*(1,0,1)_{12}

ac(as.numeric(sarima_fit$fit$residuals))

# we have got rid of the seasonal persistence :)
