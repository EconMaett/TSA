# Tut 2
# Applying the Box-Jenkins approach to GDP growth

# preliminaries ----

# clear data & close graphs
rm(list = ls())
graphics.off()

# install packages
# devtools::install_gitlab('"KevinKotze/sarb2020q1"')
# install.packages("fArma")
# install.packages("forecast")

# load packages
library(fArma)
library(forecast)
library(strucchange)
library(tidyverse)
library(lubridate)
library(tsm)
library(sarb2020q1)

# use data from package ----
gdp <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1),
         grow_lag = lag(growth)) %>%
  drop_na()

# plot data
gdp %>%
  pull(growth) %>%
  plot.ts()

# Check for break point ----

# generate BP statistic
sa_bp <- breakpoints(growth ~ grow_lag, data = gdp, breaks = 5)
summary(sa_bp)  # where the breakpoints are
plot(sa_bp, breaks = 5)

# date that relates to the observation number of the break
gdp %>%
  slice(sa_bp$breakpoint)

# Set new sample
y <- gdp %>%
  filter(date > ymd('1981-01-01')) %>%
  pull(growth)

# Model selection ----

# look at ACF and PACF
y %>% ac() # certainly stationary

# Fit model & check AIC
arma_res <- rep(0, 5)

arma_res[1] <- arima(y, order = c(1, 0, 2))$aic
arma_res[2] <- arima(y, order = c(1, 0, 1))$aic
arma_res[3] <- arima(y, order = c(1, 0, 0))$aic
arma_res[4] <- arima(y, order = c(0, 0, 2))$aic
arma_res[5] <- arima(y, order = c(0, 0, 1))$aic

# arma_res
# min(arma_res)
which(arma_res == min(arma_res))

# smallest value provided by ARMA(1,0)

# Diagnostic checking ----

# Estimate model again
arma <- y %>%
  arima(., order = c(1, 0, 0))

# Consider residuals
par(mfrow = c(1, 1))
arma$residuals %>%
  plot()

arma$residuals %>%
  ac() # No real serial correlation

# Lets generate the Q-stats
Q_stat <- rep(NA, 12) # vector of twelve observations
Q_prob <- rep(NA, 12)

for (i in 6:12) {
  Q_stat[i] <-
    Box.test(arma$residuals,
             lag = i,
             type =  "Ljung-Box",
             fitdf = 2)$statistic
  Q_prob[i] <-
    Box.test(arma$residuals,
             lag = i,
             type =  "Ljung-Box",
             fitdf = 2)$p.value
}

op <- par(mfrow = c(1, 2))
plot(Q_stat, ylab = "", main = 'Q-Statistic')
plot(Q_prob,
     ylab = "",
     ylim = c(0, 1),
     main = 'Probability values')
par(op)
# Not too many problems here :)

# To have a look at t-stats rather than standard errors use the
# fArma package ----

arma_fit1 <- fArma::armaFit( ~ arma(1, 2), data = y, include.mean = FALSE)
summary(arma_fit1)

# the MA(1) MA(2) parameters do not appear to be significant
arma_fit2 <- fArma::armaFit( ~ arma(1, 0), data = y, include.mean = FALSE)
summary(arma_fit2)

# results are not very pretty but many things have been done for us in the
# forecast package ----

auto_mod <-
  forecast::auto.arima(
    y,
    max.p = 2,
    max.q = 2,
    start.p = 0,
    start.q = 0,
    stationary = TRUE,
    seasonal = FALSE,
    allowdrift = FALSE,
    ic = "aic"
  )

auto_mod # AR(1) with intercept

# Generate forecast ----

# To compare forecasts to actual data we can go back over last 3 years
tail(gdp) # show the last few rows

# to exclude the last 12 observations (i.e. 3 years)
y_sub <- gdp %>%
  filter(date > ymd('1981-01-01')) %>%
  slice(1:(n() - 12)) %>%
  pull(growth)

y_act <- gdp %>%
  filter(date > ymd('1981-01-01')) %>%
  slice((n() - 11):n()) %>%
  pull(growth)

# Estimate the models once again
arma10 <- y_sub %>%
  arima(., order = c(1, 0, 0))
arma12 <- y_sub %>%
  arima(., order = c(1, 0, 2))

# Forecast forward
arma10_pred <- predict(arma10, n.ahead = 12)
arma12_pred <- predict(arma12, n.ahead = 12)

par(mfrow = c(1, 1))
plot.ts(y)
lines(arma10_pred$pred, col = "blue", lty = 2)
lines(arma12_pred$pred, col = "green", lty = 2)

# These look fairly poor :(
