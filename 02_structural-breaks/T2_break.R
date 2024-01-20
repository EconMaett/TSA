# Tut 2
# Testing for structural breaks

# clear data & close graphs
rm(list = ls())
graphics.off()

# load library
library(strucchange)
library(sarbcurrent)
library(tidyverse)
library(lubridate)

# Generate an AR(1) and white noise time series process with 100 observations

set.seed(123)  # setting the seed so we all get the same results
x1 <-
  arima.sim(model = list(ar = 0.9), n = 100) # simulate 100 obs for AR(1)
x2 <-
  arima.sim(model = list(ma = 0.1), n = 100) # simulate 100 obs for MA(1)
x3 <-
  arima.sim(model = list(ar = 0.5, ma = 0.3), n = 100) # simulate 100 obs for MA(1)

# concatenate the time series to create a structural break at observation 101
y <- c((1 + x1),
       x2,
       (0.5 - x3))
plot.ts(y)

# we are going to use an AR(1) so we create database for variables
dat <- tibble(ylag0 = y,
              ylag1 = lag(y)) %>%
  drop_na()

# generate QLR statistic
qlr <- Fstats(ylag0 ~ ylag1, data = dat)
breakpoints(qlr)  # where the breakpoint is
sctest(qlr, type = "supF")  # the significance of this breakpoint
plot(qlr)

# generate cusum statistic
cusum <- efp(ylag0 ~ ylag1, type = "OLS-CUSUM", data = dat)
plot(cusum)

# generate BP statistic
bp <- breakpoints(ylag0 ~ ylag1, data = dat, breaks = 5)
summary(bp)  # where the breakpoint is
plot(bp, breaks = 5)

# South African GDP conditional on the use of AR(1) model
sa_dat <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1),
         grow_lag = lag(growth)) %>%
  drop_na()

# generate QLR statistic
sa_qlr <- Fstats(growth ~ grow_lag, data = sa_dat)
breakpoints(sa_qlr)  # where the breakpoint is
sctest(sa_qlr, type = "supF")  # the significance of this breakpoint
plot(sa_qlr)

# find the date that relates to the observation number of the break
sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1),
         grow_lag = lag(growth)) %>%
  drop_na() %>%
  slice(sa_qlr$breakpoint)

# generate BP statistic
sa_bp <- breakpoints(growth ~ grow_lag, data = sa_dat, breaks = 5)
summary(sa_bp)  # where the breakpoints are
plot(sa_bp, breaks = 5)

# date that relates to the observation number of the break
sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1),
         grow_lag = lag(growth)) %>%
  drop_na() %>%
  slice(sa_bp$breakpoint)

# generate cusum statistic
sa_cusum <-
  efp(growth ~ grow_lag, data = sa_dat, type = "OLS-CUSUM")
plot(sa_cusum)
