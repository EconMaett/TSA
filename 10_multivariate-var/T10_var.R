# VAR Model - for South African data
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# load package ----
library(tidyverse)
library(lubridate)
library(tsm)
library(vars)
library(mFilter)
library(sarb2020q1)

# load data and create time series object ----
# gdp data
gdp <- sarb_quarter %>%
  dplyr::select(date, KBP6006D) %>%
  dplyr::filter(date > ymd("1981-01-01") &
                  date <= ymd("2012-12-01")) %>%
  mutate(gdp = log(KBP6006D))

plot.ts(gdp$gdp)

# inflation
inf <- sarb_month %>%
  dplyr::select(date, KBP7170N) %>%
  mutate(inf_yoy = 100 * ((KBP7170N / lag(KBP7170N, n = 1)) - 1)) %>%
  dplyr::filter(date > ymd("1981-04-01") &
                  date <= ymd("2012-12-01")) %>%
  mutate(q = ymd(paste(year(date),"-", quarter(date),"-01"))) %>%
  group_by(q) %>%
  summarise(inf = mean(inf_yoy)) %>%
  ungroup()

plot.ts(inf$inf)

# interest rates
int <- sarb_month %>%
  dplyr::select(date, KBP1414M) %>%
  dplyr::filter(date > ymd("1981-04-01") &
                  date <= ymd("2012-12-01")) %>%
  mutate(q = ymd(paste(year(date),"-", quarter(date),"-01"))) %>%
  group_by(q) %>%
  summarise(int = mean(KBP1414M)) %>%
  ungroup()

plot.ts(int$int)

# calculate output gap using HP filter ----
hp_cycle <- hpfilter(gdp$gdp, freq=1600)$cycle

gdp <- gdp %>%
  mutate(cycle = hp_cycle)

# look at the persistence in the data ----
gdp %>%
  pull(cycle) %>%
  ac(.)

inf %>%
  pull(inf) %>%
  ac(.)

int %>%
  pull(int) %>%
  ac(.)

# perform unit root tests on output and interest rates ----
gdp %>%
  pull(cycle) %>%
  ur.df(., type = "trend", selectlags = "AIC") %>%
  summary() # able to reject null of unit root

int %>%
  pull(int) %>%
  ur.df(., type = "trend", selectlags = "AIC") %>%
  summary() # able to reject null of unit root

# group data ----
dat_bv <- gdp %>%
  mutate(q = ymd(paste(year(date),"-", quarter(date),"-01"))) %>%
  left_join(inf, by = "q") %>%
  left_join(int, by = "q") %>%
  dplyr::select(date, cycle, inf, int)

# Investigating the relationship between interest rate & inflation ====

dat_bv <- dat_bv %>%
  dplyr::select(-date,-cycle)

# choose lag length
info_bv <- VARselect(dat_bv, lag.max = 12, type = "const")
info_bv$selection   # BIC/SC suggests using 2 lags

# estimate model with seasonal
bv_est <-
  VAR(
    dat_bv,
    p = 2 ,
    type = "const",
    season = 4,
    exog = NULL
  )
summary(bv_est)

# there are still many insignificant variables
# seasonal would appear to be largely insignificant

# estimate model with seasonal
bv_est <-
  VAR(
    dat_bv,
    p = 2 ,
    type = "const",
    season = NULL,
    exog = NULL
  )
summary(bv_est)

# system is stable (roots are eigenvalues)
# results suggest that the inflation is largely dependent upon it's immediate past
# while interest rates are dependent upon it's past 2 observations
# the covariation in error terms is relatively small (although not extremely small)

## perform some diagnostic tests on residuals ----

# testing serial correlation with Portmanteau-Test
bv_serial <-
  serial.test(bv_est, lags.pt = 12, type = "PT.asymptotic")
bv_serial
# p-value greater than 5% indicates absence of serial correlation

plot(bv_serial, names = "int")
plot(bv_serial, names = "inf")

# testing heteroscedasticity
bv_arch <-
  arch.test(bv_est, lags.multi = 12, multivariate.only = TRUE)
bv_arch
# p-value greater than 5% indicates absence of heteroscedasticity - could have a problem here

# testing for normality
bv_norm <- normality.test(bv_est, multivariate.only = TRUE)
bv_norm

# p-value indicates that there could be a problem with skewness and kurtosis

plot(bv_norm)
# mainly relate to interest rates (high volatility at the start and big error around observation 70)
# could use dummies for these outliers, which would be included as exogenous variable

# find outlier
resid <- residuals(bv_est)
par(mfrow = c(1, 1))
plot.ts(resid[, 1]) # somewhere between obs 60 and 80
plot.ts(resid[60:80, 1])
resid[60:80, 1] # observation 68 and 69
plot.ts(resid[, 2]) # somewhere between obs 60 and 80
plot.ts(resid[60:80, 2])
resid[60:80, 2] # observation 68 and 69

dum <- rep(0, dim(dat_bv)[1])
dum[68] <- 1
dum[69] <- 1
plot.ts(dum)

bv_estdum <-
  VAR(
    dat_bv,
    p = 2,
    type = "const",
    season = 4,
    exog = dum
  )
bv_serialdum <-
  serial.test(bv_estdum, lags.pt = 12, type = "PT.asymptotic")
plot(bv_serialdum)
bv_normdum <- normality.test(bv_estdum, multivariate.only = TRUE)
bv_normdum # makes very little difference

# testing for structural stability
bv_cusum <- stability(bv_est, type = "OLS-CUSUM")

plot(bv_cusum)
# does not break the confidence intervals for structural break

## Granger & instantaneous causality tests ====

bv_cause_gdp <- causality(bv_est, cause = "int")
bv_cause_gdp
bv_cause_une <- causality(bv_est, cause = "inf")
bv_cause_une
# Granger causality is not dismissed in both directions
# But there appears to be little instanteous causality

graphics.off() # close all graphs

## Generate irfs ====

irf_int <-
  irf(
    bv_est,
    response = "int",
    n.ahead = 40,
    boot = TRUE
  )
plot(irf_int) # response of interest rates to shocks
# interest rates respond positively to interest rate and inflation shocks

irf_inf <-
  irf(
    bv_est,
    response = "inf",
    n.ahead = 40,
    boot = TRUE
  )
plot(irf_inf) # response of inflation shocks
# inflation responds positively to interest rate and inflation shocks

# Price puzzle - this is not what we wanted to see:
# Dynamics may be more involved than standard VAR allows for
# Or we may need more than two variables

# generate variance decompositions ----
bv_vardec <- fevd(bv_est, n.ahead = 10)
plot(bv_vardec)
# interest rates are really only determined by interest rate shocks
# while inflation is influenced by interest rate shocks to a certain degree

graphics.off() # close graphics

## Forecasting with the VAR model ====
predictions <- predict(bv_est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "int")
plot(predictions, names = "inf")

# Fanchart for forecasts ----
fanchart(predictions, names = "int")
fanchart(predictions, names = "inf")

