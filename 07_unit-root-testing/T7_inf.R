# Tut 7 - Testing South African inflation

## clear data & close graphs
rm(list=ls())
graphics.off()

#devtools::install_github("KevinKotze/tsm")

## packages and routines
library(tidyverse)
library(lubridate)
library(vars)
library(tsm)
library(sarb2020q1)

# load data and create time series object
inf <- sarb_month %>%
  dplyr::select(date, KBP7170N) %>%
  mutate(inf_yoy = 100 * ((KBP7170N / lag(KBP7170N, n = 12)) - 1),
         infl_lag = lag(inf_yoy)) %>%
  filter(date >= ymd("2002-12-01")) %>%
  filter(date <= ymd("2017-04-01")) %>%
  drop_na()

# plot data
inf %>%
  pull(inf_yoy) %>%
  plot.ts()

# look at autocorrelation function
inf %>%
  pull(inf_yoy) %>%
  ac()

## ADF TESTS
adf_inf <- inf %>%
  pull(inf_yoy) %>%
  ur.df(., type='trend', selectlags=c("AIC"))

summary(adf_inf) # unable to reject null but must exclude deterministic terms

adf_inf <-  inf %>%
  pull(inf_yoy) %>%
  ur.df(., type='drift', selectlags=c("AIC"))

summary(adf_inf) # unable to reject null but must exclude deterministic term

adf_inf <-  inf %>%
  pull(inf_yoy) %>%
  ur.df(., type='drift', selectlags=c("AIC"))

summary(adf_inf) # unable to reject null

inf %>%
  pull(inf_yoy) %>%
  gts_ur() # for the full sample we are able to reject the null of a unit root

## ZA TESTS
za_inf <- inf %>%
  pull(inf_yoy) %>%
  ur.za(., model="both", lag = 1)

summary(za_inf)
plot(za_inf) # breaking the significance levels in the mid-sample

inf %>%
  slice(70)  # what is the date for observation 70

## KPSS TEST
kpss_inf <- inf %>%
  pull(inf_yoy) %>%
  ur.kpss(., type="mu", use.lag=NULL) # mu for level stationary

summary(kpss_inf)       # unable to reject null of stationarity
