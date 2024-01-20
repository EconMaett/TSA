# VAR Model - for US data on gdp and unemployment
# Kevin Kotze

## clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(tsm)
library(vars)

# load data and create time series object ----
dat <- read_csv(file = "blanchQua.csv")

dat %>%
  dplyr::select(-dates) %>%
  plot.ts()

dat %>%
  pull(GDP) %>%
  ac(.)

dat %>%
  pull(U) %>%
  ac(.)

# check unemployment for UR (as there is quite a bit of persistence)
adf_une <- dat %>%
  pull(U) %>%
  ur.df(., type = "trend", selectlags = "AIC")

summary(adf_une)   # able to reject null of unit root

# group data
dat_bv <- dat %>%
  dplyr::select(-dates)

# choose lag length
info_bv <- VARselect(dat_bv, lag.max = 12, type = "const")
info_bv$selection   # All suggest suggests using 2 lags

# estimate model
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
# there are many insignificant variables
# gdp is influenced by past unemployment
# unemployment is influenced by past gdp and unemployment

## perform some diagnostic tests on residuals ----

# testing serial correlation with Portmanteau-Test
bv_serial <-
  serial.test(bv_est, lags.pt = 12, type = "PT.asymptotic")
bv_serial
# p-value greater than 5% indicates absence of serial correlation

plot(bv_serial, names = "GDP")
plot(bv_serial, names = "U")

# testing heteroscedasticity
bv_arch <-
  arch.test(bv_est, lags.multi = 12, multivariate.only = TRUE)
bv_arch
# p-value greater than 5% indicates absence of heteroscedasticity

# testing for normality
bv_norm <- normality.test(bv_est, multivariate.only = TRUE)
bv_norm
# p-value indicates pretty normal distribution

# testing for structural stability
bv_cusum <- stability(bv_est, type = "OLS-CUSUM")
plot(bv_cusum)
# does not break the confidence intervals for structural break

## Granger & instantaneous causality tests ====

bv_cause_gdp <- causality(bv_est, cause = "GDP")
bv_cause_gdp
bv_cause_une <- causality(bv_est, cause = "U")
bv_cause_une
# Null hypothesis of no Granger causality is dismissed in both directions

graphics.off() # close all graphs

## Generate irfs ====

irf_gdp <-
  irf(
    bv_est,
    impulse = "U",
    response = "GDP",
    n.ahead = 40,
    boot = TRUE
  )
plot(irf_gdp) # reponse of gdp to unemploy shocks
# positive shock to unemployment has a negative affect on ouput (lower spending power)

irf_une <-
  irf(
    bv_est,
    impulse = "GDP",
    response = "U",
    n.ahead = 40,
    boot = TRUE
  )
plot(irf_une) # reponse of unemployment to gdp shock
# positive shock to output decreases unemployment by large and persistent amount

irf_une_un <-
  irf(
    bv_est,
    impulse = "U",
    response = "U",
    n.ahead = 40,
    boot = TRUE
  )
plot(irf_une_un)

# generate variance decompositions ----
bv_vardec <- fevd(bv_est, n.ahead = 10)
plot(bv_vardec)
# gdp is largely determined by gdp shocks, while unemployment is influenced by both shocks

graphics.off() # close graphics

## Forecasting with the VAR model ====
predictions <- predict(bv_est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "GDP")
plot(predictions, names = "U")

# Fanchart for forecasts ----
fanchart(predictions, names = "GDP")
fanchart(predictions, names = "U")

