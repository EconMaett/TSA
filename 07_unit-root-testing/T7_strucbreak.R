# Tut 7 - Simulate nonstationary time series with structural break and test for unit root

## clear data & close graphs
rm(list=ls())
graphics.off()

#devtools::install_github("KevinKotze/tsm")

## packages and routines
library(vars)
library(tsm)

## SIMULATE A NUMBER OF NON-STATIONARY TIME SERIES PROCESSES

set.seed(123456)
e <- rnorm(500)
## pure random walk
rw <- cumsum(e)
## trend
trd <- 1:500
## random walk with drift
rw_wd <- 0.5*trd + cumsum(e)
## deterministic trend and noise
dt <- e + 0.5*trd
## ar(1) model
ar1 <- arima.sim(list(order=c(1,0,0), ar=0.7), n=500)
## introduce structural break
S <- c(rep(0,249), rep(1,251))

## random walk plus structural break
y_st <- 15*S + rw

## ar(1) plus strucutral break
x_st <- ar1+15*S

## white noise plus strucutral break
e_st <- e+15*S

plot.ts(cbind(y_st, x_st, e_st), main="")

## ADF tests for unit root
gts_ur(e_st)

est.ct <- ur.df(e_st, type='trend', selectlags = c("AIC"))
  summary(est.ct)

est.c <- ur.df(e_st, type='drift', selectlags = c("AIC"))
  summary(est.c)

est_n <- ur.df(e_st, type='none', selectlags = c("AIC"))
  summary(est_n)

## Regress out the break
resids <- (lm(e_st ~ S -1))$residuals

res_ar1 <- (lm(x_st ~ S -1))$residuals

# test residuals with ADF
est_n <- ur.df(resids, type='none', selectlags = c("AIC"))
  summary(est_n)

# test residuals with Phillips Perron
est_pp <- ur.pp(resids, type='Z-tau', model='trend', lags='long')
  summary(est_pp)

est_ar1pp <- ur.pp(res_ar1, type='Z-tau', model='trend', lags='long')
  summary(est_ar1pp)

## ZIVOT-ANDREWS
za_est <- ur.za(e_st, model = "both", lag = 0)  # white noise
  summary(za_est)   # reject null of unit root test = -21.63 and 5% crit = -5.08
  plot(za_est)      # breaks all critical lines for structural break

za_ar <- ur.za(x_st, model = "both", lag = 0)  # ar1 stationary process
  summary(za_ar)    # cannot reject null of unit root test = -3.39 and 5% crit = -5.08
  plot(za_ar)       # no clear breaks of the lines

za_rw <- ur.za(rw, model = "both", lag = 0)    # random-walk
  summary(za_rw)    # cannot reject null of unit root test = -3.39 and 5% crit = -5.08
  plot(za_rw)       # no clear breaks of the lines
