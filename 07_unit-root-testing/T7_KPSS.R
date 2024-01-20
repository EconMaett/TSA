# Tut 7 - Simulate nonstationary time series and test for unit root

## clear data & close graphs
rm(list=ls())
graphics.off()

library(vars)

## SIMULATE A NUMBER OF NON-STATIONARY TIME SERIES PROCESSES

set.seed(123456)

## white noise
e <- rnorm(500)
## pure random walk
rw <- cumsum(e)
## trend
trd <- 1:500
## random walk with drift
rw_wd <- 0.5*trd + cumsum(e)
## deterministic trend and noise
dt <- e + 0.5*trd

e_kpss <- ur.kpss(e, type = "mu", use.lag=NULL) # mu for level stationary
  summary(e_kpss)        # unable to reject null of stationarity

rw_kpss <- ur.kpss(rw, type = "mu", use.lag=NULL)
  summary(rw_kpss)       # able to reject null of stationarity

rw_kpss <- ur.kpss(rw, type = "tau", use.lag=NULL) # tau for trend-strationary
  summary(rw_kpss)       # able to reject null of trend-stationarity
