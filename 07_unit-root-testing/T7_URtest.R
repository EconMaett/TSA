# Tut 7 - Simulate nonstationary time series and use ADF test for unit root

## clear data & close graphs
rm(list=ls())
graphics.off()

# devtools::install_github("KevinKotze/tsm")

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

plot.ts(rw)

## RANDOM WALK
rw_ct <- ur.df(rw, type='trend', selectlags = c("AIC")) # includes const and time trend
  summary(rw_ct)

rw_t <- ur.df(rw, type='drift', selectlags = c("AIC")) # includes const
  summary(rw_t)

rw_none <- ur.df(rw, type='none', selectlags = c("AIC")) # neither
  summary(rw_none)

# plot residuals
plot.ts(rw_none@res, ylab="Residuals")
abline(h = 0, col = "red")

ac(rw_none@res, max.lag=20)

# Perform general to specific test
gts_ur(rw)

## RANDOM WALK WITH DRIFT
rwd_ct <- ur.df(rw_wd, type='trend', selectlags = c("AIC"))
  summary(rwd_ct)

rwd_t <- ur.df(rw_wd, type='drift', selectlags = c("AIC"))
  summary(rwd_t)

# plot residuals
par(mfrow=c(1,1), mar=c(4.2,4.2,2,1), cex=0.8)
plot.ts(rwd_t@res,ylab="Residuals")
abline(h = 0, col = "red")

ac(rwd_t@res, max.lag=20)

gts_ur(rw_wd)
