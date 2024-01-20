# Tut 7 - Testing 'Dornbusch Overshooting' with ADF
# PPP suggests e = p - pf + d, where d represents deviations from PPP (which should be temporary)
# for PPP to hold, d should be stationary (i.e. not be a random walk)

## clear data & close graphs
rm(list=ls())
graphics.off()

library(tidyverse)
library(vars)
library(tsm)

# load data and create time series object
dat <- read_csv(file = "reer.csv")
aus <- ts(dat$Australia, start = c(1980, 1), frequency = 4)
can <- ts(dat$Canada, start = c(1980, 1), frequency = 4)
fra <- ts(dat$France, start = c(1980, 1), frequency = 4)
ger <- ts(dat$Germany, start = c(1980, 1), frequency = 4)
uk <- ts(dat$UK, start = c(1980, 1), frequency = 4)
us <- ts(dat$US, start = c(1980, 1), frequency = 4)

# plot data
plot(cbind(uk, us, ger), plot.type='single', col = c("blue", "green", "red"), ylab ="")
legend("bottomright", legend = c("uk","us","ger"), lty = 1, col = c("blue", "green", "red"), bty="n")
# Each series seems to meander like a random walk, little evidence of explosive series or deterministic trend

ac(uk, max.lag=20)
ac(us, max.lag=20)
ac(ger, max.lag=20)

# calculate DF test statistics
# theory of PPP doesn't allow for deterministic trend (but could incl. constant)

gts_ur(log(ger))
dfc_ger <- ur.df(log(ger), type='drift', selectlags = c("AIC"))
  summary(dfc_ger)
  plot(dfc_ger)
  # able to reject null of unit root

gts_ur(log(aus))
dfc_aus <- ur.df(log(aus), type='drift', selectlags = c("AIC"))
  summary(dfc_aus)
  plot(dfc_aus)
  # cannot reject null of unit root

dfc_aus <- ur.df(log(aus), type='none', selectlags = c("AIC"))
  summary(dfc_aus)
  plot(dfc_aus)
  # cannot reject null of unit root

gts_ur(log(can))
dfc_can <- ur.df(log(can), type='drift', selectlags = c("AIC"))
  summary(dfc_can)
  plot(dfc_can)
  # cannot reject null of unit root

dfc_can <- ur.df(log(can), type='none', selectlags = c("AIC"))
  summary(dfc_can)
  plot(dfc_can)
  # cannot reject null of unit root

gts_ur(log(fra))
dfc_fra <- ur.df(log(fra), type='drift', selectlags = c("AIC"))
  summary(dfc_fra)
  plot(dfc_fra)
  # cannot reject null of unit root

dfc_fra <- ur.df(log(fra), type='none', selectlags = c("AIC"))
  summary(dfc_fra)
  plot(dfc_fra)
  # cannot reject null of unit root
