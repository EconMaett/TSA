# Conditional heteroskedastic processes
# Using intel share price
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(tidyverse)
library(fGarch)
library(tsm)

# load data and create time series object
# intel monthly returns from 1973 to 2009
dat <- read_csv(file = "intel.csv")

intel <- ts((log(dat$intc + 1)), start = c(1973, 1), freq = 12)

plot(intel)
plot(intel ^ 2)

# Investigate persistence in mean
res1 <- ac(intel) # not much here
Box.test(intel, lag = 12, type = 'Ljung') # confirmed

# Investigate persistence in volatility
res2 <- ac(abs(intel)) # cetainly some persistence
res3 <- ac(intel ^ 2)
Box.test(abs(intel), lag = 12, type = 'Ljung') # confirmed

# Test for significant mean
t.test(intel)
at <- intel - mean(intel)
Box.test(at ^ 2, lag = 12, type = 'Ljung') # similar result
archTest(at, 12) # F-stat and p-value confirm ARCH effects

# Model Estimation
# PACF suggests ARCH(3,0) possibly
m0 <- garchFit(intel ~ garch(3, 0), data = intel, trace = F)
summary(m0) # mu = constant in mean equation; alpha = ARCH terms
# alpha2 and alpha3 appear to be insignificant
# Qstat fairly large - no serial correlation in resid

# Consider ARCH(1,0)
m1 <- garchFit(intel ~ garch(1, 0), data = intel, trace = F)
summary(m1) # all coefficients appear significant
# Qstat(10) pval: 0.28 and 0.10 -> suitable model at 5%
# Qstat for some volatility are small ??

m2 <- garchFit(intel ~ garch(1, 1), data = intel, trace = F)
summary(m2) # all coefficients appear significant & Qstats are good

check1 <- ac(residuals(m2, standardize = T)) # good
check2 <- ac((residuals(m2, standardize = T) ^ 2) ^ 0.5) # nice
check3 <- ac((residuals(m2, standardize = T) ^ 2)) # yeah baby!
par(mfrow = c(1, 1))
plot.ts(residuals(m2, standardize = T)) # much improved

# Consider students t-distribution
m3 <- garchFit(
  intel ~ garch(1, 1),
  data = intel,
  cond.dist = "std",
  trace = F
)
summary(m3) # all coefficients appear significant & Qstat is good

# Consider skewed students t-distribution
m4 <- garchFit(
  intel ~ garch(1, 1),
  data = intel,
  cond.dist = "sstd",
  trace = F
)
summary(m4) # all coefficients appear significant and are similar to student-t

# Altough not appropriate for this data this is how to model an ARMA(1,0) + GARCH(1,1)
m5 <- garchFit(intel ~ arma(1, 0) + garch(1, 1),
               data = intel,
               trace = F)
summary(m5) # note the AR(1) parameter is insignificant


# Summarizing the steps to build a ARCH/GARCH model
# 1) specify mean equation
#     - little serial correlation in res1
#     - confirmed by Qstat that has pval of 0.1079 for 12 lags
# 2) use residuals to test for ARCH/GARCH effects
#     - evidence of serial correlation in res2-res4
#     - PACF suggests that ARCH(3) may be appropriate
# 3) estimate mean and volatility equation simultaneously
#     - alpha2 and alpha3 are insignificant so try ARCH(1)
# 4) check fit and refine
#     - confirmed by Qstat that has pval of 0.0 for 12 lags
