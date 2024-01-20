# GJR-GARCH Model
# Using intel share price
# Kevin Kotze

# Mean equation: y_t = \mu + \sigma_t \epsilon_t
# Volatility:    \sigma_t^2 = \omega + \alpha (\epsilon_{t-1})^2 + \gamma \lambda_t (\epsilon_{t-1})^2 + \beta \sigma_{t-1}^2
# where \lambda is a dummy variable that is one when \epsilon_{t-1} < 1, hence we expect a postive value for \gamma

## clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(fGarch)

# load data and create time series object
# intel monthly returns from 1973 to 2009
dat <- read_csv(file = "intel.csv")

intel <- ts((log(dat$intc + 1)), start = c(1973, 1), freq = 12)

plot(intel)

mod.intel.n <-
  garchFit(
    ~ aparch(1, 1),
    data = intel,
    trace = F,
    delta = 2,
    include.delta = F
  )
summary(mod.intel.n)

mod.intel.s <-
  garchFit(
    ~ aparch(1, 1),
    data = intel,
    trace = F,
    delta = 2,
    include.delta = F,
    cond.dist = "std"
  )
summary(mod.intel.s)
# gamma not significant - suggests no significant leverage effects for intel share price

## clear data & close graphs
rm(list = ls())
graphics.off()

# load data and create time series object
# ibm monthly returns from 1926 to 2009
dat <- read_csv(file = "ibm.csv")

ibm <- ts((log(dat$ibm + 1) * 100), start = c(1926, 1), freq = 12)

plot(ibm)

mod.ibm.n = garchFit(
  ~ aparch(1, 1),
  data = ibm,
  trace = F,
  delta = 2,
  include.delta = F
)
summary(mod.ibm.n)

mod.ibm.s <-
  garchFit(
    ~ aparch(1, 1),
    data = ibm,
    trace = F,
    delta = 2,
    include.delta = F,
    cond.dist = "std"
  )
summary(mod.ibm.s)
# Perhaps there are possibly leverage effects in IBM data
