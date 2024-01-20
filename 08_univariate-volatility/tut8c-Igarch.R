# Conditional heteroskedastic processes
# Using intel share price
# Kevin Kotze

## clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(tsm)

# load data and create time series object
# intel monthly returns from 1973 to 2009
dat <- read_csv(file = "intel.csv")

intel <- log(dat$intc + 1)

plot.ts(intel)

mm <-
  Igarch(intel, include.mean = T) #function(rtn,include.mean=F,volcnt=F)
mm <- Igarch(intel)

plot(mm$volatility)
