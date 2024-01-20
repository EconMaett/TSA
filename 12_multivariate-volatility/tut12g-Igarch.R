# Conditional heteroskedastic processes
# Using intel share price
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# set working directory [getwd()]
setwd('~/git/tsm/ts-12-tut/tut')
#setwd("C:\\Users\\image")

# load package
library(tsm)

# load data and create time series object
# intel monthly returns from 1973 to 2009
dat <- read.csv(file="intel.csv",sep=";")

intel <- log(dat$intc+1)

plot.ts(intel)

mm <- Igarch(intel,include.mean=T) #function(rtn,include.mean=F,volcnt=F)
mm <- Igarch(intel) 

plot(mm$volatility)