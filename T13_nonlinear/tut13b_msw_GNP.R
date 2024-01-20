# Markov-Switching Model
# Using US GDP data
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

library(MSwM)

# Set directory and read data ----
setwd('~/git/tsm/ts-13-tut')
#setwd("C:\\Users\\image")

# Load data and create time series object ----
dat=read.csv('gnp_us.csv') # US data from Hamilton
gnp=ts(dat$gnp,start=c(1947,1),freq=4) # US gnp
rec=ts(dat$recess,start=c(1947,1),freq=4) # US recessions from dating commission

gnp.g=ts(diff(log(dat$gnp))*100,start=c(1947,2),freq=4)
grow=window(gnp.g,start=c(1952,1),end=c(1984,4),freq=4)
reces=window(rec,start=c(1953,1),end=c(1984,4),freq=4)

# Fit MSW model ----
grow.lag0 = window(grow,start=c(1953,1),end=c(1984,4),freq=4) # get al the lags right
grow.lag1 = window(grow,start=c(1952,4),end=c(1984,3),freq=4)
grow.lag2 = window(grow,start=c(1952,3),end=c(1984,2),freq=4)
grow.lag3 = window(grow,start=c(1952,2),end=c(1984,1),freq=4)
grow.lag4 = window(grow,start=c(1952,1),end=c(1983,4),freq=4)

model=lm(grow.lag0~grow.lag1+grow.lag2+grow.lag3+grow.lag4-1) # linear model

mod=msmFit(model,k=2,sw=rep(TRUE,5)) # msw model
summary(mod) # regression output

plotProb(mod, which=1) # plot fixed probabilities with smoothed probability
plotProb(mod, which=2) # plot Price against the two regimes
plotDiag(mod)
plotReg(mod, expl="grow.lag1")

#slotNames(mod@Fit)
prob=(mod@Fit@smoProb) # retrieve the smoothed probability
prob.reces = ts(prob[,1],start=c(1953,1),end=c(1984,4),freq=4)

plot(reces,type="h",lwd = 10,col="grey") # plot NBER recessions
lines(prob.reces,col=4) # include smoothed probabilities
