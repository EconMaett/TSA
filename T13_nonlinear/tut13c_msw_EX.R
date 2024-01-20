# Markov-Switching Model
# ZARUSD data
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

library(MSwM)

# Set directory and read data ----
setwd('~/git/tsm/ts-13-tut')
#setwd("C:\\Users\\image")

# Load data and create time series object ----
dat=read.csv('zar_usd_month.csv',sep=";")

exch<- ts(diff(log(dat$zarusd)),start=c(1970,12),freq=12)
#exch<- log(dat$zarusd)
plot(exch) # looks like there is low persistence

# Fit MSW model ----
exch.lag0 = window(exch,start=c(1971,4),end=c(2015,6),freq=12) # get al the lags right
exch.lag1 = window(exch,start=c(1971,3),end=c(2015,5),freq=12)
exch.lag2 = window(exch,start=c(1971,2),end=c(2015,4),freq=12)
exch.lag3 = window(exch,start=c(1971,1),end=c(2015,3),freq=12)
exch.lag4 = window(exch,start=c(1970,12),end=c(2015,2),freq=12)

model=lm(exch.lag0~exch.lag1+exch.lag2+exch.lag3+exch.lag4-1) # linear model

mod=msmFit(model,k=2,sw=rep(TRUE,5)) # msw model
summary(mod) # regression output

plotProb(mod, which=1) # plot fixed probabilities with smoothed probability
plotProb(mod, which=2) # plot Price against the two regimes
plotDiag(mod)
plotReg(mod, expl="exch.lag1")

#slotNames(mod@Fit)
prob=(mod@Fit@smoProb) # retrieve the smoothed probability
prob.reces = ts(prob[,2],start=c(1971,4),freq=12)
plot(prob.reces,col=4) # include smoothed probabilities
