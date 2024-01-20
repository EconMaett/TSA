# Conditional heteroskedastic processes
# Using USD EU exchange rate
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
# ibm monthly returns from 1926 to 2009
dat <- read.csv(file="ex_usd_eu.csv",sep=";")

fx <- log(dat$rate)
eu <- diff(fx)*100

# Model fitting - Ngarch model
m1 <- Ngarch(eu)

names(m1) # look at what's in the object

res <- m1$residuals
vol <- m1$volatility 
resi <- res/vol # obtain standardized residuals

# Plot results
par(mfcol=c(2,1),mar=c(2.2,2.2,1,2.2),cex=0.8)  
plot.ts(eu) # original data
plot.ts(resi) # residuals from model

Box.test(resi,lag=10,type='Ljung') # nice
Box.test(abs(resi),lag=10,type='Ljung') # could be better
