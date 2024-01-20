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
# ibm monthly returns from 1926 to 2009
dat <- read.csv(file="ibm.csv")

obs <- length(dat$ibm)

ibm <- log(dat$ibm[493:obs]+1)

plot.ts(ibm)

# Model fitting - Egarch model
m1 <- Egarch(ibm) 
names(m1) # look at what's in the object

stresi <- m1$residuals/m1$volatility # obtain standardized residuals

tdx <- c(1:516)/12+1967 # Compute time index

# Plot results
par(mfcol=c(2,1),mar=c(2.2,2.2,1,2.2),cex=0.8)  
plot(tdx,ibm,xlab='year',ylab='logrtn',type='l') # original data
plot(tdx,stresi,xlab='year',ylab='stresi',type='l') # residuals from model

# Model checking
Box.test(stresi,lag=10,type='Ljung')  # nice
Box.test(stresi,lag=20,type='Ljung')  # nice
Box.test(stresi^2,lag=10,type='Ljung')  # nice
Box.test(stresi^2,lag=20,type='Ljung')  # nice
