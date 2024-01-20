# TAR, LSTAR, SETAR and ANN
# Using ZARUSD exchange rate (daily)
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

library(tsDyn)
library(sm)
library(tseriesChaos)
library(Metrics)
library(tsm)

# Set directory and read data ----
setwd('~/git/tsm/ts-13-tut')
#setwd("C:\\Users\\image")

dat=read.csv('zar_usd_month.csv',sep=";")

exch<- ts(diff(log(dat$zarusd)),start=c(1970,12),freq=12)
#exch<- log(dat$zarusd)
plot(exch) # looks like there is low persistence
ac(exch) # confirms that there is low persistence

par(mfrow=c(1,3))
autopairs(exch,lag=1,type="regression") # could be some nonlinear relationship 
autopairs(exch,lag=2,type="regression") # could be some nonlinear relationship
autopairs(exch,lag=3,type="regression") # could be some nonlinear relationship

par(mfrow=c(1,1))
hist(exch,br=20)

#mutual(exch) # see timeseriesChaos
#delta.test(exch)
#delta.lin.test(exch)

## linear model
mod.ar <- linear(exch,m=3) 
summary(mod.ar) # only the first lag is significant

mod.ar <- linear(exch,m=1) 
summary(mod.ar)

## star model
mod.star = star(exch,m=3,include="const")
summary(mod.star) # switching parameter is pretty close to significant

mod.star = star(exch,m=3,include="const",mL=1,mH=3)
summary(mod.star) # switching parameter is pretty close to significant

#windows()
plot(mod.star) # looks a little (but not much better)

## SETAR model
##------------
mod.setar = setar(exch,m=3,include="const")
summary(mod.setar) # in regime one the AR(1) is significant, in the other its the AR(3)

mod.setar = setar(exch,m=3,include="none",mL=2,mH=3)
summary(mod.setar) # similar result without a constant

mod.setar = setar(exch,m=3,include="const",mL=1,mH=3) # use AR(1) and AR(3)
summary(mod.setar)

#windows()
plot(mod.setar)

selectSETAR(exch,m=3,mL=1:3,mH=1:3,thDelay=1:2)

# LSTAR model ----

mod.lstar = lstar(exch,m=2,include="const")
summary(mod.lstar)

mod.lstar = lstar(exch,m=3,include="const",mL=1,mH=3)
summary(mod.lstar)

#windows()
plot(mod.lstar)

## ANN model
##----------

mod.ann = nnetTs(exch,m=2,size=3)
summary(mod.ann)
#windows()
plot(mod.ann)

selectNNET(exch,m=3,size=1:3)

mod.ann = nnetTs(exch,m=1,size=1)
summary(mod.ann)

## Model comparison
##-----------------

mod <- list()
mod[["mod.ar"]] = linear(exch,m=1)
mod[["mod.star"]] = star(exch,m=3,include="const",mL=1,mH=3)
mod[["mod.setar"]] = setar(exch,m=3,include="const",mL=1,mH=3)
mod[["mod.lstar"]] = lstar(exch,m=3,include="const",mL=1,mH=3)
mod[["mod.nnetTs"]] = nnetTs(exch,m=1,size=1)

sapply(mod,AIC)

# out-of-sample forecast comparison
in.sample <- window(exch,end=c(2011,6))
out.sample <- window(exch,start=c(2011,7))

mod.fore <- list()
mod.fore[["mod.ar"]] = linear(in.sample,m=3)
mod.fore[["mod.star"]] = star(in.sample,m=3)
mod.fore[["mod.setar"]] = setar(in.sample,m=3)
mod.fore[["mod.lstar"]] = lstar(in.sample,m=3)
mod.fore[["mod.nnetTs"]] = nnetTs(in.sample,m=3,size=3)

fore.test <- lapply(mod.fore,predict,n.ahead=48)
plot(out.sample,ylim=range(exch))
for(i in 1:length(fore.test)){
  lines(fore.test[[i]],lty=i+1, col=i+1)
  }
legend("bottomleft",legend=c("observed",names(fore.test)),col=1:(length(fore.test)+1),lty=1:(length(fore.test)+1))

# calculate RMSE
fore.rmse <- rep(0,5)
fore.rmse[1] <- rmse(out.sample,fore.test[["mod.ar"]])
fore.rmse[2] <- rmse(out.sample,fore.test[["mod.star"]])
fore.rmse[3] <- rmse(out.sample,fore.test[["mod.setar"]])
fore.rmse[4] <- rmse(out.sample,fore.test[["mod.lstar"]])
fore.rmse[5] <- rmse(out.sample,fore.test[["mod.nnetTs"]])

fore.res = rbind(names(mod.fore),fore.rmse)
min(fore.res)
print(fore.res)
