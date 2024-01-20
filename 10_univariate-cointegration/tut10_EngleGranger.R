# Engle-Granger Cointegration
# Using commodity prices
# Kevin Kotze

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(vars)

# set directory and read data
setwd("~/git/tsm/ts-10-tut/tut")
#setwd("C:\\Users\\image")

# load data and create time series object
dat <- read.csv('comodity_price.csv')

# create Time Series Objects
gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)
silver <- ts(log(dat$silver),start=c(1993,11),frequency=12)
plat <- ts(log(dat$plat),start=c(1993,11),frequency=12)
pall <- ts(log(dat$pall),start=c(1993,11),frequency=12)

# plot data
par(mfrow=c(1,1),mar=c(2.2,2.2,1,2.2),cex=0.8)
plot.ts(cbind(gold,silver), plot.type="single", ylab="",col = 4:3)
legend("topleft",legend=c("gold","silver"),col=4:3,lty=1,bty='n')

adfg1 <- ur.df(gold, type = "trend", selectlags = c("BIC"))
summary(adfg1)  # 1 lag, adf = -1.4027, crit = -3.99 -3.43 -3.13 (1pct  5pct 10pct)

adfg1 <- ur.df(gold, type = "drift", selectlags = c("BIC"))
summary(adfg1)  # 1 lag, adf = -1.4007, crit = -3.46 -2.88 -2.57 (1pct  5pct 10pct)

adfg1 <- ur.df(gold, type = "none", selectlags = c("BIC"))
summary(adfg1)  # 1 lag, adf = 2.5856, crit = -2.58 -1.95 -1.62 (1pct  5pct 10pct)

adfg2 <- ur.df(diff(gold), selectlags = c("BIC"))
summary(adfg2)  # 1 lag, adf = -12.77, crit = -2.58 -1.95 -1.62 (1pct  5pct 10pct)
# Gold is Integrated of order 1


adfs1 <- ur.df(silver, type = "trend", selectlags = c("BIC"))
summary(adfs1)  # 1 lag, adf = -1.83, crit = -3.99 -3.43 -3.13 (1pct  5pct 10pct)

adfs1 <- ur.df(silver, type = "drift", selectlags = c("BIC"))
summary(adfs1)  # 1 lag, adf = -0.07, crit = -3.46 -2.88 -2.57 (1pct  5pct 10pct)

adfs1 <- ur.df(silver, type = "none", selectlags = c("BIC"))
summary(adfs1)  # 1 lag, adf = 1.47, crit = -2.58 -1.95 -1.62 (1pct  5pct 10pct)

adfs2 <- ur.df(diff(silver), selectlags = c("BIC"))
summary(adfs2)  # 1 lag, adf = -12.44, crit = -2.58 -1.95 -1.62 (1pct  5pct 10pct)
# Silver is Integrated of order 1

# create single object with two variables
data <- ts.union(gold,silver)

## estimate long run regression with constant
gold.eq <- lm(gold~silver,data=data)
summary(gold.eq)
plot.ts(gold.eq$residuals)

# check for unit root
error.gold <- ur.df(gold.eq$residuals,lags=1,type="none")
summary(error.gold)
# we should actually use test statistics from Engle & Granger (1987) or Engle & Yoo (1987)
# able to reject null of unit root in the residual - they are cointegrated

# prepare data for ECM (data has 231 obs and lag 1st diff has 229)
# gold.d = const + error.ecm1 + gold.d1 + silver.d1
gold.d <- diff(gold)[-1]
silver.d <- diff(silver)[-1]
error.ecm1 <- gold.eq$residuals[-1:-2]
gold.d1 <- diff(gold)[-(length(gold)-1)]
silver.d1 <- diff(silver[-(length(silver)-1)])

# estimate ECM
ecm.gold <- lm(gold.d~error.ecm1+gold.d1+silver.d1)
summary(ecm.gold)
# Note that the alpha is insignificant and close to zero
# Therefore gold is weakly exogenous with respect to the cointegrating parameters since it does not adjust to past deviations from long-run equilibrium

ecm.silver <- lm(silver.d~error.ecm1+gold.d1+silver.d1)
summary(ecm.silver)
# Note that the alpha is negative and significant, so silver does all the work
# Implies there is Granger causality from gold to silver
# It takes 1/0.078 periods to return to equilibrium

# Note that at least one of these signs should be negative and significant when cointegrated!
