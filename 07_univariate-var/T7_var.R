# VAR Model - for South African data
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# load package ----
library(tsm)
library(vars)
library(mFilter)

# set working directory ----
#getwd()
setwd('~/git/tsm/ts-7-tut')
#setwd("C:\\Users\\image")

# load data and create time series object ----
dat_sa <- read.csv(file = "data_sa.csv")

gdp <- ts(log(dat_sa$gdp), start = c(1981, 2), freq = 4)  # log real gdp
inf <- ts(dat_sa$inf, start = c(1981, 2), freq = 4)  # inflation q-o-q
int <- ts(dat_sa$int, start = c(1981, 2), freq = 4)  # 3m T-bill (quaterly)

plot(cbind(gdp, inf, int))

# calculate output gap using HP filter ----
gdp.hp <- hpfilter(gdp, freq = 1600)
#plot(gdp.hp)
gdp.gap <- gdp.hp$cycle

plot(cbind(gdp.gap, inf, int))

# look at the persistence in the data ----
gdp.acf <- ac(gdp.gap, main = "GDP")
inf.acf <- ac(inf, main = "INF")
int.acf <- ac(int, main = "INT")

# check variables for UR (as there is quite a bit of persistence)
adf.gdp <- ur.df(gdp.gap, type = "trend", selectlags = "AIC")
summary(adf.gdp)   # able to reject null of unit root

adf.inf <- ur.df(inf, type = "trend", selectlags = "AIC")
summary(adf.inf)   # able to reject null of unit root

adf.int <- ur.df(int, type = "trend", selectlags = "AIC")
summary(adf.int)   # able to reject null of unit root at 5%


## Investigating the relationship between interest rate & inflation ====

dat.bv <- cbind(int, inf)
colnames(dat.bv) <- c("int", "inf")

# The number of lags may be given by information criteria ----
info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection # BIC/SC suggests using 2 lags

# Use VAR(2) with no contemporaneous parameters (incl. const & season)
bv.est <- VAR(dat.bv, p = 2, type = "const", season = 4, exog = NULL)
summary(bv.est)
# there are still many insignificant variables
# seasonal would appear to be largely insignificant

bv.est <- VAR(dat.bv, p = 2, type = "const", season = NULL, exog = NULL)
summary(bv.est)
# results suggest that the inflation is largely dependent upon it's immediate past
# while interest rates are dependent upon it's past 2 observations
# the covariation in error terms is relatively small (although not extremely small).

## perform some diagnostic tests ----
# testing serial correlation with Portmanteau-Test
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
# p-value greater than 5% indicates absence of serial correlation (very close)
plot(bv.serial, names = "int")
plot(bv.serial, names = "inf")

# testing heteroscedasticity ----
bv.arch <- arch.test(bv.est, lags.multi = 12, multivariate.only = TRUE)
bv.arch
# p-value very close to 5%

# testing for normality ----
bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.norm
# p-value indicates that there could be a problem with skewness and kurtosis
# mainly relate to interest rates (big error around observation 70)
# could use dummies for these outliers, which would be included as exogenous variable

resid <- residuals(bv.est)
par(mfrow = c(1, 1))
plot.ts(resid[, 1]) # somewhere between obs 60 and 80
plot.ts(resid[60:80, 1])
resid[60:80, 1] # observation 68 and 69

dum <- rep(0, dim(dat.bv)[1])
dum[68] <- 1
dum[69] <- 1
plot.ts(dum)

bv.estdum <- VAR(dat.bv, p = 2, type = "const", season = 4, exog = dum)
bv.serialdum <- serial.test(bv.estdum, lags.pt = 12, type = "PT.asymptotic")
plot(bv.serialdum)
bv.normdum <- normality.test(bv.estdum, multivariate.only = TRUE)
bv.normdum # makes very little difference

# testing for structural stability ----
bv.cusum <- stability(bv.est, type = "OLS-CUSUM")
plot(bv.cusum)
# seems to be more or less ok!

## Granger & instantaneous causality tests ----
bv.cause.int <- causality(bv.est, cause = "int")
bv.cause.int
bv.cause.inf <- causality(bv.est, cause = "inf")
bv.cause.inf
# Granger causality is not dismissed in both directions
# But there appears to be little instanteous causality

## Generate irfs ----
irf.int <- irf(bv.est, response = "int", n.ahead = 40, boot = TRUE)
plot(irf.int) # reponse of interest rate to other shocks

irf.inf <- irf(bv.est, response = "inf", n.ahead = 40, boot = TRUE)
plot(irf.inf) # reponse of inflation to other shocks

# Price puzzle - this is not what we wanted to see:
# Dynamics may be more involved than standard VAR allows for
# Or we may need more than two variables

## Generate variance decompositions ----
bv.vardec <- fevd(bv.est, n.ahead = 10)
plot(bv.vardec)
# interest rates are really only determined by interest rate shocks
# while inflation is influenced by interest rate shocks to a certain degree
