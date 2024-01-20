# VAR Model - for US data on gdp and unemployment
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(tsm)
library(vars)

# set working directory ----
#getwd()
setwd('~/git/tsm/ts-7-tut')
#setwd("C:\\Users\\image")

# load data and create time series object ----
dat <- read.csv(file = "blanchQua.csv")

gdp <- ts(dat$GDP, start = c(1948, 2), freq = 4) # gdp
une <- ts(dat$U, start = c(1948, 2), freq = 4)   # unemployment

plot(cbind(gdp, une))

# look at the persistence in the data ----
gdp.acf <- ac(gdp, main = "output")
une.acf <- ac(une, main = "unemployent")

# check unemployment for UR (as there is quite a bit of persistence)
adf.une <- ur.df(une, type = "trend", selectlags = "AIC")
summary(adf.une)   # able to reject null of unit root

# group data and choose lag length
dat.bv <- cbind(gdp, une)
colnames(dat.bv) <- c("gdp", "une")

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection   # All suggest suggests using 2 lags

# group data and choose lag length
bv.est <- VAR(dat.bv, p = 2 ,type = "const", season = NULL, exog = NULL)
summary(bv.est)

# system is stable (roots are eigenvalues)
# there are many insignificant variables
# gdp is influenced by past unemployment
# unemployment is influenced by past gdp and unemployment

## perform some diagnostic tests on residuals ----

# testing serial correlation with Portmanteau-Test
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
# p-value greater than 5% indicates absence of serial correlation

windows()
plot(bv.serial, names = "gdp")
windows()
plot(bv.serial, names = "une")

# testing heteroscedasticity
bv.arch <- arch.test(bv.est, lags.multi = 12, multivariate.only = TRUE)
bv.arch
# p-value greater than 5% indicates absence of heteroscedasticity

# testing for normality
bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.norm
# p-value indicates pretty normal distribution

# testing for structural stability
bv.cusum <- stability(bv.est, type = "OLS-CUSUM")

windows()
plot(bv.cusum)
# does not break the confidence intervals for structural break

## Granger & instantaneous causality tests ====

bv.cause.gdp <- causality(bv.est, cause = "gdp")
bv.cause.gdp
bv.cause.une <- causality(bv.est, cause = "une")
bv.cause.une
# Null hypothesis of no Granger causality is dismissed in both directions

graphics.off() # close all graphs

## Generate irfs ====

irf.gdp <- irf(bv.est, impulse = "une", response = "gdp",n.ahead = 40, boot = TRUE)
plot(irf.gdp) # reponse of gdp to unemploy shocks
# positive shock to unemployment has a negative affect on ouput (lower spending power)

irf.une <- irf(bv.est, impulse = "gdp", response = "une",n.ahead = 40, boot = TRUE)
plot(irf.une) # reponse of unemployment to gdp shock
# positive shock to output decreases unemployment by large and persistent amount

irf.une_un <- irf(bv.est, impulse = "une", response = "une",n.ahead = 40, boot = TRUE)
plot(irf.une, ylab = "unemployment", main = "Shock from GDP", xlab = "")
plot(irf.une_un, ylab = "unemployment", main = "Shock from unemployment")

# generate variance decompositions ----
bv.vardec <- fevd(bv.est, n.ahead = 10)
windows()
plot(bv.vardec)
# gdp is largely determined by gdp shocks, while unemployment is influenced by both shocks

graphics.off() # close graphics

## Forecasting with the VAR model ====
predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "gdp")
plot(predictions, names = "une")

# Fanchart for forecasts ----
fanchart(predictions, names = "gdp")
fanchart(predictions, names = "une")
