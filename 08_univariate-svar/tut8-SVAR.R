# SVAR Model - Monetary policy shock
# Kevin Kotze

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(vars)

# set working directory
#getwd()
setwd('~/git/tsm/ts-8-tut')
#setwd("C:\\Users\\image")

# load data and create time series object
dat <- read.csv(file="za_dat.csv")

y = ts(log(dat$RGDP[-1]),start=c(1990,2),freq=4) # South African real output
pi = ts(diff(log(dat$CPI))*100,start=c(1990,2),freq=4) # South African consumer inflation
i.tmp = (1+(dat$repo/100))^0.25 -1
i = ts(i.tmp[-1]*100,start=c(1990,2),freq=4) # South African annualised interest rate

data = cbind(y,pi,i)
colnames(data) <- c("y","pi","i")
plot.ts(data)

# Three variable VAR system, containing the log of GDP, consumer inflation and annualised interest rate
# Over this sample period the log(GDP) may contain a deterministic time trend rather than a unit root
# We should test for this:

lin.mod=lm(y~time(y))
lin.trend = lin.mod$fitted.values
linear=ts(lin.trend, start=c(1990,2), frequency=4)
lin.cycle=y-linear

adf.lin <- ur.df(lin.cycle, type='none', selectlags = c("AIC"))
summary(adf.lin)
# Able to reject the null of a unit root at the 5% level

plot.ts(lin.cycle)
# In the graph we can see that GDP has a dip in 1992 and a flat section at the end in 2008.
# We include an exogenous dummy variable for these periods

dum92 = rep(0,length(y))
dum92[11] = 1
dum08 = rep(0,length(y))
dum08[75] = 1

dum92 = ts(dum92,start=c(1990,2),freq=4)
dum08 = ts(dum08,start=c(1990,2),freq=4)
dum = cbind(dum92,dum08)
colnames(dum) <- c("dum92","dum08")

# Variables are ordered (y,pi,i) and make use of a Cholesky decomposition on the matrix of contemporaneous parameters:
# - Only shocks to output can shift output contemporaneously.
# - Only shocks to output and inflation can shift inflation contemporaneously.
# - All shocks can affect the interest rate contemporaneously.
#  This ordering is consistent with macroeconomic theory and often applied in a closed-economy setting

# Note also that instead of using the linear cycle, we're going to use log(gdp)
# This will allow me to show you how to include a deterministic trend in the model
# The object data includes the variables (y,pi,i)


# Use information criteria to determine lag length
info.var <- VARselect(data,lag.max=12,type="both")
info.var$selection # we can make use of SIC/BIC and assume 1 lag

# Estimate reduced-form VAR to get an appropriate object
# Note that we include a deterministic time trend and a constant
var.est1 <- VAR(data,p=1,type="both",season=NULL,exog=dum)
summary(var.est1) # note the coefficients and the variance-covariance of the residuals

# Set up the matrix for the contemporaneous matrix - insert zeros for restrictions
a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[2,1] <- NA
a.mat[3,1] <- NA
a.mat[3,2] <- NA
print(a.mat) # three restrictions in the top triangle [(K^2 - K)/2]

# Set up the matrix for the identification of individual shocks
b.mat <- diag(3)
diag(b.mat) <- NA
print(b.mat) # we have no covariance terms for the residuals

svar.one <- SVAR(var.est1,Amat=a.mat,Bmat=b.mat,max.iter=10000,hessian=TRUE)
svar.one # both matrices have the correct functional form

one.int <- irf(svar.one,response="i",impulse="i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(one.int) # reponse of interest rate to interest rate shocks
# relatively persistent interest rate shock

one.gdp <- irf(svar.one,response="y",impulse="i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(one.gdp) # reponse of output to interest rate shocks
# positive interest rate shock results in delining in ouput

one.inf <- irf(svar.one,response="pi",impulse="i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(one.inf) # reponse of inflation to interest rate shocks
# positive interest rate shock results in delining inflation

# The figures show that a contractionary monetary policy shock:
# - increases the interest rate temporarily
# - has a temporary negative effect on GDP
# - has a temporary negative effect on inflation
# This is consistent with theory (although the decline in inflation is greater than decline in output)

####################################################
# Alternate ordering of the Cholesky decomposition #
####################################################

# To show the effect of changing the ordering of output and the interest rate:
# - interest rates are only affected by interest rate shocks
# - inflation responds to interest rate and inflation shocks
# - output will now respond instantly to all other shocks
# There is no theoretical justification for this ordering, we just use it to illustrate a point

a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[1,2] <- NA
a.mat[1,3] <- NA
a.mat[2,3] <- NA
print(a.mat)

# We use the alternate ordering of the Cholesky decomposition

b.mat <- diag(3)
diag(b.mat) <- NA
print(b.mat)

svar.two <- SVAR(var.est1,Amat=a.mat,Bmat=b.mat,max.iter=10000, hessian=TRUE)
svar.two

two.int <- irf(svar.two,response = "i",impulse = "i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(two.int) # reponse of interest rate to interest rate shocks
# relatively persistent interest rate shock

two.gdp <- irf(svar.two,response = "y",impulse = "i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(two.gdp) # reponse of output to interest rate shocks
# positive interest rate shock results in delining in ouput

two.inf <- irf(svar.two,response = "pi",impulse = "i",n.ahead=40,ortho=TRUE,boot=TRUE)
plot(two.inf) # reponse of inflation to interest rate shocks
# positive interest rate shock results in large increase in inflation???

# Here we see that inflation increases by a large amount following a positive interest rate shock.
# This would certainly not agree with theory and is highly contrary to the previous finding.
# To show the difference in the impulse response functions we can graph the results from the two models together.

plot.ts(cbind(one.inf$irf$i,two.inf$irf$i),plot.type = "single",col=4:2,
        ylab="inflation",main="interest rate shock")
lines(rep(0,length(one.inf$irf$i)),col="grey")
legend("topright",legend=c("model 1","model 2"),col=4:2,lty=1,bty="n")
