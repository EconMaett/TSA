## Canadian Labour Market Analysis
## replicate: Breitung, Bruggermann & Lutkepohl (2004) [BBL]

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(vars)

# set directory and read data
setwd("~/git/tsm/ts-10-tut/tut")
#setwd("C:\\Users\\image")

# Load data and create time series object
dat <- read.csv('Canada.csv')

# Create Time Series Object
Canada <- ts(dat[,2:5], start=c(1980,1), freq=4)

plot(Canada, nc = 2) # e = log of employment; rw = real wage; prod = gdp-e; U = unemployment rate

# ADF tests
summary(ur.df(Canada[, "prod"], type="trend", lags=2))
summary(ur.df(diff(Canada[, "prod"]), type="drift", lags=1))
summary(ur.df(Canada[, "e"], type="trend", lags=2))
summary(ur.df(diff(Canada[, "e"]), type="drift", lags=1))
summary(ur.df(Canada[, "U"], type="drift", lags=1))
summary(ur.df(diff(Canada[, "U"]), type="none", lags=0))
summary(ur.df(Canada[, "rw"], type="trend", lags=4))
summary(ur.df(diff(Canada[, "rw"]), type="drift", lags=3))
summary(ur.df(diff(Canada[, "rw"]), type="drift", lags=0))
# all of the variables are integrated of the order 1

# VAR lag selection
Canada <- Canada[, c("prod", "e", "U", "rw")] # maintain same order as BBL (2004)
VARselect(Canada, lag.max=8, type="both")
# results suggest between 1 and 3 lags - BBL make use of 3 and test for serial correlation in errors

# Diagnostic tests
p1ct <- VAR(Canada, p=1, type="both")
p2ct <- VAR(Canada, p=2, type="both")
p3ct <- VAR(Canada, p=3, type="both")

# Serial
serial.test(p3ct, lags.pt=16, type="PT.asymptotic")
serial.test(p2ct, lags.pt=16, type="PT.asymptotic")
serial.test(p1ct, lags.pt=16, type="PT.asymptotic")
serial.test(p3ct, lags.pt=16, type="PT.adjusted")
serial.test(p2ct, lags.pt=16, type="PT.adjusted")
serial.test(p1ct, lags.pt=16, type="PT.adjusted")

# JB
normality.test(p3ct)
normality.test(p2ct)
normality.test(p1ct)

# ARCH
arch.test(p3ct, lags.multi=5)
arch.test(p2ct, lags.multi=5)
arch.test(p1ct, lags.multi=5) # may have a problem here

# Stability (Recursive CUSUM)
plot(stability(p3ct), nc=2)
plot(stability(p2ct), nc=2)
plot(stability(p1ct), nc=2)
# BBL (2004) make use of either VAR(2) or VAR(3)


# Cointegration testing
trace.3 <- ca.jo(Canada, type="trace", ecdet="trend", K=3) # 3 lags
summary(trace.3)
trace.2 <- ca.jo(Canada, type="trace", ecdet="trend", K=2) # 2 lags
summary(trace.2)
# for both models it suggests one cointegrating vector (36.42<42.44) and (37.33<42.44)
# whilst for r=0, (84.92>62.99) and (86.12>62.99)

eigen.3 <- ca.jo(Canada, type="eigen", ecdet="trend", K=3) # 3 lags
summary(eigen.3)
eigen.2 <- ca.jo(Canada, type="eigen", ecdet="trend", K=2) # 2 lags
summary(eigen.2)
# for both models it suggests one cointegrating vector (17.70<25.54) and (21.69<25.54)
# whilst for r=0, (48.50>31.46) and (48.78>31.46)


# Express VECM
# normalise for rw in the 4th column

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type="trace", ecdet="trend",K=3)
vecm.r1 <- cajorls(vecm, r=1)

alpha <- coef(vecm.r1$rlm)[1, ]
beta <- vecm.r1$beta
resids <- resid(vecm.r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids) / N

# t-stats for alpha
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, vecm@Z1)))[1, 1]* diag(sigma))
alpha.t <- alpha / alpha.se # prod.d and U.d would appear insignificant


# t-stats for beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[, -1])),solve(t(alpha) %*% solve(sigma)%*% alpha))))
beta.t <- c(NA, beta[-1] / beta.se)

print(rbind(alpha,alpha.t))
print(t(cbind(beta,beta.t)))

# Test exogeniety
A1 <- matrix( c(0,0,0,
                1,0,0,
                0,1,0,
                0,0,1), nrow=4, ncol=3, byrow=TRUE)
print(A1) # first variable significant different from zero

A2 <- matrix( c(1,0,0,
                0,0,0,
                0,1,0,
                0,0,1), nrow=4, ncol=3, byrow=TRUE)
print(A2) # second variable significant different from zero

A3 <- matrix( c(1,0,0,
                0,1,0,
                0,0,0,
                0,0,1), nrow=4, ncol=3, byrow=TRUE)
print(A3) # third variable significant different from zero

A4 <- matrix( c(1,0,0,
                0,1,0,
                0,0,1,
                0,0,0), nrow=4, ncol=3, byrow=TRUE)
print(A4) # fourth variable significant different from zero

summary(alrtest(z=vecm, A=A1, r=1)) # p-val=0.00 : rw - reject null of this restriction
summary(alrtest(z=vecm, A=A2, r=1)) # p-val=0.41 : prod - cannot reject null of this restriction
summary(alrtest(z=vecm, A=A3, r=1)) # p-val=0.07 : e - reject null of this restriction
summary(alrtest(z=vecm, A=A4, r=1)) # p-val=0.19 : U - cannontreject null of this restriction
# most of the short run relationship is between real wage and employment
# production and unemployment are weakly exogenous

# Make use of long-term sign restriction
H1 <- ca.jo(Canada[, c("rw", "U", "prod", "e")], type="trace",
            ecdet="const", K=2, spec="transitory")
B1 <- matrix( c(1,0,0,0,
                -1,0,0,0,
                0,1,0,0,
                0,0,1,0,
                0,0,0,1), nrow=5, ncol=4, byrow=TRUE)
# test whether wages and productivity are negatively related in long-run

summary(blrtest(z=H1, H=B1, r=1)) # p-val=0.07 : reject null of sign restriction at 10% level
