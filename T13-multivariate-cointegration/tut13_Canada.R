## Canadian Labour Market Analysis
## replicate: Breitung, Bruggermann & Lutkepohl (2004) [BBL]

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(tidyverse)
library(vars)

# Load data and create time series object
dat <- read_csv('Canada.csv')

# plot the data
dat$prod %>%
  plot.ts(., main = "prod")

dat$e %>%
  plot.ts(., main = "e")

dat$rw %>%
  plot.ts(., main = "rw")

dat$U %>%
  plot.ts(., main = "U")

# unit root tests
dat$prod %>%
  ur.df(., type = "trend", lags = 2) %>%
  summary()

dat$prod %>%
  diff() %>%
  ur.df(., type = "trend", lags = 1) %>%
  summary()

dat$e %>%
  ur.df(., type = "trend", lags = 2) %>%
  summary()

dat$e %>%
  diff() %>%
  ur.df(., type = "trend", lags = 1) %>%
  summary()

dat$U %>%
  ur.df(., type = "trend", lags = 1) %>%
  summary()

dat$U %>%
  diff() %>%
  ur.df(., type = "trend", lags = 0) %>%
  summary()

dat$rw %>%
  ur.df(., type = "trend", lags = 4) %>%
  summary()

dat$rw %>%
  diff() %>%
  ur.df(., type = "trend", lags = 3) %>%
  summary()

dat$rw %>%
  diff() %>%
  ur.df(., type = "trend", lags = 0) %>%
  summary()

# all of the variables are integrated of the order 1

# VAR lag selection
dat_can <- dat %>%
  dplyr::select(prod, e, U, rw) # maintain same order as BBL (2004)
VARselect(dat_can, lag.max = 8, type = "both")
# results suggest between 1 and 3 lags - BBL make use of 3 and test for serial correlation in errors

# Estimate three different models
p1ct <- VAR(dat_can, p = 1, type = "both")
p2ct <- VAR(dat_can, p = 2, type = "both")
p3ct <- VAR(dat_can, p = 3, type = "both")

# Serial
serial.test(p3ct, lags.pt = 16, type = "PT.asymptotic")
serial.test(p2ct, lags.pt = 16, type = "PT.asymptotic")
serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
serial.test(p3ct, lags.pt = 16, type = "PT.adjusted")
serial.test(p2ct, lags.pt = 16, type = "PT.adjusted")
serial.test(p1ct, lags.pt = 16, type = "PT.adjusted")

# JB
normality.test(p3ct)
normality.test(p2ct)
normality.test(p1ct)

# ARCH
arch.test(p3ct, lags.multi = 5)
arch.test(p2ct, lags.multi = 5)
arch.test(p1ct, lags.multi = 5) # may have a problem here

# Stability (Recursive CUSUM)
plot(stability(p3ct), nc = 2)
plot(stability(p2ct), nc = 2)
plot(stability(p1ct), nc = 2)
# BBL (2004) make use of either VAR(2) or VAR(3)


# Cointegration testing
trace_3 <- ca.jo(dat_can,
                 type = "trace",
                 ecdet = "trend",
                 K = 3) # 3 lags
summary(trace_3)

trace_2 <- ca.jo(dat_can,
                 type = "trace",
                 ecdet = "trend",
                 K = 2) # 2 lags
summary(trace_2)
# for both models it suggests one cointegrating vector (36.42<42.44) and (37.33<42.44)
# whilst for r=0, (84.92>62.99) and (86.12>62.99)

eigen_3 <- ca.jo(dat_can,
                 type = "eigen",
                 ecdet = "trend",
                 K = 3) # 3 lags
summary(eigen_3)

eigen_2 <- ca.jo(dat_can,
                 type = "eigen",
                 ecdet = "trend",
                 K = 2) # 2 lags
summary(eigen_2)
# for both models it suggests one cointegrating vector (17.70<25.54) and (21.69<25.54)
# whilst for r=0, (48.50>31.46) and (48.78>31.46)


# Expression for VECM ----
# normalise for rw in the 4th column
vecm <- dat_can %>%
  dplyr::select(rw, prod, e, U) %>%
  ca.jo(., type = "trace", ecdet = "trend", K = 3)

vecm_r1 <- cajorls(vecm, r = 1)

alpha <- coef(vecm_r1$rlm)[1, ]
beta <- vecm_r1$beta

resids <- resid(vecm_r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids) / N

# t-stats for alpha
alpha_se <-
  sqrt(solve(crossprod(cbind(
    vecm@ZK %*% beta, vecm@Z1
  )))[1, 1] * diag(sigma))

alpha_t <- alpha / alpha_se # prod.d and U.d would appear insignificant

# t-stats for beta
beta_se <-
  sqrt(diag(kronecker(
    solve(crossprod(vecm@RK[,-1])), solve(t(alpha) %*% solve(sigma) %*% alpha)
  )))
beta_t <- c(NA, beta[-1] / beta_se)

print(rbind(alpha, alpha_t))
print(t(cbind(beta, beta_t)))

# Test exogeniety
A1 <- diag(4)[, -1]
print(A1) # test for first variable significant different from zero

A2 <- diag(4)[, -2]
print(A2) # second variable significant different from zero

A3 <- diag(4)[, -3]
print(A3) # third variable significant different from zero

A4 <- diag(4)[, -4]
print(A4) # fourth variable significant different from zero

alrtest(z = vecm, A = A1, r = 1) %>%
  summary() # p-val=0.00 : rw - reject null of this restriction
alrtest(z = vecm, A = A2, r = 1) %>%
  summary() # p-val=0.41 : prod - cannot reject null of this restriction
alrtest(z = vecm, A = A3, r = 1) %>%
  summary() # p-val=0.07 : e - reject null of this restriction
alrtest(z = vecm, A = A4, r = 1) %>%
  summary() # p-val=0.19 : U - cannot reject null of this restriction
# most of the short run relationship is between real wage and employment
# production and unemployment are weakly exogenous

# Make use of long-term sign restriction

H1 <- dat_can %>%
  dplyr::select(rw, U, prod, e) %>%
  ca.jo(
    .,
    type = "trace",
    ecdet = "const",
    K = 2,
    spec = "transitory"
  )

B1 <- diag(5)[,-2]
B1[2,1] <- -1
print(B1)
# test whether wages and productivity are negatively related in long-run

blrtest(z = H1, H = B1, r = 1) %>%
  summary() # p-val=0.07 : reject null of sign restriction at 10% level
