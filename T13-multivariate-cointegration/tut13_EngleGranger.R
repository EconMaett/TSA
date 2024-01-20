# Engle-Granger Cointegration
# Using commodity prices
# Kevin Kotze

# clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(vars)

# load data and take the natural logarithm of prices
dat <- read_csv('comodity_price.csv')

dat <- dat %>%
  mutate(
    gold = log(gold),
    silver = log(silver),
    plat = log(plat),
    pall = log(pall)
  )


# plot data
par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 2.2),
    cex = 0.8)
plot.ts(
  cbind(dat$gold, dat$silver),
  plot.type = "single",
  ylab = "",
  col = 4:3
)
legend(
  "topleft",
  legend = c("gold", "silver"),
  col = 4:3,
  lty = 1,
  bty = 'n'
)

# check gold price level for unit root
dat$gold %>%
  ur.df(., type = "trend", selectlags = c("BIC")) %>%
  summary()

dat$gold %>%
  ur.df(., type = "drift", selectlags = c("BIC")) %>%
  summary()

dat$gold %>%
  ur.df(., type = "none", selectlags = c("BIC")) %>%
  summary()
# Gold is non-stationary

# check first difference for unit root
dat$gold %>%
  diff() %>%
  ur.df(., type = "trend", selectlags = c("BIC")) %>%
  summary()
# Gold is Integrated of order 1

# check silver price level for unit root
dat$silver %>%
  ur.df(., type = "trend", selectlags = c("BIC")) %>%
  summary()

dat$silver %>%
  ur.df(., type = "drift", selectlags = c("BIC")) %>%
  summary()

dat$silver %>%
  ur.df(., type = "none", selectlags = c("BIC")) %>%
  summary()
# Silver is non-stationary

# check first difference for unit root
dat$silver %>%
  diff() %>%
  ur.df(., type = "trend", selectlags = c("BIC")) %>%
  summary()
# Silver is Integrated of order 1

# create single object with two variables
dat_lr <- dat %>%
  dplyr::select(gold, silver)

## estimate long run regression with constant
gold_eq <- lm(gold ~ silver, data = dat_lr)
summary(gold_eq)
plot.ts(gold_eq$residuals)

# check for unit root
gold_eq$residuals %>%
  ur.df(., lags = 1, type = "none") %>%
  summary()
# we should actually use test statistics from Engle & Granger (1987) or Engle & Yoo (1987)
# able to reject null of unit root in the residual - they are cointegrated

# prepare data for ECM (data has 231 obs and lag 1st diff has 229)
# gold.d = const + error.ecm1 + gold.d1 + silver.d1
dat_ecm <- tibble(
  gold_d = diff(dat$gold)[-1],
  silver_d = diff(dat$silver)[-1],
  error_ecm1 = gold_eq$residuals[-1:-2],
  gold_d1 = diff(dat$gold)[-(length(dat$gold) - 1)],
  silver_d1 = diff(dat$silver)[-(length(dat$silver) - 1)]
)

# estimate ECM
ecm_gold <-
  lm(gold_d ~ error_ecm1 + gold_d1 + silver_d1, data = dat_ecm)
summary(ecm_gold)
# Note that the alpha is insignificant and close to zero
# Therefore gold is weakly exogenous with respect to the cointegrating parameters since it does not adjust to past deviations from long-run equilibrium

ecm_silver <-
  lm(silver_d ~ error_ecm1 + gold_d1 + silver_d1, data = dat_ecm)
summary(ecm_silver)
# Note that the alpha is negative and significant, so silver does all the work
# Implies there is Granger causality from gold to silver
# It takes 1/0.079 periods to return to equilibrium

# Note that at least one of these signs should be negative and significant when cointegrated!
