# ARDL model for cointegration

#install.packages("devtools")
#devtools::install_github("fcbarbi/ardl")

library(tidyverse)
library(ardl)
library(vars)
data(br_month)

# The data pertains to measures for Brazil between January 2001 up to December 2014 for the:
# interest rate, mpr
# consumer prices, cpi
# exchange rate, reer

plot(br_month[, c("mpr", "cpi", "reer")])

# Note that the variables appear to contain unit-root processes. We can test for this with the aid of the Dickey-Fuller tests
# Although we should apply general to specific procedure - I've just done the simple test to save time

br_month$cpi %>%
  ur.df(., type = "none", selectlags = c("BIC")) %>%
  summary()

br_month$mpr %>%
  ur.df(., type = "none", selectlags = c("BIC")) %>%
  summary()

br_month$reer %>%
  na.omit() %>%
  ur.df(.,
        type = "none",
        selectlags = c("BIC")) %>%
  summary()

# An ARDL(2,1,1) model structure that is used to describe the monetary policy rate `mpr` with 2 lags
# Include two regressors: prices `cpi` and the exchange rate `reer` with at most one lag each may be expressed as:

m1 <-
  ardl(
    mpr ~ cpi + reer |
      d_lula,
    data = br_month,
    ylag = 2,
    xlag = c(1, 1),
    case = 3
  )

# The ARDL methodology allows the estimation in levels of a common long-term relation between the regressors and the explained variable
# In function `coint()` a stationary specification is tested after controlling for the lag of the long-term relation, expressed as `L(coint)`
# To get model details on the coefficients and the usual tests use the traditional `summary()` function

summary(m1)

# To test the existence of the cointegration relation with the bounds test
# The bounds test checks the existence of a long-term relation with critical values for I(0) and I(1) regressors.

bounds.test(m1)

# To visualize the long-term coefficients use the function `coint()`

coint(m1)

# Note that the SR coefficients (second panel) come from a model with regressors in first difference
# This would imply that the variables are first-difference stationary

# This modelling framework allows for an automated model selection process, which selects the maximum lag for each regressor
# If no such information is provided, it is assumed that a single lag is the maximum

m2 <-
  auto.ardl(
    mpr ~ cpi + prod + reer |
      d_lula,
    data = br_month,
    ymax = 2,
    xmax = c(2, 2, 2),
    ic = "bic"
  )
summary(m2)

# The selection process involves estimating the best fit for each regressor in the order they are included in the canonical equation

m3 <-
  auto.ardl(
    mpr ~ cpi + reer,
    data = br_month,
    ymax = 2,
    xmax = c(1, 1),
    verbose = TRUE,
    case = 1
  )
