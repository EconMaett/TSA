# CVAR Model
# Replicate results of MacDonald & Ricci (2004) - SAJE
# Kevin Kotze

# clear data & close graphs
rm(list=ls())
graphics.off()

# load package
library(tidyverse)
library(vars)


# load data and create time series object
dat <- read_csv("Mac_Ric.csv")

# logarithm of real effective exchange rate
dat$LREERS %>%
  plot.ts(., main = "LREERS")

# real interest rate relative to trading partners
dat$RIRR %>%
  plot.ts(., main = "RIRR")

# logarithm of real GDP per capita relative to trading partners
dat$LRGDPPCR %>%
  plot.ts(., main = "LRGDPPCR")

# real commodity prices
dat$LPR2COMM5 %>%
  plot.ts(., main = "LPR2COMM5")

# openness - ratio to GDP of exports and imports
dat$OPENY %>%
  plot.ts(., main = "OPENY")

# ratio of fiscal balance to GDP
dat$FBYA %>%
  plot.ts(., main = "FBYA")

# ratio to GDP of net foreign assets of the banking system
dat$NFAOFPY %>%
  plot.ts(., main = "NFAOFPY")

# ESTIMATE VAR ----

# separate endogenous and exogenous variables
dat_VAR <- dat %>%
  dplyr::select(LREERS,
                RIRR,
                LRGDPPCR,
                LPR2COMM5,
                OPENY,
                FBYA,
                NFAOFPY)

dat_EXO <- dat %>%
  dplyr::select(SDUMC1,
                SDUMC2,
                SDUMC3,
                DUMRER1,
                DUMRER2,
                DUMFBYA,
                DUMNFAOFPY)

# estimate model
VAR_est <- VAR(
  dat_VAR,
  p = 4,
  type = "const",
  season = NULL,
  exog = dat_EXO
)
summary(VAR_est)

# use trace statistic
H1_trace <-
  ca.jo(
    dat_VAR,
    ecdet = c("const"),
    type = "trace",
    K = 4,
    season = NULL,
    dumvar = dat_EXO
  )
summary(H1_trace) # r <= 1 : 94.47 < 102.14

# use maximum eigenvalue statistic
H1_eigen <-
  ca.jo(
    dat_VAR,
    ecdet = c("const"),
    type = "eigen",
    K = 4,
    season = NULL,
    dumvar = dat_EXO
  )
summary(H1_eigen) # r <= 1 : 28.06 < 40.30

# extract the speed of adjustment coefficients
beta <- H1_trace@V
alpha <- H1_trace@W

A1 <- diag(7)[,-1]
print(A1) # note the first column is missing
          # tests whether the speed of adjustment in first variable is equal to zero

H2 <- alrtest(z = H1_trace, A = A1, r = 1)
summary(H2) # pval = 0.37. Cannot reject the null that the first variable (exchange rate) is weakly exogenous

# The exchange rate does not react to the equilibrium exchange rate???
