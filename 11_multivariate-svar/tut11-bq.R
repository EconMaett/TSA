# SVAR Model - Blanchard & Quah Replication
# Kevin Kotze

# clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(vars)

# load data
dat <- read_csv("BQdata.csv")

# get time series
data <- tibble(
  yt = diff(100 * log(dat$GNP82)),
  ut = dat$USUNRATEE[-1]
)

nt <- dim(data)[1]

# demean data
data <- data %>%
  mutate(
    dyt = yt - mean(yt),
    dut = ut - mean(ut)
  )

# set up dat for VAR
vardat0 <- data %>%
  dplyr::select(dyt, dut)

# plot processes
plot.ts(vardat0, main = "")

# estimate VAR(8) as in BQ
model0 <- VAR(vardat0, p = 8, type = "none")
summary(model0)

# estimate SVAR imposing BQ long-run restrictions
model1 <- BQ(model0)
summary(model1)

# get IRF for dyt and dut
irf.dyt <- irf(model1,
               impulse = "dyt",
               boot = FALSE,
               n.ahead = 40)
irf.dut <- irf(model1,
               impulse = "dut",
               boot = FALSE,
               n.ahead = 40)

# get supply shocks (dyt)
# to get shocks to yt we need the cumulative sum
supply <- cbind(cumsum(irf.dyt$irf$dyt[, 1]), irf.dyt$irf$dyt[, 2])

# get demand shocks (dut)
# to get shocks to yt we need the cumulative sum
# to match BQ graphs multiply by -1
# (negative shock rather than positive)
demand <-
  cbind(-1 * cumsum(irf.dut$irf$dut[, 1]),-1 * irf.dut$irf$dut[, 2])

# plot IRFs - demand shocks
plot.ts(
  demand[, 1],
  col = "black",
  lwd = 2,
  ann = FALSE,
  xlim = c(0, 40),
  ylim = c(-.6, 1.4)
)
lines(demand[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(
  x = "topright",
  c("Output response", "Unemployment response"),
  col = c("black", "blue"),
  lwd = 2,
  bty = "n"
)

# plot IRFs - supply shocks
plot.ts(
  supply[, 1],
  col = "black",
  lwd = 2,
  ann = FALSE,
  xlim = c(0, 40),
  ylim = c(-.6, 2)
)
lines(supply[, 2], col = "blue", lwd = 2)
abline(h = 0)
legend(
  x = "topright",
  c("Output response", "Unemployment response"),
  col = c("black", "blue"),
  lwd = 2,
  bty = "n"
)

