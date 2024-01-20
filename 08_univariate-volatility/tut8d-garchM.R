# GARCH in Mean
# Using S&P500 excess returns
# Kevin Kotze

## clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library(tidyverse)
library(tsm)

# load data and create time series object
# S&P 500 monthly returns from 1926 to 1991
dat <- read_csv(file = "sp500.csv")

sp500 <- dat$sp500

GARCHmean <- garchM(sp500 * 100, 1) # 1 is for typical GARCH in mean
# mu = mean in mean equation
# gamma = relationship between mean and volatility
# omega = constant in volatility
# alpha = ARCH(1)
# beta = GARCH(1)

# Note that gamma is insignificant so there is no risk premium
