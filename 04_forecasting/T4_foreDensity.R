# Tut 3
# Forecasting the density of GDP growth

## clear data & close graphs
rm(list = ls())
graphics.off()

# make use of a library
library(tsm)
library(forecast)
library(strucchange)
library(tidyverse)
library(lubridate)
library(sarb2020q1)


# load data and create time series object
# use data from the sarb2020q1 package
gdp <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1),
         grow_lag = lag(growth)) %>%
  drop_na()

# plot data
gdp %>%
  pull(growth) %>%
  plot.ts()

# Set new sample
y <- gdp %>%
  filter(date > ymd('1981-01-01')) %>%
  select(date, growth)

# Set horizon and intervals
H <- 16
percentiles <- c(0.10, 0.15, 0.25, 0.35)
num <- dim(y)[1]
inter <- length(percentiles) * 2

# Generate forecast for AR(1) models
ar_mod <- tibble(forecast = y %>%
                   pull(growth) %>%
                   arima(., order = c(1, 0, 0)) %>%
                   predict(., n.ahead = H) %>%
                   pluck(1))

# Add a column for the mse
ar_mod <- ar_mod %>%
  mutate(mse = rep(0, H))

for (i in 1:H) {
  ar_mod$mse[i] <- var(c(y$growth, ar_mod$forecast[i]))
}

# prepare for fan chart
fanChart <- matrix(rep(0, H * inter), nrow = H)

for (i in 1:length(percentiles)) {
  z2 <- abs(qnorm(percentiles[i] / 2, 0, 1))
  forcInt <- matrix(rep(0, H * 2), ncol = 2)

  for (h in 1:H) {
    forcInt[h, 1] <- ar_mod$forecast[h] - z2 * sqrt(ar_mod$mse[h])
    forcInt[h, 2] <- ar_mod$forecast[h] + z2 * sqrt(ar_mod$mse[h])

    fanChart[h, i] <- forcInt[h, 1]
    fanChart[h, inter - i + 1] <- forcInt[h, 2]
  }
}

# Plot point forecast
plot_len <- num + H

plot_yt <- c(y$growth, rep(NaN, H))
plot_ytF <- c(rep(NaN, num), ar_mod$forecast)

plot.ts(plot_yt, type = "o")
lines(plot_ytF, col = "red", lty = 1)

# Plot Density
plot_ytD <- rbind(matrix(rep(NaN, num * inter), ncol = inter), fanChart)

no_int <- dim(plot_ytD)

for (i in 1:no_int[2]) {
  lines(plot_ytD[, i], col = "darkgrey", lty = 2)
}

# Or alternatively
start_obs <- num + 1

plot(
  plot_yt,
  type = "o",
  ylim = c(-6, 6),
  ylab = "",
  xlab = ""
)

polygon(c(c(start_obs:plot_len), rev(c(start_obs:plot_len))),
        c(plot_ytD[start_obs:plot_len, 1], rev(plot_ytD[start_obs:plot_len, 8])),
        col = "grey90",
        border = NA)

polygon(c(c(start_obs:plot_len), rev(c(start_obs:plot_len))),
        c(plot_ytD[start_obs:plot_len, 2], rev(plot_ytD[start_obs:plot_len, 7])),
        col = "grey80",
        border = NA)

polygon(c(c(start_obs:plot_len), rev(c(start_obs:plot_len))),
        c(plot_ytD[start_obs:plot_len, 3], rev(plot_ytD[start_obs:plot_len, 6])),
        col = "grey70",
        border = NA)

polygon(c(c(start_obs:plot_len), rev(c(start_obs:plot_len))),
        c(plot_ytD[start_obs:plot_len, 4], rev(plot_ytD[start_obs:plot_len, 5])),
        col = "grey60",
        border = NA)

lines(plot_ytF, col = "red", lty = 1)
