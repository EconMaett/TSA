# Tut 3
# Density forecasting for GDP growth

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

# Settings
mu <- 0.4
phi <- 0.7
sigma2 <- 0.10

Nbig <- 10000
Nsmall <- 100

# observed time series
yt <- c(2.5, 2.2, 1.8, 2.0, 2.1, 1.5, 1.8, 2)

# set percentiles
percentiles <- c(0.10, 0.15, 0.25, 0.35)

# hypothetical forecasts
ytF <-
  c(1.80,  1.66,	1.56,	1.49,	1.45,	1.41,	1.39,	1.37,	1.36,	1.35)

# properties of forecast
H <- length(ytF)
n_obs <- length(yt)
inter <- length(percentiles) * 2

# Sort out MSE
mse <- rep(0, H)
mse[1] <- sigma2

for (i in 2:H) {
  mse[i] <- mse[i - 1] + phi ^ (2 * (i - 1)) * sigma2
}

# Generate data for fan chart
fanChart <- matrix(rep(0, H * inter), nrow = H)

for (i in 1:length(percentiles)) {
  z2 <- abs(qnorm(percentiles[i] / 2, 0, 1))
  forcInt <- matrix(rep(0, H * 2), ncol = 2)

  for (h in 1:H) {
    forcInt[h, 1] <- ytF[h] - z2 * sqrt(mse[h])
    forcInt[h, 2] <- ytF[h] + z2 * sqrt(mse[h])

    fanChart[h, i] <- forcInt[h, 1]
    fanChart[h, inter - i + 1] <- forcInt[h, 2]
  }
}

# Plot point forecast
plot_len <- n_obs + H

plot_yt <- c(yt, rep(NaN, H))
plot_ytF <- c(rep(NaN, n_obs), ytF)

plot.ts(plot_yt, type = "o")
lines(plot_ytF, col = "red", lty = 2)

# Plot Density
plot(plot_yt, type = "o", ylim = c(0, 4))
plot_ytD <- rbind(matrix(rep(NaN, n_obs * inter), ncol = inter), fanChart)
no_int <- dim(plot_ytD)

for (i in 1:no_int[2]) {
  lines(plot_ytD[, i], col = "red", lty = 2)
}

# Or alternatively
plot(
  plot_yt,
  type = "o",
  ylim = c(0, 4),
  ylab = "",
  xlab = ""
)

polygon(c(c(1:18), rev(c(1:18))),
        c(plot_ytD[, 1], rev(plot_ytD[, 8])),
        col = "grey90",
        border = NA)

polygon(c(c(1:18), rev(c(1:18))),
        c(plot_ytD[, 2], rev(plot_ytD[, 7])),
        col = "grey80",
        border = NA)

polygon(c(c(1:18), rev(c(1:18))),
        c(plot_ytD[, 3], rev(plot_ytD[, 6])),
        col = "grey70",
        border = NA)

polygon(c(c(1:18), rev(c(1:18))),
        c(plot_ytD[, 4], rev(plot_ytD[, 5])),
        col = "grey60",
        border = NA)

# Consider using random normal generator

# Calculate Density
forcDensityBig <- matrix(rep(0, H * Nbig), ncol = Nbig)
forcDensitySmall <- matrix(rep(0, H * Nsmall), ncol = Nsmall)

for (i in 1:H) {
  forcDensityBig[i, ] <- rnorm(Nbig, ytF[i], sqrt(mse[i]))
  forcDensitySmall[i, ] <- rnorm(Nsmall, ytF[i], sqrt(mse[i]))
}

hist(forcDensitySmall, breaks = 12)

# Add a Normal Curve
h <- hist(forcDensitySmall, breaks = 12)
xfit <- seq(min(forcDensitySmall), max(forcDensitySmall), length = 40)
yfit <-
  dnorm(xfit,
        mean = mean(forcDensitySmall),
        sd = sd(forcDensitySmall))
yfit <- yfit * diff(h$mids[1:2]) * length(forcDensitySmall)
lines(xfit, yfit, col = "blue", lwd = 2)

hist(forcDensityBig, breaks = 24)
