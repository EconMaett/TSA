# Tut 3
# Forecasting for GDP growth

## clear data & close graphs
rm(list = ls())
graphics.off()

# devtools::install_gitlab("KevinKotze/sarb2020q1")
# devtools::install_github("KevinKotze/tsm")
# install.packages('strucchange', repos='https://cran.rstudio.com/', dependencies=TRUE)
# install.packages('forecast', repos='https://cran.rstudio.com/', dependencies=TRUE)

# detach all non-base packages

# make use of a library
library(tsm)
library(forecast)
library(strucchange)
library(tidyverse)
library(lubridate)
library(sarb2020q1)

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

# Check for break point ----

# generate BP statistic
sa_bp <- breakpoints(growth ~ grow_lag, data = gdp, breaks = 5)
summary(sa_bp)  # where the breakpoints are
plot(sa_bp, breaks = 5)

# date that relates to the observation number of the break
gdp %>%
  slice(sa_bp$breakpoint)

# Set new sample
y <- gdp %>%
  filter(date > ymd('1981-01-01')) %>%
  select(date, growth)

# look at persistence
y %>%
  pull(growth) %>%
  ac()

# Prepare for forecast ----

# data series "y"
# - forecast over 10 years for 8 steps ahead
H <- 8
P <- 40
R <- dim(y)[1] - (P + H) + 1

# split data into initial training and testing portions
train <- y %>%
  slice_head(n = R)

test <- y %>%
  slice_tail(n = (P + H - 1))

# check to ensure that it looks correct
train %>%
  tail()
test %>%
  head()

# illustrate how the data will be stored
actual <- matrix(data = rep(0, P * H), ncol = H)

# add an observation number to the dataset
illust <- y %>%
  mutate(num = seq(1, n()))

for (i in 1:P) {
  first <- R + i
  last <- first + H - 1
  actual[i, 1:H] <- illust$num[first:last]
}

# that looks good, now let's use actual data
actual <- matrix(data = rep(0, P * H), ncol = H)

for (i in 1:P) {
  first <- R + i
  last <- first + H - 1
  actual[i, 1:H] <- y$growth[first:last]
}

# store forecasts for arma(1,1) model
arma_fore <- matrix(data = rep(0, P * H), ncol = H)

for (i in 1:P) {
  arma_fore[i, 1:H] <- y %>%
    slice_head(n = (R + i - 1)) %>%
    pull(growth) %>%
    arima(., order = c(1, 0, 1)) %>%
    predict(., n.ahead = H) %>%
    pluck(1)
}

# store forecasts for ar(1) model
ar_fore <- matrix(data = rep(0, P * H), ncol = H)

for (i in 1:P) {
  ar_fore[i, 1:H] <- y %>%
    slice_head(n = (R + i - 1)) %>%
    pull(growth) %>%
    arima(., order = c(1, 0, 0)) %>%
    predict(., n.ahead = H) %>%
    pluck(1)
}

# Plot a forecast
rowf <- 30

par(mfrow = c(1, 1))
plot.ts(
  actual[rowf,],
  col = "black",
  ylab = "Example",
  xlab = "Steps",
  ylim = c(min(actual[rowf,] * (-2)), max(actual[rowf,] * 2))
)
lines(ar_fore[rowf,],
      col = "red",
      lty = 2,
      lwd = 2)
legend(
  "topright",
  legend = c("actual", "forecast"),
  lty = c(1, 2),
  col = c("black", "red"),
  bty = "n"
)

# Calculate forecast error
error_arma <- actual - arma_fore
error_ar <- actual - ar_fore

# Calculate the bias at each step ahead
bias_arma_step <-  colMeans(error_arma)
bias_ar_step <- colMeans(error_ar)

print(bias_ar_step)

# Calculate the bias at each point in time
bias_arma_time <- rowMeans(error_arma)
bias_ar_time <- rowMeans(error_ar)

print(bias_ar_time)

# Calculate root mean square error ----

# averaged over time for each H
RMSE_arma_step <- sqrt(colMeans(error_arma ^ 2))
RMSE_ar_step <- sqrt(colMeans(error_ar ^ 2))

# averaged over steps for each time period
RMSE_arma_time <- sqrt(rowMeans(error_arma ^ 2))
RMSE_ar_time <- sqrt(rowMeans(error_ar ^ 2))

# plot results
RMSE_arma_step %>%
  plot()

RMSE_ar_step %>%
  plot()

RMSE_arma_time %>%
  plot()

RMSE_ar_time %>%
  plot()

# Diebold Mariano statistic ----

# Calculate DM stats for each step
dm_steps <- rep(0, H)

for (i in 1:H) {
  dm_steps[i] <-
    dm.test(error_arma[, i], error_ar[, i], h = i)$statistic
}

plot(dm_steps,
     ylim = c(-3, 3),
     xlab = "Steps",
     ylab = "DM statistic")
lines(rep(0, H), col = "grey", lty = 1)
lines(rep(1.96, H), col = "red", lty = 2)
lines(rep(-1.96, H), col = "red", lty = 2)
# most are within confidence bands so not significantly different
# the second and seventh observation is above the confidence interval

RMSE_arma_step[2]
RMSE_ar_step[2]
# so the AR(1) provides the better forecast for h=2 and when compared to the
# ARMA(1,1) the difference in forecasting performance at this horizon is
# significantly different

# Store DM stats for each time period
dm_time <- rep(0, P)

for (i in 1:P) {
  dm_time[i] <-
    dm.test(error_arma[i,], error_ar[i,], h = 1)$statistic
}

plot(dm_time,
     ylim = c(-3, 3),
     xlab = "Steps",
     ylab = "DM statistic")
lines(rep(0, P), col = "grey", lty = 1)
lines(rep(1.96, P), col = "red", lty = 2)
lines(rep(-1.96, P), col = "red", lty = 2)

# Now identify which model is better at these points in time
RMSE_arma_time[5]
RMSE_ar_time[5]
# again the AR(1) is better and note that at no time was it significantly worse

# Clarke-West statistic ----

# Since these models are nested within one another the DM-test is not appropriate
# Note that this is a one-sided test that is used to test if the bigger model provides
# superior forecasts and should not be used to test if the performance of the smaller
# model is significantly better than the larger model

cwstat_steps <- rep(0, H)
cwpval_steps <- rep(0, H)

for (i in 1:H) {
  cwstat_steps[i] <-
    cw(error_arma[, i], error_ar[, i], arma_fore[, i], ar_fore[, i])$test
  cwpval_steps[i] <-
    cw(error_arma[, i], error_ar[, i], arma_fore[, i], ar_fore[, i])$pvalue
}

# plot the results
plot(cwstat_steps,
     ylim = c(-3, 3),
     xlab = "Steps",
     ylab = "CW statistic")
lines(rep(0, H), col = "grey", lty = 1)
lines(rep(1.96, H), col = "red", lty = 2)
lines(rep(-1.96, H), col = "red", lty = 2)
# within confidence bands so not significantly different

# Calculate CW stats for each time period
cwstat_time <- rep(0, P)
cwpval_time <- rep(0, P)

for (i in 1:P) {
  cwstat_time[i] <-
    cw(error_arma[i,], error_ar[i,], arma_fore[i,], ar_fore[i,])$test
  cwpval_time[i] <-
    cw(error_arma[i,], error_ar[i,], arma_fore[i,], ar_fore[i,])$test
}

plot(cwstat_time,
     ylim = c(-5, 5),
     xlab = "Time",
     ylab = "CW statistic")
lines(rep(0, P), col = "grey", lty = 1)
lines(rep(1.96, P), col = "red", lty = 2)
lines(rep(-1.96, P), col = "red", lty = 2)

# would conclude that the bigger model does not provide more accurate forecasts

## Draw hairy plot with all forecasts

h_start <- R - 12

y_plot <-
  ts(y$growth[h_start:length(y$growth)], start = c(2005, 1), frequency =
       4)

n_obs <- length(y_plot)
no_fore <- n_obs - P - H
nons <- n_obs - H

fore_plot <- matrix(data = rep(0, n_obs * P), nrow = P)

for (i in 1:P) {
  nan_beg <- rep(NaN, no_fore + i)
  nan_end <- rep(NaN, P - i)
  fore_plot[i, 1:n_obs] <- c(nan_beg, ar_fore[i,], nan_end)
}

length(fore_plot[i, 1:n_obs])
length(c(nan_beg, ar_fore[i,], nan_end))

plot.ts(y_plot)

for (i in 1:P) {
  lines(ts(fore_plot[i,], start = c(2005, 1), frequency = 4),
        col = "red",
        lty = 2)
}
