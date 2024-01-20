# Tut 2
# Simulating and Estimating ARMA models with the Box-Jenkins approach

## clear data & close graphs
rm(list = ls())
graphics.off()

# Don't like the default ACF in R so I use:
devtools::install_github("KevinKotze/tsm")

library(tidyverse)
library(tsm)

# setting the seed so we all get the same results
set.seed(123)

# simulate different ARMA(p,q) processes and store them in a tibble
dat <- tibble(
  ar1 = arima.sim(model = list(ar = 0.8), n = 200),
  ma1 = arima.sim(model = list(ma = 0.7), n = 200),
  arma10 = arima.sim(model = list(ar  = 0.4), n = 200),
  arma01 = arima.sim(model = list(ma = 0.5), n = 200),
  arma11 = arima.sim(model = list(ar = 0.4, ma = 0.5), n = 200),
  arma21 = arima.sim(model = list(
    ar = c(0.6, -0.2), ma = c(0.4)
  ), n = 200)
)

# start with an AR(1) with coefficient of 0.8 ----
dat %>%
  pull(ar1) %>%
  plot.ts()  # alternatively you could use the `plot' function

dat %>%
  pull(ar1) %>%
  ac(., max.lag = 20) # this should plot the ACF and PACF together (both start with lag of 1)

# To display the Ljung-Box statistic for the first lag
dat %>%
  pull(ar1) %>%
  Box.test(., lag = 1, type = "Ljung-Box") # the p-value suggests that the autocorrelation is different from zero

# We could assign the output from the test to an object `Box.res'
Box_res <- dat %>%
  pull(ar1) %>%
  Box.test(., lag = 1, type = "Ljung-Box") # the p-value suggests that the autocorrelation is different from zero

# To check what is in the object
Box_res  # alternatively you could use "print(Box.res)"
names(Box_res)

# If we want to create a vector of Q-stats for the first 10 lags we can assign values to a object
# Create a vector for the Q-stat results
Q_stat <-
  rep(0, 10) # creates a vector for ten observations, where 0 is repeated 10 times
Q_prob <- rep(0, 10)

# Assign value for Q-stat to a different object
Q_stat[1] <- Box.test(dat$ar1, lag = 1, type = "Ljung-Box")$statistic
Q_prob[1] <- Box.test(dat$ar1, lag = 1, type = "Ljung-Box")$p.value

# To view the data in the respective vectors
Q_stat
Q_prob

# Rather than assign each individual values we can use the loop:
for (i in 1:10) {
  Q_stat[i] <- Box.test(dat$ar1, lag = i, type = "Ljung-Box")$statistic
  Q_prob[i] <- Box.test(dat$ar1, lag = i, type = "Ljung-Box")$p.value
}

# To graph both of these statistics together
op <- par(mfrow = c(1, 2)) # create plot area of (1 X 2)
plot(Q_stat, ylab = "", main = 'Q-Statistic')
plot(Q_prob,
     ylab = "",
     ylim = c(0, 1),
     main = 'Probability values')
par(op) # close plot area
# p-value value suggest there is significant autocorrelation

# To speed up the above computation you could use the purrr::map() function

# We can now fit a model to this process
arma10 <- dat %>%
  pull(ar1) %>%
  arima(., order = c(1, 0, 0), include.mean = FALSE) # uses ARIMA(p,d,q) specification
arma10  # where the slots contain #summary(arma10)

# Take a look at what the residuals look like
par(mfrow = c(1, 1))
arma10$residuals %>%
  plot()

# Consider the ACF and PACF of the residuals
arma10$residuals %>%
  ac(., max.lag = 20)

arma10$residuals %>%
  Box.test(.,
           lag = 1,
           type = "Ljung-Box",
           fitdf = 1)

# Now lets work with a MA(1) process ----

# Take a look at the data
par(mfrow = c(1, 1))
dat %>%
  pull(ma1) %>%
  plot.ts()

# Consider the ACF and PACF for the data
dat %>%
  pull(ma1) %>%
  ac(., max.lag = 20)

# Fit model
arma01 <- dat %>%
  pull(ma1) %>%
  arima(., order = c(0, 0, 1)) # uses ARIMA(p,d,q) with constant
arma01

# Take a look at what the residuals look like
par(mfrow = c(1, 1))
arma01$residuals %>%
  plot()

# Consider the ACF and PACF of the residuals
arma01$residuals %>%
  ac(., max.lag = 20)

arma01$residuals %>%
  Box.test(.,
           lag = 1,
           type = "Ljung-Box",
           fitdf = 2)

# Look at a ARMA model ----

# autocorrelation function for ARMA(1,0)
dat %>%
  pull(arma10) %>%
  ac(., max.lag = 20)

# autocorrelation function for ARMA(0,1)
dat %>%
  pull(arma01) %>%
  ac(., max.lag = 20)

# autocorrelation function for ARMA(1,1)
dat %>%
  pull(arma11) %>%
  ac(., max.lag = 20)

# Now lets simulate an ARMA(2,1) process

# Take a look at the data
par(mfrow = c(1, 1))
dat %>%
  pull(arma21) %>%
  plot()

# Consider the ACF and PACF for the data
dat %>%
  pull(arma21) %>%
  ac(., max.lag = 20)

# Check the fit model with AIC
arma_res <- rep(0, 16)
arma_res[1] <-
  arima(dat$arma21, order = c(3, 0, 2))$aic # fit arma(3,2) and save aic value
arma_res[2] <- arima(dat$arma21, order = c(2, 0, 2))$aic
arma_res[3] <- arima(dat$arma21, order = c(2, 0, 1))$aic
arma_res[4] <- arima(dat$arma21, order = c(1, 0, 2))$aic
arma_res[5] <- arima(dat$arma21, order = c(1, 0, 1))$aic
arma_res[6] <- arima(dat$arma21, order = c(3, 0, 0))$aic
arma_res[7] <- arima(dat$arma21, order = c(2, 0, 0))$aic
arma_res[8] <- arima(dat$arma21, order = c(0, 0, 2))$aic
arma_res[9]  <-
  arima(dat$arma21, order = c(3, 0, 2), include.mean = FALSE)$aic
arma_res[10] <-
  arima(dat$arma21, order = c(2, 0, 2), include.mean = FALSE)$aic
arma_res[11] <-
  arima(dat$arma21, order = c(2, 0, 1), include.mean = FALSE)$aic
arma_res[12] <-
  arima(dat$arma21, order = c(1, 0, 2), include.mean = FALSE)$aic
arma_res[13] <-
  arima(dat$arma21, order = c(1, 0, 1), include.mean = FALSE)$aic
arma_res[14] <-
  arima(dat$arma21, order = c(3, 0, 0), include.mean = FALSE)$aic
arma_res[15] <-
  arima(dat$arma21, order = c(2, 0, 0), include.mean = FALSE)$aic
arma_res[16] <-
  arima(dat$arma21, order = c(0, 0, 2), include.mean = FALSE)$aic

arma_res
min(arma_res) # smallest value from ARMA(2,0)
which(arma_res == min(arma_res))

# Re-estimate the model and store all the results
arma11 <- arima(dat$arma21, order = c(1, 0, 1), include.mean = FALSE)
arma11

# Consider residuals
arma11$residuals %>%
  ac(., max.lag = 20)

# Lets generate the Q-stats
# - when applied to residuals you need correct for degrees of freedom (fitdf = p+q)
Q_stat <- rep(NA, 10) # vector of ten observations
Q_prob <- rep(NA, 10)

for (i in 4:10) {
  Q_stat[i] <-
    Box.test(arma11$residuals,
             lag = i,
             type = "Ljung-Box",
             fitdf = 3)$statistic
  Q_prob[i] <-
    Box.test(arma11$residuals,
             lag = i,
             type = "Ljung-Box",
             fitdf = 3)$p.value
}

op <- par(mfrow = c(1, 2))
plot(Q_stat, ylab = "", main = 'Q-Statistic')
plot(Q_prob,
     ylab = "",
     ylim = c(0, 1),
     main = 'Probability values')
par(op)
# Looking good!
