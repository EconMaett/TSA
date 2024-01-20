# Tut 5
# SS local level slope model with stan

# clear data & close graphs
rm(list=ls())
graphics.off()

# Install packages
# install.packages('rstan', repos='https://cran.rstudio.com/', dependencies=TRUE)

# make use of a library
library(tsm)
library(rstan)
library(tidyverse)
library(lubridate)
library(sarb2020q1)

# use data from the sarb2020q1 package
dat <- sarb_quarter %>%
  select(date, KBP6006L, KBP6006D) %>%
  mutate(defl = c(0, diff(log(KBP6006L/KBP6006D)*100, lag = 1))) %>%
  dplyr::filter(date > '1981-01-01')

# drop_na()
# dplyr::filter(date > '1981-01-01')

plot.ts(dat$defl)

# prepare data for stan which will use c++
standata <- within(list(), {
  y <- dat$defl
  n <- length(y)
})

# read model structure
model_file <- 'mod/llsm.stan'
cat(paste(readLines(model_file)), sep = '\n')

lmresult <- lm(dat$defl ~ x, data = data.frame(x = 1:length(dat$defl), y = as.numeric(dat$defl)))
init <- list(list(mu = rep(mean(dat$defl), length(dat$defl)),
                  v = rep(coefficients(lmresult)[[2]], length(dat$defl) - 1),
                  sigma_level = sd(dat$defl) / 2,
                  sigma_drift = sd(dat$defl) / 2,
                  sigma_irreg = 0.001))

# estimate model parameters
fit <- stan(file = model_file, data = standata,
            warmup = 4000, iter = 10000, chains = 2)

# summary of results
summary(fit)$summary
mu <- get_posterior_mean(fit, par = 'mu')[, 'mean-all chains']
v <- get_posterior_mean(fit, par = 'v')[, 'mean-all chains']
sigma <- get_posterior_mean(fit, par = 'sigma')[, 'mean-all chains']
sigma_drift <- sigma[[1]]
sigma_irreg <- sigma[[2]]
sigma_level <- sigma[[3]]
resids <- dat$defl - mu - c(0,v)

# plot results for errors
print(fit, pars = 'sigma')
bayesplot::mcmc_areas(as.matrix(fit),
                      regex_pars = "sigma")

# plot results for fitted values
plot.ts(dat$defl)
lines(mu, col = "red")

plot.ts(v)


# plot results for residuals
plot.ts(resids)

# Diagnostics
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
hist(
  resids,
  prob = TRUE,
  col = "grey",
  main = "",
  breaks = seq(-4.5, 7, length.out = 30)
)

# acf
ac(resids)

# joint autocorrelation
Box.test(resids,
         lag = 12,
         type = "Ljung",
         fitdf = 2) # No autocorrelation when p-val bigger than 5%

# normality
shapiro.test(resids) # Residuals normal when p-val bigger than 5%

# To complete
loglik <- ?
num_coef <- ?
r.aic <- (2*(loglik)) + 2*(sum(num_coef))
r.bic <- (2*(loglik)) + (log(length(dat$defl)))*(num_coef)

# Kalman Filter & Smoother
filtered <- ?
smoothed <- ?

