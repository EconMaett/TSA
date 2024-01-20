# Tut 5
# SS local level model with stan

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
model_file <- 'mod/llm.stan'
cat(paste(readLines(model_file)), sep = '\n')

# estimate model parameters
fit <- stan(file = model_file, data = standata,
            warmup = 4000, iter = 10000, chains = 2)
#fit <- stanc_builder(file = model_file, isystem = 'home/kevin/git/tsm/ts-5-tut/tut/T5-state-space/mod/ssm.stan')

# summary of results
summary(fit)$summary
mu <- get_posterior_mean(fit, par = 'mu')[, 'mean-all chains']
sigma_irreg <- get_posterior_mean(fit, par = 'sigma_irreg')[, 'mean-all chains']
sigma_level <- get_posterior_mean(fit, par = 'sigma_level')[, 'mean-all chains']
resids <- dat$defl - mu

# plot results for errors
print(fit, pars = c('sigma_level',
                    'sigma_irreg'))
bayesplot::mcmc_areas(as.matrix(fit),
                      regex_pars = c("sigma_level", "sigma_irreg"))

# plot results for fitted values
plot.ts(dat$defl)
lines(mu, col = "red")

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

