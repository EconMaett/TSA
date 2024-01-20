# BVAR Model - Minnesota Prior
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

# install packages ----
#devtools::install_github("kthohr/BMR")

# load package ----
library(BMR)

# set working directory
#getwd()
setwd('~/git/tsm/ts-9-tut')
#setwd("C:\\Users\\image")

# load data ----
dat <- read.csv(file="USdata.csv")

# plot data ----
gtsplot(dat[,2:4], dates = dat[,1])

# setup the model and estimate the parameters  ----

bvar_obj <- new(bvarm)

bvar_obj$build(data.matrix(dat[,2:4]),
               TRUE, # constant
               4) # lags

prior<-c(1,1,1) # assume all variables have unit root

bvar_obj$prior(prior, # priors
               1, # var_type
               1, # decay_type
               0.2, # HP1
               0.5, # HP2
               10^5, # HP3
               1.0) # HP4

# var_type -  "1" implies geometric decay as in Koop & Korobilis 2010
# HP1 - first hyperparameter (Canova, 2007) recommends value of 0.2
# HP2 - first hyperparameter (Canova, 2007) recommends value of 0.5
# HP3 - first hyperparameter (Canova, 2007) recommends value of 10^5

bvar_obj$gibbs(10000) # number of draws

# number of draws - Number of Gibbs sampling replications to keep from the sampling run

# plot posterior densities for the coefficients ----
plot(bvar_obj, var_names = colnames(dat)[-1], save = FALSE)

# impulse response functions ----
IRF(bvar_obj, 20, var_names = colnames(dat)[-1], save = FALSE)

# generate a forecast for each of the variables ----
forecast(bvar_obj, shocks=TRUE, var_names=colnames(dat)[-1], back_data=12, save=FALSE)

# to save the results of the forecast
predict <- forecast(bvar_obj, shocks=TRUE, var_names=colnames(dat)[-1], save=TRUE)
