# Markov-Switching Model
# Spanish Energy Prices
# Kevin Kotze

# clear data & close graphs ----
rm(list=ls())
graphics.off()

library(MSwM)

# Set directory and read data ----
setwd('~/git/tsm/ts-13-tut')
#setwd("C:\\Users\\image")

# Load data and create time series object ----
dat=read.csv('energy.csv')

# Price - Average price of energy (Cent/kwh)
# Oil - Oil price (Euro/barril)
# Gas - Gas price (Euro/MWh)
# Coal - Coal price (Euro/T)
# EurDol - Exchange rate between Dolar-Euro (USD-Euro)
# Ibex35 - Ibex 35 index divided by one thousand
# Demand - Daily demand of energy (GWh)

model=lm(Price~Oil+Gas+Coal+EurDol+Ibex35+Demand, dat) # linear model
mod=msmFit(model,k=2,sw=rep(TRUE,8)) # MSW model
summary(mod) # regression output

plotProb(mod, which=1) # plot fixed probabilities with smoothed probability
plotProb(mod, which=2) # plot Price against the two regimes
plotDiag(mod) # may need to include a lag of Price on the RHS
plotReg(mod, expl="Demand") # not only demand which is giving rise to the two regimes
