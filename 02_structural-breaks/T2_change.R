# Tut 2
# Testing for change points

# preliminaries ----

#> install packages ----
# commands to install packages - you only need to run these once
install.packages("devtools")
install.packages("changepoint")
install.packages("strucchange")
install.packages("tidyverse")
install.packages("lubridate")

devtools::install_gitlab("KevinKotze/sarbcurrent")

#> clear data & close graphs ----
rm(list = ls())
graphics.off()

#> load library ----
library(changepoint)
library(sarbcurrent)
library(tidyverse)
library(lubridate)

# change points in the mean ----
# set seed for the random number generator
set.seed(123)

#> simulate data ----
sim_mean <- c(rnorm(100, 0, 1),
              rnorm(50, 1.5, 1),
              rnorm(90, 0, 1),
              rnorm(120,-0.8, 1))

#> plot data ----
plot.ts(sim_mean)

# look at `changepoint` package

#> use binary segmentation ----
# binary segmentation
m_binseg <-
  cpt.mean(sim_mean,
           penalty = "BIC",
           method = "BinSeg",
           Q = 5)
plot(m_binseg,
     type = "l",
     xlab = "Index",
     cpt.width = 4)
cpts(m_binseg)

# Segmented neighbour
m_segneigh <-
  cpt.mean(sim_mean,
           penalty = "BIC",
           method = "SegNeigh",
           Q = 5)
plot(m_segneigh,
     type = "l",
     xlab = "Index",
     cpt.width = 4)
cpts(m_segneigh)

# PELT
m_pelt <- cpt.mean(sim_mean, penalty = "BIC", method = "PELT")
plot(
  m_pelt,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)
cpts(m_pelt)

# Manual penalty
m_pm <-
  cpt.mean(sim_mean,
           penalty = "Manual",
           pen.value = "1.5 * log(n)",
           method = "PELT")
plot(
  m_pm,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)
cpts(m_pm)

# change point in the mean of South African Gross Domestic Product ----

# get data in tibble format
data(package = 'sarbcurrent')
View(sarb_quarter, title = "sarb_data")

# basic pipe - use sarb dataset and then select the date & GDP columns
sarb_quarter %>%
  select(date, KBP6006D)

# select the date & GDP columns
# then create a new column called growth - and calculate the growth rate from GDP
# then only include rows where the date is greater than 1960-01-01
# then pull the growth rate out of tibble
# then place this data where we position the full stop
# and assign the result to `m_gdp`

m_gdp <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  pull(growth) %>%
  cpt.mean(., penalty = "SIC", method = "PELT")

plot(
  m_gdp,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)

# find the date for this change point
sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  slice(cpts(m_gdp))

# change points in the variance ----

# simulate data
sim_var <- c(rnorm(100, 0, 1),
             rnorm(50, 0, 2),
             rnorm(90, 0, 1),
             rnorm(120, 0, 0.5))
plot.ts(sim_var)

# perform change point test on variance
v_pelt <- cpt.var(sim_var, method = "PELT")
plot(
  v_pelt,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)
cpts(v_pelt)

# change point in the variance of South African Gross Domestic Product ----
v_gdp <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  pull(growth) %>%
  cpt.var(., method = "PELT")

plot(
  v_gdp,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)

# find the date for the change point
sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  slice(cpts(v_gdp))

# change in mean and variance ----
sim_mv <- c(rnorm(100, 0, 1),
            rnorm(50, 1, 2),
            rnorm(90, 0, 1),
            rnorm(120,-0.8, 0.5))
plot.ts(sim_mv)

# to check for a change in the mean and variance
mv_pelt <- cpt.meanvar(sim_mv, method = "PELT")
plot(mv_pelt)

# change point in the mean and variance of South African Gross Domestic Product ----
mv_gdp <- sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  pull(growth) %>%
  cpt.meanvar(., method = "PELT")

plot(
  mv_gdp,
  type = "l",
  cpt.col = "blue",
  xlab = "Index",
  cpt.width = 4
)

sarb_quarter %>%
  select(date, KBP6006D) %>%
  mutate(growth = 100 * ((KBP6006D / lag(KBP6006D)) - 1)) %>%
  filter(date > "1960-01-01") %>%
  slice(cpts(mv_gdp))
