# Stochastic Volatility Models
# Using ZARUSD data
# Kevin Kotze

# Mean equation: y_t = \exp^{h_t/2} \epsilon_t
# Volatility   : h_t = \mu + \phi [h_{t-1} - \mu] + \sigma \eta_t

# Largely equivalent to the model discussed in class, but allows for cases where volatility is highly persistent
#     - i.e. for use with high frequency data
# We use Bayesian techniques with the default uniformative priors (which is a form of QML estimator)

## clear data & close graphs
rm(list = ls())
graphics.off()

# load package
library('stochvol')

# load data and create time series object
# daily exchange rates from 31dec97 to 30dec2011
dat <- read_csv(file = "ex_data.csv")

plot.ts(dat$ZAR)

zar.logret <- diff(log(dat$ZAR))  # log-returns
zar.dat <- zar.logret - mean(zar.logret)  # de-mean

zar.ins <- zar.dat[1:3622]
zar.out <- zar.dat[3623:3652]

plot.ts(abs(zar.ins))

res.sv <- svsample(zar.ins)   # run MCMC sampler for SV model

volplot(res.sv) # plot volatility (h_t)
paradensplot(res.sv) # posterior densities of parameters

print(summary(res.sv))

sv.pred <- predict(res.sv, 30)

sv.fore <-
  t(matrix(apply(
    100 * exp(sv.pred$h / 2), 2, quantile, c(0.05, 0.5, 0.95)
  ), nrow = 3))


############################################################
## Now let's compare these results to the GARCH(1,1) model
############################################################

# load package
library(fGarch)

res.garch <-
  garchFit(zar.ins ~ garch(1, 1), data = zar.ins, trace = F)
summary(res.garch) #

garch.h_t <- 100 * (res.garch@sigma.t)
sv.h_t <- (100 * exp(res.sv$summary$latent[, 3:5] / 2))

res.pf <-
  read_csv(file = "res_pf.csv")  # model that uses particle filter
sv.pf <- 100 * res.pf[1:3622, 3]

cond.vol <- cbind(garch.h_t, sv.h_t[, 2], sv.pf)

plot.ts(cond.vol,
        plot.type = "single",
        col = c("blue", "green", "orange"))
legend(
  "topleft",
  legend = c("garch", "stochvol", "particle"),
  lty = 1,
  col = c("blue", "green", "orange")
)

# Now let's forecast

garch.pred <- predict(res.garch, n.ahead = 30, crit_val = 2)
garch.fore <- 100 * (garch.pred)

results <- cbind((100 * zar.out) ^ 2, garch.fore[, 3], sv.fore[, 2])
plot.ts(results,
        plot.type = "single",
        col = c("red", "blue", "green"))
legend(
  "topright",
  legend = c("actual", "garch", "stochvol"),
  lty = 1,
  col = c("red", "blue", "green")
)
