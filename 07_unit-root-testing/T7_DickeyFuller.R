# Compute critical values of the Dickey Fuller test
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

set.seed(123456)         # seed for random numbers
noo <- 100                  # number of observations
phi <- 1                    # AR(1) coefficient
nos <- 10000                # number of simulations

phi_mat <- rep(0,nos)       # vector for phi estimates

#i <- 1

for (i in 1:nos){
  y <- stats::filter(rnorm(noo), filter = phi, method = 'recursive') # generate AR(1) process
  x <- y[-noo]               # regressors
  y <- y[-1]                 # dependent variables
  phiest <- (lm(y ~ x - 1))$coef
  phi_mat[i] <- phiest       # store estimated values of phi
  }

hist(phi_mat, 50, main="", xlab="")  # plot phi estimates - note bias below one
cat("bias = ", bias <- mean(phi_mat - phi))  # bias suggests estimated value is less than 1
