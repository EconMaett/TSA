# Compute critical values of the Dickey Fuller test and check the power of the statistic
# Kevin Kotze

## clear data & close graphs
rm(list=ls())
graphics.off()

set.seed(123456)            # seed for random numbers
noo <- 100                  # number of observations
phi <- 0.95                 # AR(1) coefficient
nos <- 10000                # number of simulations

phi_mat <- rep(0,nos)       # vector for phi estimates
tvalue <- rep(0,nos)        # vector for t-value estimates

#i <- 1

for (i in 1:nos){
  y <- stats::filter(rnorm(noo), filter = phi, method = 'recursive') # generate AR(1) process
  x <- y[-noo]               # regressors
  y <- y[-1]                 # dependent variables
  phiest <- (lm(y ~ x - 1))$coef
  phi_mat[i] <- phiest                # store estimated values of phi
  sig <- sum((y - phiest*x)^2)/(noo - 1)     # estimate variance of residual
  #vars <- sig*solve((t(x))%*%x)             # variance of phi hat
  vars <- sig*solve(Conj(t(x)) %*% x)        # variance of phi hat
  tvalue[i] <- (phiest - phi)/sqrt(vars)     # t-value of phi hat
}

terror1 <- rep(0,nos)
for (i in 1:nos){
  if (tvalue[i] < -2.5866)    # critical value - DF tables, noo = 100 at 1%
    terror1[i] <- 1           # not possible to reject null of unit root
}

terror2 <- rep(0,nos)
for (i in 1:nos){
  if (tvalue[i] < -1.9433)    # critical value - DF tables, noo = 100 at 5%
    terror2[i] <- 1           # not possible to reject null of unit root
}

terror3 <- rep(0,nos)
for (i in 1:nos){
  if (tvalue[i] < -1.6174)    # critical value - DF tables, noo = 100 at 10%
    terror3[i] <- 1           # not possible to reject null of unit root
}

# Results
cat("AR(1) coefficient = ", phi,"\n")
cat("rejection probability at 1% = ", sum(terror1)/nos,"\n")
cat("rejection probability at 5% = ", sum(terror2)/nos,"\n")
cat("rejection probability at 10% = ", sum(terror3)/nos,"\n")
