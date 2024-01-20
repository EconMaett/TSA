// local level model with intervention variable
// stochastic level

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] w;
}

parameters {
  vector[n] mu;
  real lambda;
  real<lower=0> sigma_level;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] yhat;
  yhat = mu + lambda * w;
}

model {
  for(t in 2:n)
    mu[t] ~ normal(mu[t-1], sigma_level);

  y ~ normal(yhat, sigma_irreg);
}
