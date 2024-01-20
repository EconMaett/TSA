// local level seasonal model (quarterly)
// stocashtic level
// stochastic seasonal (quarterly)

data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  vector[n] mu;
  vector[n] seasonal;
  real<lower=0> sigma_level;
  real<lower=0> sigma_seas;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] yhat;
  yhat = mu + seasonal;
}

model {
  for(t in 4:n)
    seasonal[t] ~ normal(- sum(seasonal[t-3:t-1]), sigma_seas);

  for(t in 2:n)
    mu[t] ~ normal(mu[t-1], sigma_level);

  y ~ normal(yhat, sigma_irreg);
}
