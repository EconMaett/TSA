// local level + slope model
// stochastic level
// deterministic slope

data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  vector[n] mu;
  real v;
  real<lower=0> sigma_level;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] yhat;
  yhat = mu;
}

model {
  mu[1] ~ normal(y[1], sigma_level);
  for(t in 2:n)
    mu[t] ~ normal(mu[t-1] + v, sigma_level);

  y ~ normal(yhat, sigma_irreg);

  sigma_level ~ student_t(4, 0, 1);
  sigma_irreg ~ student_t(4, 0, 1);
  v ~ normal(0, 5);
}
