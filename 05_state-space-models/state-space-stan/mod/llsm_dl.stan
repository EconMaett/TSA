// local level + slope model
// deterministic level
// stochastic slope

data {
  int<lower=1> n;
  vector[n] y;
}
parameters {
  real mu;
  vector[n-1] v;
  real<lower=0> sigma_drift;
  real<lower=0> sigma_irreg;
}
transformed parameters {
  vector[n] yhat;
  yhat[1] = mu;
  for(t in 2:n) {
    yhat[t] = yhat[t-1] + v[t-1];
  }
}
model {
  for(t in 2:n-1)
    v[t] ~ normal(v[t-1], sigma_drift);

  y ~ normal(yhat, sigma_irreg);
}
