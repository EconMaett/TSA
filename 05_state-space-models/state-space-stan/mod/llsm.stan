// local level + slope model
// stochastic level
// stochastic slope

data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  vector[n] mu;
  vector[n-1] v;
  positive_ordered[3] sigma;
}

transformed parameters {
  vector[n] yhat;
  yhat = mu;
}

model {
  v[1] ~ normal(0, sigma[1]);
  for(t in 2:n-1)
    v[t] ~ normal(v[t-1], sigma[1]);

  mu[1] ~ normal(y[1], sigma[3]);
  for(t in 2:n)
    mu[t] ~ normal(mu[t-1] + v[t-1], sigma[3]);

  y ~ normal(yhat, sigma[2]);

  sigma ~ student_t(4, 0, 1);
}
