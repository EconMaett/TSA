// local level model with time varying parameter
// deterministic level

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] x;
}

parameters {
  real mu;
  real beta;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] yhat;
  yhat = mu + beta * x;
}

model {
  y ~ normal(yhat, sigma_irreg);
}
