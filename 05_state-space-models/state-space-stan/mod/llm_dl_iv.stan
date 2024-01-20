// local level model with intervention variable
// deterministic level

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] w;
}

parameters {
  real mu;
  real lambda;
  real<lower=0> sigma_irreg;
}
transformed parameters {
  vector[n] yhat;
  yhat = mu + lambda * w;
}
model {
  y ~ normal(yhat, sigma_irreg);
}
