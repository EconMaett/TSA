// local level model with:
// deterministic level
// stochastic seasonal (monthly)
// time varying parameter
// stochastic intervention

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] x;
  vector[n] w;
}

parameters {
  real mu;
  vector[11] seas;
  real beta;
  real lambda;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] seasonal;
  vector[n] yhat;
  for(t in 1:11) {
    seasonal[t] = seas[t];
  }
  for(t in 12:n) {
    seasonal[t] = - sum(seasonal[t-11:t-1]);
  }

  yhat = mu + beta * x + lambda * w;
}

model {
  y ~ normal(yhat + seasonal, sigma_irreg);
}
