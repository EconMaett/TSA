// local level seasonal model (monthly)
// deterministic level
// deterministic seasonal (monthly)

data {
  int<lower=1> n;
  vector[n] y;
}

parameters {
  real mu;
  vector[11] seas;
  real<lower=0> sigma_irreg;
}

transformed parameters {
  vector[n] seasonal;
  vector[n] yhat;
  for(t in 1:11)
    seasonal[t] = seas[t];

  for(t in 12:n)
    seasonal[t] = - sum(seasonal[t-11:t-1]);

  yhat = mu + seasonal;
}

model {
  y ~ normal(yhat, sigma_irreg);
}
