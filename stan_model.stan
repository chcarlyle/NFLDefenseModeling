data {
  int<lower=1> N;                 // number of plays
  int<lower=1> D;                 // number of defenses
  int<lower=1> O;                 // number of offenses
  int<lower=1, upper=D> defense[N]; // defense team ID for play i
  int<lower=1, upper=O> offense[N]; // offense team ID for play i
  
  vector[N] DL_involved;          // indicator/covariate: DL pressure
  vector[N] LB_involved;          // indicator/covariate: LB involvement
  vector[N] DB_involved;          // indicator/covariate: DB involvement
  vector[N] is_pass;              // pass indicator
  
  vector[N] y;                    // outcome: EPA
}

parameters {
  real alpha;                     // global intercept
  
  // league-wide effects
  real beta_DL;
  real beta_LB;
  real beta_DB;
  real beta_pass;
  
  // defense random slopes
  vector[D] def_DL_raw;
  vector[D] def_LB_raw;
  vector[D] def_DB_raw;
  
  // offense random intercepts
  vector[O] off_raw;
  
  // hierarchical standard deviations
  real<lower=0> sigma;         // residual sd
  real<lower=0> sigma_def_DL;
  real<lower=0> sigma_def_LB;
  real<lower=0> sigma_def_DB;
  real<lower=0> sigma_off;
}

transformed parameters {
  vector[D] def_DL = sigma_def_DL * def_DL_raw;
  vector[D] def_LB = sigma_def_LB * def_LB_raw;
  vector[D] def_DB = sigma_def_DB * def_DB_raw;
  vector[O] off    = sigma_off * off_raw;
}

model {
  // Priors
  alpha ~ normal(0, 1);
  
  beta_DL ~ normal(0, 1);
  beta_LB ~ normal(0, 1);
  beta_DB ~ normal(0, 1);
  beta_pass ~ normal(0, 1);
  
  def_DL_raw ~ normal(0, 1);
  def_LB_raw ~ normal(0, 1);
  def_DB_raw ~ normal(0, 1);
  off_raw    ~ normal(0, 1);
  
  sigma ~ exponential(1);
  sigma_def_DL ~ exponential(1);
  sigma_def_LB ~ exponential(1);
  sigma_def_DB ~ exponential(1);
  sigma_off ~ exponential(1);
  
  // Likelihood
  for (i in 1:N) {
    real mu = alpha
      + (beta_DL + def_DL[defense[i]]) * DL_involved[i]
      + (beta_LB + def_LB[defense[i]]) * LB_involved[i]
      + (beta_DB + def_DB[defense[i]]) * DB_involved[i]
      + beta_pass * is_pass[i]
      + off[offense[i]];
    
    y[i] ~ normal(mu, sigma);
  }
}
