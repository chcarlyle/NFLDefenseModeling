library(ggdag)

# Define the DAG structure
qb_defense_dag <- dagify(
  y ~ mu_i,
  mu_i ~ alpha_q + beta_DL + beta_LB + beta_SEC + X,
  alpha_q ~ mu_alpha,
  beta_DL ~ mu_DL,
  beta_LB ~ mu_LB,
  beta_SEC ~ mu_SEC,
  mu_alpha ~ 1,
  mu_DL ~ 1,
  mu_LB ~ 1,
  mu_SEC ~ 1,
  coords = list(
    y       = c(5, 0),
    mu_i    = c(5, 1),
    alpha_q = c(3, 2),
    beta_DL = c(4.5, 2),
    beta_LB = c(5, 2.5),
    beta_SEC= c(5.5, 2),
    X       = c(7, 2),
    mu_alpha= c(3, 3),
    mu_DL   = c(4.5, 3),
    mu_LB   = c(5, 3.5),
    mu_SEC  = c(5.5, 3)
  )
)

# Plot
ggdag(qb_defense_dag, text = FALSE, use_labels = "name") +
  theme_dag()
