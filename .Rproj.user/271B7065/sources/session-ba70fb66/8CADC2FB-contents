dat <- read.csv('plays_with_positions.csv')

library(brms)
dat$is_pass <- ifelse(dat$play_type == 'pass', 1, 0)
dat$play_type_id <- as.numeric(as.factor(dat$play_type))


model_bayes <- brm(
  formula = epa ~ DL_involved*is_pass + LB_involved*is_pass + DB_involved*is_pass,
  data = dat,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),   # priors on regression coefficients
    prior(normal(0, 1), class = "Intercept"),
    prior(exponential(1), class = "sigma") # prior on residual variance
  ),
  chains = 4, cores = 4, iter = 2000, seed = 123
)


summary(model_bayes)
plot(model_bayes)   # posterior distributions
posterior_samples(model_bayes) |>  head()

library(tidyverse)
library(rstan)

# --- 1. Read in your data ---
df <- read_csv("plays_with_positions.csv")

# --- 2. Make sure columns are in the right form ---
df <- df %>%
  mutate(
    DL_involved = as.numeric(DL_involved),
    LB_involved = as.numeric(LB_involved),
    DB_involved = as.numeric(DB_involved),
    is_pass = if_else(play_type == "pass", 1, 0)
  )

# --- 3. Encode offense/defense teams as integer IDs ---
offense_levels <- df %>% distinct(posteam) %>% arrange(posteam) %>% pull(posteam)
defense_levels <- df %>% distinct(defteam) %>% arrange(defteam) %>% pull(defteam)

df <- df %>%
  mutate(
    offense_id = as.integer(factor(posteam, levels = offense_levels)),
    defense_id = as.integer(factor(defteam, levels = defense_levels))
  )

# --- 4. Create Stan data list ---
stan_data <- list(
  N = nrow(df),
  D = length(unique(df$defense_id)),
  O = length(unique(df$offense_id)),
  defense = df$defense_id,
  offense = df$offense_id,
  DL_involved = df$DL_involved,
  LB_involved = df$LB_involved,
  DB_involved = df$DB_involved,
  is_pass = df$is_pass,
  y = df$epa
)

# --- 5. Fit Stan model ---
# Save the Stan model code into "defense_offense_hierarchical.stan"
fit <- stan(
  file = "stan_model.stan",
  data = stan_data,
  iter = 4000,      # 1000 warmup + 1000 sampling
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 123
)

# --- 6. Summarize output ---
print(fit, pars = c("alpha", "beta_DL", "beta_LB", "beta_DB", "beta_pass",
                    "sigma", "sigma_def_DL", "sigma_def_LB", "sigma_def_DB", "sigma_off"),
      probs = c(0.025, 0.5, 0.975))

saveRDS(fit, file = "bayes_first_attempt.rds")
