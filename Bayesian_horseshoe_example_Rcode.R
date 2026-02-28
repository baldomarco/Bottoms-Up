BAYESIAN FUNCTION CALCULATION


# Packages
library(caret)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ----------------------------
# Example input data structure
# ----------------------------
# df must contain:
#   y  = species richness (or another response)
#   candidate predictors = many stand variables (Appendix X pool)
#
# Here we assume df already exists in your environment.

# ----------------------------
# (i) Collinearity screening
# ----------------------------
# Select only numeric candidate predictors (exclude y)
cand_names <- setdiff(names(df), "y")
X_cand <- df[, cand_names]
X_cand <- X_cand[, sapply(X_cand, is.numeric), drop = FALSE]

# Correlation matrix (pairwise complete obs is common in ecological data)
cmat <- cor(X_cand, use = "pairwise.complete.obs", method = "pearson")

# Keep variables with |r| < r_threshold (caret removes variables to satisfy threshold)
r_threshold <- 0.70  # <-- replace with your r < 0.x rule
to_drop <- findCorrelation(cmat, cutoff = r_threshold, names = TRUE, exact = TRUE)
X_keep <- X_cand[, setdiff(colnames(X_cand), to_drop), drop = FALSE]

# ----------------------------
# (ii) Force-include your final predictor set (example)
# ----------------------------
# Replace these with your final chosen predictors if they are present in X_keep:
final_vars <- c(
  "age_oldest20",     # age of oldest 20% of trees
  "lai",              # leaf area index
  "deadwood_c",       # carbon stock in deadwood
  "n_dbh_10_40",      # number of trees 10-40 cm
  "n_dbh_gt40",       # number of large trees >40 cm
  "n_broad_gt40"      # number of large broadleaved trees >40 cm
)

missing <- setdiff(final_vars, colnames(X_keep))
if (length(missing) > 0) {
  stop("These required predictors are missing after screening: ", paste(missing, collapse = ", "))
}

X <- X_keep[, final_vars, drop = FALSE]

# ----------------------------
# (iii) Add quadratic terms for all predictors (linear + quadratic)
# ----------------------------
X2 <- X^2
colnames(X2) <- paste0(colnames(X), "_sq")
X_design <- cbind(X, X2)

# Standardize predictors (important for horseshoe behavior)
X_scaled <- scale(X_design)
y <- df$y

# ----------------------------
# (iv) Expert weights -> prior scale multipliers
# ----------------------------
# Suppose experts give probabilities in [0,1] per *original* predictor (not per quadratic term).
# Example weights: replace with your expert table values.
w_linear <- c(
  age_oldest20 = 0.80,
  lai         = 0.90,
  deadwood_c  = 0.85,
  n_dbh_10_40 = 0.40,
  n_dbh_gt40  = 0.75,
  n_broad_gt40= 0.70
)

# Extend weights to quadratic terms (common choices: same weight, or slightly smaller)
w_all <- c(w_linear, setNames(w_linear * 0.8, paste0(names(w_linear), "_sq")))

# Map weights -> positive multipliers s_j for prior scales.
# One reasonable mapping: s = sqrt(w / (1-w)), capped to avoid explosions near 1.
eps <- 1e-3
w_clipped <- pmin(pmax(w_all, eps), 1 - eps)
s <- sqrt(w_clipped / (1 - w_clipped))
s <- pmin(s, 10)  # cap (engineering choice; adjust as needed)

stopifnot(length(s) == ncol(X_scaled))

# ----------------------------
# (v) Stan data list
# ----------------------------
stan_data <- list(
  N = length(y),
  P = ncol(X_scaled),
  X = unclass(X_scaled),
  y = as.vector(y),
  s = as.vector(s),
  
  # Prior hyperparameters for regularized horseshoe:
  # p0 = prior guess for "effective number of non-zero coefficients"
  p0 = 4,          # <-- set based on expected sparsity (not necessarily = number of predictors)
  slab_scale = 2.0,
  slab_df = 4.0
)

# ----------------------------
# (vi) Compile and sample
# ----------------------------
stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> P;
  matrix[N, P] X;
  vector[N] y;

  vector<lower=0>[P] s;      // expert scale multipliers

  real<lower=0> p0;          // prior guess: effective number of nonzero coeffs
  real<lower=0> slab_scale;  // slab scale for regularized horseshoe
  real<lower=0> slab_df;     // slab df
}
parameters {
  real alpha;
  real<lower=0> sigma;

  vector[P] z;                    // standardized coefficients
  vector<lower=0>[P] lambda;      // local shrinkage
  real<lower=0> tau;              // global shrinkage

  real<lower=0> c2_raw;           // slab parameter (squared scale)
}
transformed parameters {
  real<lower=0> c2 = square(slab_scale) * c2_raw;

  vector[P] lambda_tilde;
  vector[P] beta;

  // Regularized horseshoe: lambda_tilde = sqrt( c2 * lambda^2 / (c2 + tau^2 * lambda^2) )
  for (j in 1:P) {
    real lam2 = square(lambda[j]);
    lambda_tilde[j] = sqrt( c2 * lam2 / (c2 + square(tau) * lam2) );
  }

  // beta = z .* (tau * lambda_tilde .* s)
  beta = z .* (tau * lambda_tilde .* s);
}
model {
  // Likelihood
  y ~ normal(alpha + X * beta, sigma);

  // Weakly informative intercept and noise
  alpha ~ normal(0, 2);
  sigma ~ normal(0, 1);

  // Horseshoe priors
  z ~ normal(0, 1);
  lambda ~ cauchy(0, 1);

  // Piironen & Vehtari-style scaling for tau based on p0, N, and sigma:
  // tau0 = (p0 / (P - p0)) * (sigma / sqrt(N))
  {
    real tau0 = (p0 / (P - p0)) * (sigma / sqrt(N));
    tau ~ cauchy(0, tau0);
  }

  // Slab regularization
  c2_raw ~ inv_gamma(0.5 * slab_df, 0.5 * slab_df);
}
generated quantities {
  vector[N] y_hat = alpha + X * beta;
}
"

sm <- stan_model(model_code = stan_code)

fit <- sampling(
  sm, data = stan_data,
  iter = 2000, warmup = 1000, chains = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

print(fit, pars = c("alpha", "sigma"))
# Coefficients (linear + quadratic)
post <- rstan::extract(fit)
beta_mean <- colMeans(post$beta)
names(beta_mean) <- colnames(X_scaled)
beta_mean