# simulate_data.R
# The data-generating process: one dataset of an encouragement design
# Z -> X -> Y in which the latent treatment X is measured by K continuous items
# plus a gold anchor. The code follows the structural model of the manuscript
# and uses its symbols:
#
#   X_i  = beta_Xi Z_i + pi_X U_i + sum_j pi_Xj U_ji + eps_Xi
#   Y_i  = beta_Yi X_i + pi_Y U_i + pi_YS U_Si + sum_j pi_Yj U_ji + eps_Yi
#   I_ji = nu_j + dnu_j Z_i + (lambda_j + dlambda_j Z_i)(1 + cv E_ji) X_i
#          + pi_j U_(j)i + kappa_j U_(j)i Xt_i + eps_ji
#   I_0i = nu_0 + lambda_0 [1 + cv E_0i + kappa_0 (beta_Xi - beta_X)/sd_beta_X] X_i
#          + eps_0i
#
# with Xt = (X - mu_X) / sigma_X. U_(j) is the variable that contaminates item j:
# its own U_j, the shared U_S, one of the nuisance factors U_A / U_B, or none.
# The slopes (beta_Xi, beta_Yi) and the loadings vary across persons, so no
# effect is assumed constant. A scenario switches on only the terms of its
# violation; with the default arguments every scenario term is zero (the
# all-clean baseline).
#
# mu_X and sigma_X are the population mean and SD of X, computed from the
# structural parameters. The average loading lambda_j is converted from a
# target standardized loading with that sigma_X, so a clean item without
# loading heterogeneity has Corr(I_j, X) = lambda_std_j.
#
# The dataset is returned in two views:
#   A: the gold anchor as I0 + items 2..K (item 1 dropped) as I1..I(K-1)
#   B: the first affected item as I0 + the other K - 1 items (no gold anchor);
#      used for the designs in which the anchor itself carries the violation
# Each view comes with its population truths, computed in closed form from the
# item parameters of this dataset:
#   ATE_X        = beta_Y                     effect per unit of X
#   ATE_X_scaled = beta_Y * sigma_X           theta_SD, effect per SD of X
#   ATE_I0       = beta_Y / E[lambda_0i]      theta_I0, effect per unit of I0
#   TRUE_IV_I0   = Cov(Z, Y) / Cov(Z, I0)     [not in manuscript] the population
#                                             Wald ratio, a diagnostic
library(data.table)

simulate_data <- function(
  n = 2000,               # sample size
  K = 10,                 # ordinary items generated per dataset

  # --- f_X and f_Y ------------------------------------------------------------
  beta_X    = 1.5,        # mean first-stage slope E[beta_Xi]
  sd_beta_X = 0.5,        # SD of beta_Xi
  beta_Y    = 1.5,        # ACE, E[beta_Yi]
  sd_beta_Y = 1,          # SD of beta_Yi
  rho       = 0,          # Corr(beta_Xi, beta_Yi); nonzero violates A4(b)
  pi_X      = 2,          # U -> X
  pi_Y      = 2.5,        # U -> Y
  sd_eps_X  = 1,          # SD of eps_X
  sd_eps_Y  = 1,          # SD of eps_Y

  # --- ordinary items ---------------------------------------------------------
  lambda_std_mean = 0.70, # target standardized loading ~ TN(.70, .10) on [.20, .85]
  lambda_std_sd   = 0.10,
  lambda_std_min  = 0.20,
  lambda_std_max  = 0.85,
  sd_eps_I_min    = 0.5,  # item error SD sigma_j ~ U(.5, 1.5)
  sd_eps_I_max    = 1.5,
  cv_lambda       = 0.12, # loading heterogeneity: lambda_j (1 + cv E_ji)

  # --- gold anchor ------------------------------------------------------------
  lambda_std_0 = 0.75,    # target standardized loading, fixed; the anchor's loading
                          # varies across persons through cv_lambda (and kappa_0)
  sd_eps_0 = 1,           # anchor error SD
  nu_0     = 0,           # anchor intercept
  kappa_0  = 0,           # anchor loading moderated by the standardized compliance
                          # deviation, relative to lambda_0; nonzero violates A6(b)

  # --- affected items ---------------------------------------------------------
  affected   = c("none", "U_item", "U_S", "U_AB", "DIF"),
  n_affected = 0,         # number of affected items (the last n_affected of K)
  n_A        = 5,         # U_AB: affected items on U_A; the rest are on U_B
  pi_I       = c(0.7, 0.21),  # U_(j) -> I_j intercept: pi_j ~ N(mean, sd)
  kappa_I    = c(0.21, 0.14), # U_(j) moderates the loading: kappa_j ~ N(mean, sd)
  kappa_centred = TRUE,   # Xt = (X - mu_X)/sigma_X. FALSE (check scenario only)
                          # uses X/sigma_X, so the moderation acts on the loading
                          # alone, without an intercept part
  pi_X_item  = 0,         # total U_j -> X; pi_Xj = pi_X_item / sqrt(n_affected)
  pi_Y_item  = 0,         # total U_j -> Y; pi_Yj = pi_Y_item / sqrt(n_affected)
  pi_Y_S     = 0,         # U_S -> Y
  dif_nu         = 0,     # DIF by Z: intercept shift at Z = 1
  dif_lambda_std = 0,     # DIF by Z: standardized-loading shift at Z = 1
  dif_lambda_std_cap = 0.95, # cap of the shifted standardized loading

  view_B = FALSE          # also return view B (first affected item as I0)
) {

  affected <- match.arg(affected)
  if (affected == "none") n_affected <- 0L

  # ===========================================================================
  # 1. Item parameters (drawn once per dataset)
  # ===========================================================================
  aff_idx <- if (n_affected > 0) (K - n_affected + 1):K else integer(0)
  type <- rep("none", K)
  if (n_affected > 0) {
    type[aff_idx] <- switch(affected,
      U_item = "U_item",
      U_S    = "U_S",
      U_AB   = c(rep("U_A", n_A), rep("U_B", n_affected - n_A)),
      DIF    = "DIF")
  }
  has_U <- type %in% c("U_item", "U_S", "U_A", "U_B")

  lambda_std <- pmin(pmax(rnorm(K, lambda_std_mean, lambda_std_sd),
                          lambda_std_min), lambda_std_max)
  sd_eps_I   <- runif(K, sd_eps_I_min, sd_eps_I_max)
  nu         <- rnorm(K)
  # pi_j and kappa_j are drawn for every item and used only where U_(j) exists
  pi_j    <- ifelse(has_U, pi_I[1]    + pi_I[2]    * rnorm(K), 0)
  kappa_j <- ifelse(has_U, kappa_I[1] + kappa_I[2] * rnorm(K), 0)

  # ===========================================================================
  # 2. Population moments of X and raw loadings
  # ===========================================================================
  pi_Xj <- if (n_affected > 0 && affected == "U_item") pi_X_item / sqrt(n_affected) else 0
  pi_Yj <- if (n_affected > 0 && affected == "U_item") pi_Y_item / sqrt(n_affected) else 0

  mu_X    <- beta_X / 2
  sigma_X <- sqrt((beta_X^2 + sd_beta_X^2) / 2 - beta_X^2 / 4 +
                    pi_X^2 + sd_eps_X^2 + n_affected * pi_Xj^2)

  # standardized loading -> raw loading on X, given the item's error SD
  std2raw <- function(l_std, sd_eps) l_std * sd_eps / (sqrt(1 - l_std^2) * sigma_X)

  lambda   <- std2raw(lambda_std, sd_eps_I)
  lambda_0 <- std2raw(lambda_std_0, sd_eps_0)

  is_DIF    <- type == "DIF"
  dnu       <- ifelse(is_DIF, dif_nu, 0)
  lambda_Z1 <- std2raw(pmin(lambda_std + dif_lambda_std * is_DIF, dif_lambda_std_cap),
                       sd_eps_I)
  dlambda   <- ifelse(is_DIF, lambda_Z1 - lambda, 0)

  items <- data.table(j = 1:K, type, lambda_std, sd_eps_I, nu, lambda,
                      dnu, dlambda, pi = pi_j, kappa = kappa_j,
                      pi_Xj = ifelse(type == "U_item", pi_Xj, 0),
                      pi_Yj = ifelse(type == "U_item", pi_Yj, 0))

  # ===========================================================================
  # 3. Exogenous person variables
  # ===========================================================================
  Z <- sample(rep(c(0, 1), length.out = n))

  # (beta_Xi, beta_Yi) bivariate normal with correlation rho; c_X is the
  # standardized compliance deviation (beta_Xi - beta_X) / sd_beta_X
  c_X <- rnorm(n)
  c_Y <- rho * c_X + sqrt(1 - rho^2) * rnorm(n)
  beta_Xi <- beta_X + sd_beta_X * c_X
  beta_Yi <- beta_Y + sd_beta_Y * c_Y

  U   <- rnorm(n)
  U_S <- if (affected == "U_S")  rnorm(n) else 0
  U_A <- if (affected == "U_AB") rnorm(n) else 0
  U_B <- if (affected == "U_AB") rnorm(n) else 0
  U_item <- if (affected == "U_item") matrix(rnorm(n * K), n, K) else NULL

  # ===========================================================================
  # 4. f_X and f_Y
  # ===========================================================================
  U_item_sum <- if (!is.null(U_item)) rowSums(U_item[, aff_idx, drop = FALSE]) else 0

  X <- beta_Xi * Z + pi_X * U + pi_Xj * U_item_sum + rnorm(n, sd = sd_eps_X)
  Y <- beta_Yi * X + pi_Y * U + pi_Y_S * U_S + pi_Yj * U_item_sum +
    rnorm(n, sd = sd_eps_Y)

  # ===========================================================================
  # 5. f_j for the K items and f_0 for the gold anchor
  # ===========================================================================
  Xt <- if (kappa_centred) (X - mu_X) / sigma_X else X / sigma_X

  I <- vapply(1:K, function(j) {
    U_j <- switch(type[j], U_item = U_item[, j], U_S = U_S, U_A = U_A, U_B = U_B, 0)
    nu[j] + dnu[j] * Z +
      (lambda[j] + dlambda[j] * Z) * (1 + cv_lambda * rnorm(n)) * X +
      pi_j[j] * U_j + kappa_j[j] * U_j * Xt +
      rnorm(n, sd = sd_eps_I[j])
  }, numeric(n))

  I0_gold <- nu_0 + lambda_0 * (1 + cv_lambda * rnorm(n) + kappa_0 * c_X) * X +
    rnorm(n, sd = sd_eps_0)

  # ===========================================================================
  # 6. Views and truths
  # ===========================================================================
  # Columns I0, I1, ..., Z, Y and X_star (the latent X, used only by the
  # manifest benchmarks)
  make_view <- function(I0, others) {
    dt <- data.table(I0, I[, others, drop = FALSE])
    setnames(dt, paste0("I", 0:length(others)))
    dt[, `:=`(Z = Z, Y = Y, X_star = X)]
    dt
  }

  # Cov(Z, Y) / Var(Z) = E[beta_Xi beta_Yi]
  cov_ZY <- beta_X * beta_Y + rho * sd_beta_X * sd_beta_Y
  truth <- function(lambda_bar, cov_ZI0) list(
    ATE_X        = beta_Y,
    ATE_X_scaled = beta_Y * sigma_X,
    ATE_I0       = beta_Y / lambda_bar,   # E[beta_Yi] / E[lambda_0i]
    TRUE_IV_I0   = cov_ZY / cov_ZI0       # Cov(Z, Y) / Cov(Z, I0)
  )

  # View A: gold anchor + items 2..K
  view_A  <- make_view(I0_gold, 2:K)
  truth_A <- truth(lambda_0, lambda_0 * beta_X + lambda_0 * kappa_0 * sd_beta_X)

  # View B: the first affected item p as I0; under DIF its average loading is
  # the mean over the two arms
  view_B_dt <- truth_B <- NULL
  if (view_B) {
    p <- aff_idx[1]
    view_B_dt <- make_view(I[, p], setdiff(1:K, p))
    truth_B   <- truth(lambda[p] + dlambda[p] / 2,
                       (lambda[p] + dlambda[p]) * beta_X + dnu[p])
  }

  # items, lambda_0, mu_X, sigma_X: for inspecting a dataset; the simulation
  # uses only the views and their truths
  list(A = view_A, B = view_B_dt, truth_A = truth_A, truth_B = truth_B,
       items = items, lambda_std_0 = lambda_std_0, lambda_0 = lambda_0,
       mu_X = mu_X, sigma_X = sigma_X)
}
