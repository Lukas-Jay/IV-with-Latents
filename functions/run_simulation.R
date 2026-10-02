# run_simulation.R
# The Monte Carlo study: scenarios, estimators and the parallel replication loop.
# run_simulation_study() returns one row per replication x scenario view, with
# the population truths and every estimate with its 95% CI.
#
# Every scenario generates one dataset per replication. View A (clean anchor,
# clean) is stored under the scenario name. For scenarios with `twin = TRUE`,
# view B of the same dataset (one affected item becomes the anchor I0)
# is estimated as well and stored as <name>_itemAnchor.
source("functions/simulate_data.R")
source("functions/estimators/sem_iv.R")
source("functions/estimators/two_step_fa_iv.R")
source("functions/estimators/anchor_wald.R")
source("functions/estimators/other_estimators.R")

library(future.apply)
library(data.table)
library(progressr)

run_simulation_study <- function(N_iter, n_each,
                                 n_workers = availableCores() - 1,
                                 include_check = FALSE) {  # [not in manuscript] check scenario

  # --- 1. Parallel plan -------------------------------------------------------
  old_plan <- plan()
  plan(multisession, workers = n_workers)
  on.exit(plan(old_plan), add = TRUE)

  my_packages <- c("data.table", "ivreg", "lavaan")

  # --- 2. Parameters ------------------------------------------------------------
  # Global parameters, the same in every scenario (manuscript: table of
  # population parameters). Item parameters are drawn per dataset inside
  # simulate_data().
  base_args <- list(
    n = n_each,
    K = 10,              # 10 ordinary items + the clean anchor

    beta_X    = 1.5,     # beta_X = E[beta_Xi]; manipulation-check d ~ .66
    sd_beta_X = 0.5,     # sigma_beta_X, SD of beta_Xi
    beta_Y    = 1.5,     # beta_Y = E[beta_Yi], the ACE
    sd_beta_Y = 1,       # sigma_beta_Y, SD of beta_Yi
    rho       = 0,       # Corr(beta_Xi, beta_Yi) = 0: A4(b) holds
    pi_X      = 2,       # U -> X
    pi_Y      = 2.5,     # U -> Y
    sd_eps_X  = 1,
    sd_eps_Y  = 1,       # Var(X) = 5.69 (9.69 in orthogonal_IUX)

    lambda_std_mean = 0.70, lambda_std_sd = 0.10,   # items: standardized loadings
    lambda_std_min  = 0.20, lambda_std_max = 0.85,  #   TN(.70, .10) on [.20, .85]
    sd_eps_I_min = 0.5, sd_eps_I_max = 1.5,         # item error SDs U(.5, 1.5)
    cv_lambda = 0.12,    # CV, loading heterogeneity, independent of compliance

    lambda_std_0 = 0.75, # clean anchor
    sd_eps_0 = 1, nu_0 = 0,

    pi_I    = c(0.7, 0.21),  # pi_j ~ N(.7, .21) and kappa_j ~ N(.21, .14): the
    kappa_I = c(0.21, 0.14)  # same contamination for every U -> I scenario
  )

  # --- 3. Scenarios -----------------------------------------------------------
  # Each entry lists only the terms it switches on; everything else stays at
  # base_args. "Item term" = pi_j U_(j)i + kappa_j U_(j)i Xt_i added to I_ji,
  # with Xt = (X - mu_X) / sigma_X. Every measurement violation affects all 10
  # ordinary items.
  scenarios <- list(
    # Baseline: all scenario terms zero.
    all_clean = list(args = list(), twin = FALSE),

    # Own U_ji for every item: item term with U_(j) = U_ji, and
    # Y += sum_j pi_Yj U_ji with pi_Yj = 2 / sqrt(10).
    # Violates nothing: U_j acts on Y and one item, not on X.
    orthogonal_IUY = list(
      args = list(affected = "U_item", n_affected = 10, pi_Y_item = 2),
      twin = TRUE),

    # Corr(beta_Xi, beta_Yi) = .4, so the IV estimand becomes
    # E[beta_Xi beta_Yi] / beta_X = 1.5 + .4 * .5 * 1 / 1.5 = 1.63. Violates A4(b).
    nosh_violated = list(
      args = list(rho = 0.4),
      twin = FALSE),

    # Own U_ji for every item: item term with U_(j) = U_ji, and
    # X += sum_j pi_Xj U_ji with pi_Xj = 2 / sqrt(10). Var(X) rises from 5.69
    # to 9.69, so sigma_X and both targets change. Violates A8.
    orthogonal_IUX = list(
      args = list(affected = "U_item", n_affected = 10, pi_X_item = 2),
      twin = TRUE),

    # I_ji = nu_j + dnu_j Z + (lambda_j + dlambda_j Z)(1 + CV E_ji) X + eps_ji
    # with dnu_j = .175 and dlambda_j raising the standardized loading by .03
    # at Z = 1. Violates A5 (and A7, A8: Z becomes a common cause of X and
    # every item).
    DIF_in_z = list(
      args = list(affected = "DIF", n_affected = 10,
                  dif_nu = 0.175, dif_lambda_std = 0.03),
      twin = TRUE),

    # One U_Si shared by all items: item term with U_(j) = U_S, and
    # Y += 2 U_S (the same total U -> Y as orthogonal_IUY). Violates A7.
    one_confounder = list(
      args = list(affected = "U_S", n_affected = 10, pi_Y_S = 2),
      twin = TRUE),

    # Item term with U_(j) = U_A for items 1-3 and U_B for items 4-10; neither
    # acts on X or Y. Violates A7. The split is unequal so that the twin's
    # anchor (item 1) is not contaminated like the average item; with a 5/5
    # split the distortion of the one-factor loadings cancels on the I0 metric.
    two_more_factors = list(
      args = list(affected = "U_AB", n_affected = 10, n_A = 3),
      twin = TRUE),

    # Item loading lambda_ji = lambda_j (1 + CV E_ji + kappa_Cj c_Xi) with
    # kappa_Cj ~ N(-.3, .1) and c_Xi = (beta_Xi - beta_X) / sd_beta_X, so
    # Cov(lambda_ji, beta_Xi) = kappa_Cj lambda_j sd_beta_X. Violates A6(b) at
    # every item (and A8, unavoidably: the loading tracks compliance, which is
    # part of X in the treated arm).
    item_nosh_violated = list(
      args = list(affected = "NOSH", n_affected = 10, kappa_C = c(-0.3, 0.1)),
      twin = TRUE)
  )

  # ---- Not reported in the manuscript: check scenario ------------------------
  # A8 through the loading channel alone. U_j -> X, and U_j moderates the
  # loading on raw X without shifting the intercept (pi_j = 0), so
  # Cov(X, delta_j) = kappa_j pi_Xj mu_X / sigma_X. It checks that A8 can be
  # broken by the loading alone; the resulting bias is small.
  if (include_check)
    scenarios$orthogonal_IUX_loading <- list(
      args = list(affected = "U_item", n_affected = 10, pi_X_item = 2,
                  pi_I = c(0, 0), kappa_centred = FALSE),
      twin = TRUE)

  # --- 4. One dataset view -> one output row ----------------------------------
  # Both CFAs are fitted once and shared by the two-step estimator and the
  # factor-score benchmarks.
  estimate_view <- function(scenario_name, dat, tr) {
    scores_I0 <- fit_factor_scores(dat, metric = "I0")
    scores_SD <- fit_factor_scores(dat, metric = "SD")

    cbind(
      data.table(simulation   = scenario_name,
                 ATE_X        = tr$ATE_X,
                 ATE_X_scaled = tr$ATE_X_scaled,
                 ATE_I0       = tr$ATE_I0,
                 TRUE_IV_I0   = tr$TRUE_IV_I0),
      # the three estimators of the manuscript
      sem_iv(dat),
      two_step_fa_iv(dat, scores_I0, scores_SD),
      anchor_wald(dat),
      # [not in manuscript]
      other_estimates(dat, scores_I0, scores_SD))
  }

  # One scenario: one dataset, estimated on view A and (twin) on view B.
  # The data are simulated before any model is fitted, because lavaan restores
  # .Random.seed on exit.
  run_scenario <- function(scenario_name, config) {
    sim_args <- modifyList(base_args, config$args)
    sim_args$view_B <- config$twin
    sim <- do.call(simulate_data, sim_args)

    out <- list(estimate_view(scenario_name, sim$A, sim$truth_A))
    if (config$twin)
      out[[2]] <- estimate_view(paste0(scenario_name, "_itemAnchor"),
                                sim$B, sim$truth_B)
    rbindlist(out)
  }

  my_globals <- c("simulate_data", "sem_iv", "fit_factor_scores",
                  "two_step_fa_iv", "anchor_wald", "other_estimates",
                  "no_estimate", "slope_ci", "score_data", "score_iv",
                   "score_ols",
                  "base_args", "scenarios", "estimate_view", "run_scenario")

  # --- 5. Replications in parallel, with a progress bar -----------------------
  result <- with_progress({
    p <- progressor(steps = N_iter)

    run_single_iteration <- function(i) {
      p(sprintf("Iteration %d", i))
      rbindlist(lapply(names(scenarios), function(name)
        run_scenario(name, scenarios[[name]])))
    }

    results_list <- future_lapply(
      1:N_iter,
      run_single_iteration,
      future.seed = TRUE,
      future.globals = my_globals,
      future.packages = my_packages
    )

    rbindlist(results_list, idcol = "iteration_id")
  })

  result
}
