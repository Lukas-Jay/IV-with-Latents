# other_estimators.R   [not in manuscript]
# Estimates computed in every replication but reported nowhere in the
# manuscript. They serve as benchmarks and checks; sim_supplement.qmd uses
# some of them.
#
#   manifest IV, OLS   2SLS and OLS of Y on the true latent X (X_star), raw and
#                      per SD of X_star; OLS is confounded by U
#   ITT                difference in mean Y between the arms (Welch t-test CI),
#                      the reduced form, not an effect of X
#   OLS on scores      OLS of Y on regression factor scores; confounded by U
#   two-step, regression scores
#                      two_step_fa_iv() with regression instead of Bartlett
#                      scores
#   z_x_hat_cor        Corr(Z, Bartlett score), SD metric
#   z_x_star_cor       Corr(Z, X_star)
#
# Needs fit_factor_scores(), slope_ci(), score_data() and score_iv() from
# two_step_fa_iv.R.
library(ivreg)
library(data.table)

# OLS of Y on the scores; NA when the CFA failed
score_ols <- function(data, scores) {
  if (is.null(scores)) return(no_estimate)
  slope_ci(lm(y ~ x, data = score_data(data, scores)))
}

other_estimates <- function(data, scores_I0, scores_SD) {
  sd_X <- sd(data$X_star)

  manifest_iv  <- slope_ci(ivreg(Y ~ X_star | Z, data = data))
  manifest_ols <- slope_ci(lm(Y ~ X_star, data = data))

  itt <- t.test(data$Y[data$Z == 1], data$Y[data$Z == 0])

  ols_I0 <- score_ols(data, scores_I0$regression)
  ols_SD <- score_ols(data, scores_SD$regression)
  reg_I0 <- score_iv(data, scores_I0$regression)
  reg_SD <- score_iv(data, scores_SD$regression)

  z_x_hat_cor <- if (is.null(scores_SD)) NA_real_ else
    score_data(data, scores_SD$Bartlett)[, cor(x, z)]

  data.table(
    manifestIV                 = manifest_iv[["est"]],
    manifestIV_LowerCI         = manifest_iv[["lower"]],
    manifestIV_UpperCI         = manifest_iv[["upper"]],
    manifestIV_scaled          = manifest_iv[["est"]]   * sd_X,
    manifestIV_scaled_LowerCI  = manifest_iv[["lower"]] * sd_X,
    manifestIV_scaled_UpperCI  = manifest_iv[["upper"]] * sd_X,

    manifestOLS                = manifest_ols[["est"]],
    manifestOLS_LowerCI        = manifest_ols[["lower"]],
    manifestOLS_UpperCI        = manifest_ols[["upper"]],
    manifestOLS_scaled         = manifest_ols[["est"]]   * sd_X,
    manifestOLS_scaled_LowerCI = manifest_ols[["lower"]] * sd_X,
    manifestOLS_scaled_UpperCI = manifest_ols[["upper"]] * sd_X,

    ITT                        = itt$estimate[[1]] - itt$estimate[[2]],
    ITT_LowerCI                = itt$conf.int[1],
    ITT_UpperCI                = itt$conf.int[2],

    ols_FS_reg_Beta            = ols_I0[["est"]],
    ols_FS_reg_lowerCI         = ols_I0[["lower"]],
    ols_FS_reg_UpperCI         = ols_I0[["upper"]],
    ols_FS_reg_Beta_scaled     = ols_SD[["est"]],
    ols_FS_reg_scaled_LowerCI  = ols_SD[["lower"]],
    ols_FS_reg_scaled_UpperCI  = ols_SD[["upper"]],

    TwoStep_reg                = reg_I0[["est"]],
    TwoStep_reg_LowerCI        = reg_I0[["lower"]],
    TwoStep_reg_UpperCI        = reg_I0[["upper"]],
    TwoStep_reg_scaled         = reg_SD[["est"]],
    TwoStep_reg_scaled_LowerCI = reg_SD[["lower"]],
    TwoStep_reg_scaled_UpperCI = reg_SD[["upper"]],

    z_x_hat_cor                = z_x_hat_cor,
    z_x_star_cor               = cor(data$X_star, data$Z))
}
