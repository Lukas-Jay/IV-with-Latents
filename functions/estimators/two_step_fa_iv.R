# two_step_fa_iv.R
# Two-step FA-IV: (1) fit a one-factor CFA to all items and compute factor
# scores, (2) use the scores as the treatment in 2SLS with Z as instrument.
#
# fit_factor_scores() is step 1. Its scores are also used by
# other_estimators.R, so each CFA is fitted only once per dataset.
# two_step_fa_iv() is step 2 with Bartlett scores.
library(lavaan)
library(ivreg)
library(sandwich)
library(data.table)

# --- Step 1: CFA and factor scores --------------------------------------------
# metric = "I0": anchor loading fixed to 1, so the scores are on the anchor
#                metric (theta_I0)
# metric = "SD": factor variance fixed to 1, so the scores are in SD units of X
#                (theta_SD)
# Returns Bartlett and regression scores, or NULL when the CFA does not converge
# or returns a negative variance estimate; the replication then counts as a
# failure of the two-step estimator.
fit_factor_scores <- function(data, metric) {
  items <- grep("^I", names(data), value = TRUE)   # I0 comes first

  if (metric == "I0") {
    model  <- paste0("X =~ 1*", paste(items, collapse = " + "))
    std_lv <- FALSE
  } else {
    model  <- paste0("X =~ ", paste(items, collapse = " + "))
    std_lv <- TRUE
  }

  # We use ML, because only the scores are used, not the SE
  # bad.marker.crit = 0 keeps I0 as the marker even 
  # when lavaan judges it a poor item
  fit <- cfa(model, data = data, std.lv = std_lv, test = "none",
             bad.marker.crit = 0)
  if (!lavInspect(fit, "converged")) return(NULL)

  pe <- parameterEstimates(fit)
  if (any(pe$est[pe$op == "~~" & pe$lhs == pe$rhs] < 0)) return(NULL)

  # Bartlett scores are conditionally unbiased given the factor. Regression
  # scores are not
  list(Bartlett   = as.vector(lavPredict(fit, method = "Bartlett")),
       regression = as.vector(lavPredict(fit, method = "regression")))
}

# --- Helpers for step 2 -------------------------------------------------------
no_estimate <- c(est = NA_real_, lower = NA_real_, upper = NA_real_)

# Slope of the regressor in an ivreg or lm fit, with its 95% CI from HC3
# (heteroskedasticity-robust) SEs and t quantiles, as in anchor_wald()
slope_ci <- function(model) {
  est  <- coef(model)[[2]]
  half <- qt(0.975, df.residual(model)) * sqrt(vcovHC(model, type = "HC3")[2, 2])
  c(est = est, lower = est - half, upper = est + half)
}

# Y, the scores and Z in one table; a person with an infinite score is dropped
score_data <- function(data, scores) {
  data.table(y = data$Y, x = scores, z = data$Z)[!is.infinite(x)]
}

# 2SLS of Y on the scores with Z as instrument; NA when the CFA failed
score_iv <- function(data, scores) {
  if (is.null(scores)) return(no_estimate)
  slope_ci(ivreg(y ~ x | z, data = score_data(data, scores)))
}

# --- Step 2: 2SLS on the Bartlett scores --------------------------------------
# scores_I0, scores_SD: output of fit_factor_scores() for the two metrics
two_step_fa_iv <- function(data, scores_I0, scores_SD) {
  est_I0 <- score_iv(data, scores_I0$Bartlett)
  est_SD <- score_iv(data, scores_SD$Bartlett)

  data.table(
    TwoStep                = est_I0[["est"]],
    TwoStep_LowerCI        = est_I0[["lower"]],
    TwoStep_UpperCI        = est_I0[["upper"]],
    TwoStep_scaled         = est_SD[["est"]],
    TwoStep_scaled_LowerCI = est_SD[["lower"]],
    TwoStep_scaled_UpperCI = est_SD[["upper"]])
}
