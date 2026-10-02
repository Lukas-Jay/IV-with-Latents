# sem_iv.R
# SEM-IV: the measurement model and the IV equations Z -> X -> Y fitted jointly
# in one lavaan model.
#
# Input:  one dataset view (columns I0, I1, ..., Z, Y); I0 is the anchor and
#         comes first.
# Output: one row with the estimate and 95% CI on both scales, theta_I0 (per
#         unit of the anchor metric) and theta_SD (per SD of X). All NA when
#         the model does not converge.
library(lavaan)
library(data.table)

sem_iv <- function(data) {
  items <- grep("^I", names(data), value = TRUE)

  # 1*I0 fixes the anchor loading to 1, which puts X on the anchor metric.
  # X ~~ Y frees the residual covariance of X and Y and absorbs the confounding
  # X <- U -> Y; without it this is a confounded latent regression.
  model <- paste0(
    "X =~ 1*", paste(items, collapse = " + "), "
     X ~ a*Z     # first stage
     Y ~ b*X     # second stage
     X ~~ Y      # endogeneity")

  # MLR: ML point estimates with robust (Huber-White) SEs, since the random
  # slopes and loadings make the data non-normal. bad.marker.crit = 0 keeps I0
  # as the marker even when lavaan judges it a poor item.
  fit <- sem(model, data = data, estimator = "MLR", 
              bad.marker.crit = 0) # only needed for Simulation coherence

  out <- data.table(sem = NA_real_, sem_LowerCI = NA_real_, sem_UpperCI = NA_real_,
                    sem_scaled = NA_real_, sem_scaled_LowerCI = NA_real_,
                    sem_scaled_UpperCI = NA_real_)
  if (!lavInspect(fit, "converged")) return(out)

  # theta_I0: the structural slope b
  pe <- parameterEstimates(fit, ci = TRUE, level = 0.95)
  b  <- pe[pe$label == "b", ]
  out[, `:=`(sem = b$est, sem_LowerCI = b$ci.lower, sem_UpperCI = b$ci.upper)]

  # theta_SD: the std.lv column of the standardized solution, b times the
  # model-implied SD of X, with a delta-method CI that accounts for the
  # estimated SD. tryCatch: a rare error here would otherwise stop the whole
  # parallel run; the SD-scale estimate then stays NA.
  ss <- tryCatch(standardizedSolution(fit, type = "std.lv", level = 0.95),
                 error = function(e) NULL)
  if (!is.null(ss)) {
    b_std <- ss[ss$lhs == "Y" & ss$op == "~" & ss$rhs == "X", ]
    out[, `:=`(sem_scaled = b_std$est.std, sem_scaled_LowerCI = b_std$ci.lower,
               sem_scaled_UpperCI = b_std$ci.upper)]
  }
  out
}
