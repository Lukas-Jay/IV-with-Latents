# anchor_wald.R
# Anchor-Wald: the Wald ratio Cov(Z, Y) / Cov(Z, I0), estimated as 2SLS of Y on
# the anchor item I0 with Z as instrument. It uses no other item and no
# measurement model, and estimates theta_I0 only.
#
# Input:  one dataset view (columns I0, Z, Y)
# Output: one row with the estimate and its 95% CI
library(ivreg)
library(sandwich)
library(data.table)

anchor_wald <- function(data) {
  fit <- ivreg(Y ~ I0 | Z, data = data)
  # HC3 (heteroskedasticity-robust) SEs, since the random slopes and loadings
  # make the residual variance differ between persons
  ci  <- confint(fit, vcov. = vcovHC(fit, type = "HC3"))[2, ]

  data.table(anchorWald         = coef(fit)[[2]],
             anchorWald_LowerCI = ci[[1]],
             anchorWald_UpperCI = ci[[2]])
}
