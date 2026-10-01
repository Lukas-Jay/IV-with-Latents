# 01_build_long_data.R
# Reshapes the wide simulation results into one row per replication x scenario
# x estimator x scale, following estimator_map in shared_constants.R.
#
# Input:  sim_res/results_n250.csv, sim_res/results_n2000.csv
# Output: sim_res/long_n250.rds, sim_res/long_n2000.rds with the columns
#   estimate, lower, upper  point estimate and 95% CI
#   truth                   the population value it targets (theta_I0, theta_SD
#                           or beta_Y, from the truth columns of the results)
#   bias                    estimate - truth
#   covered                 whether the CI contains the truth
source("functions/shared_constants.R")

build_long <- function(results) {
  long <- rbindlist(lapply(seq_len(nrow(estimator_map)), function(i) {
    m <- estimator_map[i]
    data.table(
      iteration_id = results$iteration_id,
      scenario     = results$simulation,
      estimator    = m$estimator,
      scale        = m$scale,
      estimate     = results[[m$estimator_col]],
      lower        = results[[m$lower_col]],
      upper        = results[[m$upper_col]],
      truth        = results[[m$truth_col]]
    )
  }))

  long[, bias      := estimate - truth]
  long[, covered   := lower <= truth & upper >= truth]
  long[, scale     := factor(scale,     levels = scale_levels)]
  long[, estimator := factor(estimator, levels = estimator_levels)]
  long[, scenario  := factor(scenario,  levels = scenario_levels)]
  long
}

long_n250  <- build_long(fread("sim_res/results_n250.csv"))
long_n2000 <- build_long(fread("sim_res/results_n2000.csv"))

saveRDS(long_n250,  "sim_res/long_n250.rds")
saveRDS(long_n2000, "sim_res/long_n2000.rds")
