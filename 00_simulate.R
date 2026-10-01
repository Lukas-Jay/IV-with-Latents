# 00_simulate.R
# Stage 0: the Monte Carlo study at n = 250 and n = 2000. Takes several hours.
# Run from the project root (open the .Rproj).
#
# Output: sim_res/results_n250.csv, sim_res/results_n2000.csv (one row per
#         replication x scenario view) and sim_res/session_info.txt
source("functions/run_simulation.R")

N_ITER    <- 2000   # replications per sample size; the manuscript reports 10,000
N_WORKERS <- 6      # parallel R sessions

# progress bar in the console
handlers(global = TRUE)
handlers("progress")

# --- n = 250 ---------------------------------------------------------------------
set.seed(123)
results_n250 <- run_simulation_study(N_iter = N_ITER, n_each = 250,
                                     n_workers = N_WORKERS)
fwrite(results_n250, "sim_res/results_n250.csv")

# --- n = 2000 --------------------------------------------------------------------
set.seed(123)
results_n2000 <- run_simulation_study(N_iter = N_ITER, n_each = 2000,
                                      n_workers = N_WORKERS)
fwrite(results_n2000, "sim_res/results_n2000.csv")

# R and package versions used for these results
writeLines(capture.output(sessionInfo()), "sim_res/session_info.txt")
