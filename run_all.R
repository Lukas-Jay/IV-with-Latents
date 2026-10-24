# run_all.R
# Regenerates every simulation table and figure of the manuscript from the
# cached results in sim_res/. Run from the folder that contains IV with Latents/.
setwd("IV with Latents")


# Stage 0, the simulation itself, takes several hours; its results are cached
# in sim_res/. Set to TRUE to rerun it.
source("functions/run_simulation.R")

if (FALSE) {
  
N_ITER    <- 10000 # replications per sample size
N_WORKERS <- 10    # parallel R sessions

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

}

# Then generate Latex Results and Figures
source("01_build_long_data.R")   # sim_res/results_*.csv -> sim_res/long_*.rds
source("02_tables.R")            # -> tables/*.csv, appendix table bodies (.tex)
source("03_figures.R")           # -> the manuscript figures (.pdf)
