# run_all.R
# Regenerates every simulation table and figure of the manuscript from the
# cached results in sim_res/. Run from the project root (open the .Rproj).

# Stage 0, the simulation itself, takes several hours; its results are cached
# in sim_res/. Set to TRUE to rerun it.
RUN_SIMULATION <- FALSE
if (RUN_SIMULATION) source("00_simulate.R")

source("01_build_long_data.R")   # sim_res/results_*.csv -> sim_res/long_*.rds
source("02_tables.R")            # -> tables/*.csv, appendix table bodies (.tex)
source("03_figures.R")           # -> the manuscript figures (.pdf)
