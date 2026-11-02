# IV with Latents

Simulation code for the paper **"Identifying Causal Effects with Latent Treatments: Psychological Experiments as Instrumental Variable Designs"**.

The paper treats a psychological experiment as a randomized encouragement design. The manipulation `Z` is an instrument for a latent treatment `X`, which is measured by survey items, and `X` acts on an outcome `Y`. The simulation compares three estimators of the effect of `X` on `Y` across scenarios that each violate one assumption:

| Estimator | File | Targets |
|---|---|---|
| SEM-IV | `functions/estimators/sem_iv.R` | θ_I0 and θ_SD |
| Two-step FA-IV | `functions/estimators/two_step_fa_iv.R` | θ_I0 and θ_SD |
| Anchor-Wald | `functions/estimators/anchor_wald.R` | θ_I0 |

θ_I0 is the effect per unit of the anchor item `I0`, and θ_SD the effect per standard deviation of `X`.

## Files

```
run_all.R                 stage 0 (switched off) + stages 1-3: all tables and figures
01_build_long_data.R      stage 1: wide results -> long format
02_tables.R               stage 2: summaries and the appendix tables
03_figures.R              stage 3: the manuscript figures
functions/
  simulate_data.R         the data-generating process (one dataset)
  run_simulation.R        scenarios, estimators and the parallel replication loop
  estimators/             one file per manuscript estimator, plus other_estimators.R
  shared_constants.R      estimator map, scenario labels, plot theme
  dag_glyphs.R            the DAG glyphs drawn under the scenario axis
sim_res/                  simulation results (the result CSVs are not in the repository)
tables/                   summary tables (CSV)
```

## Running the code

Set the working directory to `IV with Latents/` and run

```r
source("run_all.R")
```

This rebuilds every table and figure from `sim_res/results_n250.csv` and `sim_res/results_n2000.csv`, which takes a few minutes. The tables and figures are written to the folder set by `paper_dir` in `functions/shared_constants.R`. Change it if the manuscript folder is not next to this one.

To rerun the simulation itself, change `if (FALSE)` to `if (TRUE)` in `run_all.R`. With 10,000 replications per sample size, this takes many hours. `N_ITER` and `N_WORKERS` are set at the top of that block. The results do not depend on the number of workers: `future.seed = TRUE` gives every replication its own random-number stream.

Requirements: R 4.6.1 with data.table 1.18.4, lavaan 0.7.2, ivreg 0.6.8, sandwich 3.1.2, future.apply 1.20.2, progressr 1.0.0, ggplot2 4.0.3, patchwork 1.3.2, ggh4x 0.3.1 and scales 1.4.0. The simulation block writes the full session information to `sim_res/session_info.txt`.

## Quick start: one dataset

The default arguments of `simulate_data()` are the baseline values of the study.

```r
source("functions/simulate_data.R")
source("functions/estimators/sem_iv.R")
source("functions/estimators/two_step_fa_iv.R")
source("functions/estimators/anchor_wald.R")

set.seed(1)
sim <- simulate_data(n = 2000)          # baseline: all assumptions hold
dat <- sim$A                            # clean anchor I0 + 9 items, Z, Y
unlist(sim$truth_A)                     # ATE_I0 = theta_I0, ATE_X_scaled = theta_SD

scores_I0 <- fit_factor_scores(dat, metric = "I0")
scores_SD <- fit_factor_scores(dat, metric = "SD")
cbind(sem_iv(dat), anchor_wald(dat), two_step_fa_iv(dat, scores_I0, scores_SD))
```

To generate a scenario, pass its arguments from `functions/run_simulation.R`. For example, `simulate_data(n = 2000, affected = "U_S", n_affected = 10, pi_Y_S = 2)` adds a confounder shared by all items and `Y`.

## Scenarios

Each scenario switches on the terms of one violation and leaves everything else at the baseline. The definitions, with the terms they switch on, are in `functions/run_simulation.R`.

| Code name | Name in the paper | Violates |
|---|---|---|
| `all_clean` | All clean | – |
| `orthogonal_IUY` | Indep. U_j → I_j, Y | – |
| `nosh_violated` | NOSH violated | A4(b) no simultaneous heterogeneity |
| `orthogonal_IUX` | Indep. U_j → I_j, X | A8 no X–I confounding |
| `DIF_in_z` | DIF by Z | A5 measurement exclusion |
| `one_confounder` | Shared U → I, Y | A7 local independence |
| `two_more_factors` | Two more factors | A7 local independence |
| `item_nosh_violated` | Item-NOSH violated | A6(b) item NOSH |

Every dataset has a clean anchor (view A). Six scenarios also have a contaminated-anchor twin, `<name>_itemAnchor`. It estimates the same dataset with the first affected item as the anchor (view B), so that the violation sits at the anchor itself.

The scenario `orthogonal_IUX_loading` is **not reported in the manuscript** and is off by default. It is a check that A8 can be broken through the loading channel alone; switch it on with `run_simulation_study(..., include_check = TRUE)`.

## What the manuscript reports

The manuscript reports the three estimators above on θ_I0 and θ_SD, for the eight scenarios and six twins. The code also computes quantities the manuscript does not report. They are tagged `[not in manuscript]` in the code:

- `functions/estimators/other_estimators.R`:
  - manifest IV and OLS on the true latent `X`;
  - ITT;
  - OLS on factor scores;
  - the two-step estimator with regression instead of Bartlett scores;
  - the correlations of `Z` with `X` and with its factor score;
- the population Wald ratio `TRUE_IV_I0`;
- the check scenario `orthogonal_IUX_loading`.

`estimator_map$in_manuscript` in `functions/shared_constants.R` marks the reported estimator × scale combinations.

## Result columns (`sim_res/results_n*.csv`)

One row per replication × scenario view.

| Column | Meaning |
|---|---|
| `iteration_id`, `simulation` | replication and scenario |
| `ATE_I0` | θ_I0 = β_Y / E[λ_0i], the truth for all I0-metric estimates |
| `ATE_X_scaled` | θ_SD = β_Y σ_X, the truth for all SD-scale estimates |
| `ATE_X` | β_Y, the effect per unit of `X` |
| `TRUE_IV_I0` | Cov(Z, Y) / Cov(Z, I0) [not in manuscript] |
| `sem`, `sem_scaled` | SEM-IV on θ_I0 and θ_SD |
| `TwoStep`, `TwoStep_scaled` | Two-step FA-IV (Bartlett scores) on θ_I0 and θ_SD |
| `anchorWald` | Anchor-Wald on θ_I0 |
| `manifestIV*`, `manifestOLS*`, `ITT*`, `ols_FS_reg*`, `TwoStep_reg*`, `z_x_hat_cor`, `z_x_star_cor` | [not in manuscript], see `other_estimators.R` |

Every estimate comes with its 95% confidence interval in `<name>_LowerCI` and `<name>_UpperCI`. The one exception is OLS on factor scores, which keeps an older naming: `ols_FS_reg_Beta` with `ols_FS_reg_lowerCI` and `ols_FS_reg_UpperCI`, and `ols_FS_reg_Beta_scaled` with `ols_FS_reg_scaled_LowerCI` and `ols_FS_reg_scaled_UpperCI`. An NA estimate is a failed replication. For SEM-IV this means non-convergence. For the two-step estimator it means a CFA that did not converge or returned a negative variance.
