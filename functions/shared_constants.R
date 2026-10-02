# shared_constants.R
# Lookup tables, labels and the plot theme shared by the pipeline scripts
# 01-03. This is the one place that says which estimators and scenarios the
# manuscript reports.
library(data.table)
library(ggplot2)

# Folder of the manuscript. The tables (.tex rows) are written into it and the
# figures into its "Simulation Results/" subfolder; change it to write elsewhere.
paper_dir <- "../Latex Paper"

# --- Estimators ----------------------------------------------------------------
# One row per estimator x scale: the result columns it reads, and the truth
# column its bias is measured against.
#   in_manuscript: TRUE for the three manuscript estimators (SEM-IV, Anchor-Wald,
#   Two-step FA-IV) on the two scales theta_I0 and theta_SD. The other rows are
#   [not in manuscript] benchmarks: they go into tables/summary_*.csv only.
# "Two-step FA-IV" reads the Bartlett-score columns (TwoStep*); "OLS on factor
# scores" reads the regression-score columns (ols_FS_reg*), with which OLS is
# free of attenuation and shows the confounding bias alone.
# ITT estimates the reduced form, so its "bias" against ATE_X is expected.
estimator_map <- data.table(
  estimator_col = c("sem", "anchorWald", "TwoStep", "ols_FS_reg_Beta",
                    "manifestIV", "manifestOLS", "ITT",
                    "sem_scaled", "manifestIV_scaled", "TwoStep_scaled",
                    "ols_FS_reg_Beta_scaled", "manifestOLS_scaled"),
  lower_col     = c("sem_LowerCI", "anchorWald_LowerCI",
                    "TwoStep_LowerCI", "ols_FS_reg_lowerCI",
                    "manifestIV_LowerCI", "manifestOLS_LowerCI", "ITT_LowerCI",
                    "sem_scaled_LowerCI", "manifestIV_scaled_LowerCI",
                    "TwoStep_scaled_LowerCI", "ols_FS_reg_scaled_LowerCI",
                    "manifestOLS_scaled_LowerCI"),
  upper_col     = c("sem_UpperCI", "anchorWald_UpperCI",
                    "TwoStep_UpperCI", "ols_FS_reg_UpperCI",
                    "manifestIV_UpperCI", "manifestOLS_UpperCI", "ITT_UpperCI",
                    "sem_scaled_UpperCI", "manifestIV_scaled_UpperCI",
                    "TwoStep_scaled_UpperCI", "ols_FS_reg_scaled_UpperCI",
                    "manifestOLS_scaled_UpperCI"),
  estimator     = c("SEM-IV", "Anchor-Wald",
                    "Two-step FA-IV", "OLS on factor scores",
                    "Manifest IV", "Manifest OLS", "ITT",
                    "SEM-IV", "Manifest IV", "Two-step FA-IV",
                    "OLS on factor scores", "Manifest OLS"),
  scale         = c("I₀ anchor metric", "I₀ anchor metric",
                    "I₀ anchor metric", "I₀ anchor metric",
                    "X raw (per unit X*)", "X raw (per unit X*)", "X raw (per unit X*)",
                    "X SD-scaled", "X SD-scaled", "X SD-scaled",
                    "X SD-scaled", "X SD-scaled"),
  truth_col     = c("ATE_I0", "ATE_I0", "ATE_I0", "ATE_I0",
                    "ATE_X", "ATE_X", "ATE_X",
                    "ATE_X_scaled", "ATE_X_scaled", "ATE_X_scaled",
                    "ATE_X_scaled", "ATE_X_scaled"),
  in_manuscript = c(TRUE, TRUE, TRUE, FALSE,
                    FALSE, FALSE, FALSE,
                    TRUE, FALSE, TRUE,
                    FALSE, FALSE)
)

scale_levels <- c("I₀ anchor metric",
                  "X raw (per unit X*)", "X SD-scaled")

estimator_levels <- c("SEM-IV", "Anchor-Wald",
                      "Two-step FA-IV",
                      "OLS on factor scores", "Manifest IV", "Manifest OLS", "ITT")

estimator_colors <- setNames(
  rev(scales::viridis_pal(option = "D")(length(estimator_levels))),
  estimator_levels
)

# --- Scenarios -------------------------------------------------------------------
scenario_levels <- c(
  # reported: clean anchor
  "all_clean", "orthogonal_IUY", "orthogonal_IUX",
  "DIF_in_z", "one_confounder", "two_more_factors",
  "nosh_violated", "item_nosh_violated",
  # reported: contaminated anchor (an affected item is I₀)
  "orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
  "one_confounder_itemAnchor", "two_more_factors_itemAnchor",
  "DIF_in_z_itemAnchor", "item_nosh_violated_itemAnchor",
  # [not in manuscript] check scenario (A8 through the loading channel), only
  # with run_simulation_study(include_check = TRUE)
  "orthogonal_IUX_loading", "orthogonal_IUX_loading_itemAnchor")

# Figure axis labels: the scenario name, and the assumption at stake on a
# second line
scenario_labels <- c(
  all_clean            = "All clean\n(baseline)",
  orthogonal_IUY       = "Indep. Uⱼ → Iⱼ,Y\n(A7 holds)",
  orthogonal_IUX       = "Indep. Uⱼ → Iⱼ,X\n(A8 violated)",
  DIF_in_z             = "DIF by Z\n(A5 violated)",
  one_confounder       = "Shared U → I,Y\n(A7 violated)",
  two_more_factors     = "Two more factors\n(A7 violated)",
  nosh_violated        = "NOSH\n(A4b violated)",
  item_nosh_violated   = "Item-NOSH\n(A6b violated)"
)

# The same for the contaminated-anchor designs (an affected item is I₀). Each
# label states the assumption at stake at the anchor. theta_I0 needs A1-A6 at
# the anchor only, so only DIF by Z (A5) and item NOSH (A6b) at the anchor
# break it.
item_anchor_scenario_labels <- c(
  orthogonal_IUY_itemAnchor     = "Indep. Uⱼ → Iⱼ,Y\n(A7, A8 hold)",
  orthogonal_IUX_itemAnchor     = "Indep. Uⱼ → Iⱼ,X\n(A8 violated)",
  one_confounder_itemAnchor     = "Shared U → I,Y\n(A7 violated)",
  two_more_factors_itemAnchor   = "Two more factors\n(A7 violated)",
  DIF_in_z_itemAnchor           = "DIF by Z\n(A5 violated)",
  item_nosh_violated_itemAnchor = "Item-NOSH\n(A6b violated)"
)

# Row names of the manuscript tables (02_tables.R), as in the scenario table of
# the manuscript. The order is the split used in the figures and tables: in the
# first three rows both estimands share a verdict, in the last five only
# theta_I0 is identified.
scenario_tex_names <- c(
  all_clean            = "All clean",
  orthogonal_IUY       = "Indep.\\ $U_j \\to I_j, Y$",
  nosh_violated        = "NOSH violated",
  orthogonal_IUX       = "Indep.\\ $U_j \\to I_j, X$",
  DIF_in_z             = "DIF by $Z$",
  one_confounder       = "Shared $U \\to I, Y$",
  two_more_factors     = "Two more factors",
  item_nosh_violated   = "Item-NOSH violated"
)

# Contaminated-anchor designs. The table caption says that the anchor is
# contaminated; DIF by Z and item NOSH keep "at the anchor" because they are
# the two violations theta_I0 cannot survive.
item_anchor_tex_names <- c(
  orthogonal_IUY_itemAnchor     = "Indep.\\ $U_j \\to I_j, Y$",
  orthogonal_IUX_itemAnchor     = "Indep.\\ $U_j \\to I_j, X$",
  one_confounder_itemAnchor     = "Shared $U \\to I, Y$",
  two_more_factors_itemAnchor   = "Two more factors",
  DIF_in_z_itemAnchor           = "DIF by $Z$ at the anchor",
  item_nosh_violated_itemAnchor = "Item-NOSH at the anchor"
)

# --- Plot theme --------------------------------------------------------------------
theme_pub <- theme_classic(base_size = 11) +
  theme(panel.grid        = element_blank(),
        panel.border      = element_blank(),
        axis.line         = element_line(color = "black", linewidth = 0.4),
        strip.background  = element_blank(),
        strip.text        = element_text(size = 10),
        legend.position   = "bottom",
        legend.title      = element_blank(),
        legend.margin     = margin(t = -4),
        axis.text.x       = element_text(angle = 15, hjust = 1),
        axis.text.y       = element_text(size = 9))
