# 03_figures.R
# The simulation figures of the manuscript.
#
# Input:  sim_res/long_n250.rds, sim_res/long_n2000.rds
# Output: <paper_dir>/Simulation Results/
#   SimFigure1.pdf             relative median bias of the three estimators,
#                              shaded by whether theory predicts the estimand to
#                              be identified; clean anchor, n = 2000
#   SimFigure2.pdf             bias distributions, scenarios in which theta_I0
#                              and theta_SD share a verdict; n = 250
#   SimFigure3.pdf             the same, scenarios in which only theta_I0 is
#                              identified
#   SimFigure4.pdf             the same, contaminated anchor
#   fig_verdict_itemAnchor.pdf SimFigure1 for the contaminated-anchor designs
#   dag_glyph_sheet.pdf        all 14 scenarios as DAG glyphs, with the key
#
# Every figure with a scenario axis carries a strip of DAG glyphs under it
# (functions/dag_glyphs.R).
source("functions/shared_constants.R")
source("functions/dag_glyphs.R")   # dag_strip(), glyph_key(), attach_dag_strip()

library(patchwork)

OUT <- file.path(paper_dir, "Simulation Results")

# cairo_pdf, not the default pdf device: the scenario labels contain ⱼ (U+2C7C)
# and → (U+2192), which the base device cannot encode.
save_fig <- function(name, plot, ...)
  ggsave(file.path(OUT, paste0(name, ".pdf")), plot, device = grDevices::cairo_pdf, ...)

# Glyph strip below the figure, legend on top.
with_dags <- function(p, scenarios, heights = c(1, 2.8))
  attach_dag_strip(p, scenarios, heights = heights, position = "bottom")

# Estimand headers (theta_I0, theta_SD): bold, large and centred, so they read
# as subheadings rather than as panel labels.
estimand_strip <- theme(strip.text = element_text(size = 15, hjust = 0.5,
                                                  margin = margin(t = 6, b = 4)))

# Plotmath, because the cairo sans face has ⱼ (U+2C7C) but not ₀ (U+2080):
# a literal "I₀" in device text comes out as an empty box.
stacked_scales       <- c("I₀ anchor metric", "X SD-scaled")
stacked_facet_labels <- c("I₀ anchor metric" = "bold('θ')[bold(italic(I))[bold(0)]]",
                          "X SD-scaled"      = "bold('θ')[bold(SD)]")
anchor_ests          <- c("SEM-IV", "Anchor-Wald", "Two-step FA-IV")

long_n2000 <- readRDS("sim_res/long_n2000.rds")
long_n250  <- readRDS("sim_res/long_n250.rds")


# === Glyph sheet (appendix) =====================================================
# All 14 scenarios as glyphs, plus glyph_key(), the legend for every glyph
# strip. Rows are families. The two anchor regimes are column-aligned (slot 2 is
# orthogonal_IUY in both rows, slot 3 orthogonal_IUX, ...), so reading down a
# column shows the same violation moving onto the anchor. Slot 1 of the
# contaminated-anchor row is empty because all_clean has no twin.
all_labels <- c(scenario_labels, item_anchor_scenario_labels)

sheet_row <- function(scs, title, k_slots = length(scs)) {
  keep <- !is.na(scs)
  lab <- all_labels[scs[keep]]
  lab[is.na(lab)] <- scs[keep][is.na(lab)]
  dag_strip(scs, k_slots = k_slots, text_size = 2.9, x_expand = 0,
            ylim = c(-0.45, 0.98)) +
    annotate("text", x = which(keep), y = -0.36,
             label = lab, size = 2.5,
             color = "grey30", lineheight = 0.95) +
    labs(subtitle = title) +
    theme(plot.subtitle = element_text(size = 9, face = "bold",
                                       color = "grey25",
                                       margin = margin(b = 1)))
}

row_clean <- sheet_row(
  c("all_clean", "orthogonal_IUY", "orthogonal_IUX", "one_confounder",
    "two_more_factors", "DIF_in_z", "item_nosh_violated"),
  "Clean anchor: violation at the non-anchor items")

row_item <- sheet_row(
  c(NA, "orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
    "one_confounder_itemAnchor", "two_more_factors_itemAnchor",
    "DIF_in_z_itemAnchor", "item_nosh_violated_itemAnchor"),
  # no subscript zero here: the bold face of the default device font lacks U+2080
  "Contaminated anchor: an affected item becomes the anchor")

# One structural glyph, so the key takes the rest of the row.
row_struct <- sheet_row("nosh_violated", "Structural (NOSH)")

# Saved near its printed width (about 6.5in): ggplot text has an absolute size,
# so a wide canvas scaled down in LaTeX would shrink the labels.
save_fig("dag_glyph_sheet",
         wrap_plots(row_clean, row_item,
                    (row_struct | glyph_key(text_size = 2.5)) +
                      plot_layout(widths = c(1, 6)),
                    ncol = 1, heights = c(1, 1, 1)),
         width = 8, height = 6.6)


# === Scenario groups ============================================================
# The eight clean-anchor scenarios, split by what theory predicts:
#   same     theta_I0 and theta_SD share a verdict: both identified (all_clean,
#            orthogonal_IUY) or both not (A4b in nosh_violated)
#   differ   theta_I0 identified, theta_SD not: the violation sits at the
#            ordinary items and breaks A5 (DIF_in_z), A6b (item_nosh_violated),
#            A7 (one_confounder, two_more_factors) or A8 (orthogonal_IUX)
same_scenarios   <- c("all_clean", "orthogonal_IUY", "nosh_violated")
differ_scenarios <- c("orthogonal_IUX", "DIF_in_z", "one_confounder",
                      "two_more_factors", "item_nosh_violated")

# The six contaminated-anchor designs (an affected item is the anchor)
item_anchor_scenarios <- c("orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
                           "one_confounder_itemAnchor", "two_more_factors_itemAnchor",
                           "DIF_in_z_itemAnchor", "item_nosh_violated_itemAnchor")


# === SimFigure1 and fig_verdict_itemAnchor: relative median bias ================
# Each cell prints the relative median bias, median(bias / truth), at n = 2000.
# Median, because the just-identified Wald ratio has no finite moments. The
# fill is not computed from the bias: it is theory's prediction (the yes/no
# columns of the scenario table), so readers can judge for themselves whether a
# number is off.
#
# Scenarios in which each estimand is identified. With a contaminated anchor,
# theta_I0 needs A1-A6 at the anchor only, so only DIF by Z (A5) and item NOSH
# (A6b) at the anchor break it; theta_SD needs A1-A8 at every item, which only
# orthogonal_IUY_itemAnchor satisfies.
identified_scenarios <- list(
  "I₀ anchor metric" = c("all_clean", "orthogonal_IUY", "orthogonal_IUX",
                         "DIF_in_z", "one_confounder", "two_more_factors",
                         "item_nosh_violated",
                         "orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
                         "one_confounder_itemAnchor",
                         "two_more_factors_itemAnchor"),
  "X SD-scaled"      = c("all_clean", "orthogonal_IUY",
                         "orthogonal_IUY_itemAnchor"))

# Scenarios that violate a restriction the estimator imposes, again from theory.
# SEM-IV and Two-step FA-IV build their first stage from a one-factor model of
# all items, so they need A5 to A8 at every item; SEM-IV also fixes the
# item-Y residual covariances to 0, which the U_j -> I_j, Y confounders break.
# Anchor-Wald uses the anchor alone. A cell is hatched where the estimand is
# identified but the estimator is on this list.
violated_scenarios <- list(
  "SEM-IV"         = c("orthogonal_IUY", "orthogonal_IUX", "DIF_in_z",
                       "one_confounder", "two_more_factors", "item_nosh_violated",
                       "orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
                       "one_confounder_itemAnchor",
                       "two_more_factors_itemAnchor", "DIF_in_z_itemAnchor",
                       "item_nosh_violated_itemAnchor"),
  "Two-step FA-IV" = c("orthogonal_IUX", "DIF_in_z", "one_confounder",
                       "two_more_factors", "item_nosh_violated",
                       "orthogonal_IUX_itemAnchor", "one_confounder_itemAnchor",
                       "two_more_factors_itemAnchor", "DIF_in_z_itemAnchor",
                       "item_nosh_violated_itemAnchor"),
  "Anchor-Wald"    = character(0))

# The item-Y restriction only shrinks the latent unit relative to the anchor;
# the reduced form and the standardized first stage stay intact, so theta_SD
# is unaffected: no hatch for SEM-IV on the SD scale in these scenarios.
unit_only_scenarios <- c("orthogonal_IUY", "orthogonal_IUY_itemAnchor")

# The hatched level keeps the identified blue, with orange hatch lines. Its
# fill is one digit off the blue: it looks the same, but lets draw_key_hatch()
# tell the two levels apart in the legend.
ident_levels <- c("Identified", "Violated", "Not identified")
ident_colors <- c(Identified = "#c6dbef", Violated = "#c6dbee",
                  "Not identified" = "#fdd0a2")
ident_labels <- c(Identified = "Identified",
                  Violated   = "Identified, estimator's\nassumptions violated",
                  "Not identified" = "Not identified")
hatch_colour <- "#fdae6b"

# Diagonal hatching over tiles. A stat, because discrete x/y are already mapped
# to integer positions when compute_panel runs, so this works with free-y facets.
# slope is in data units (tile width 1, height 1); about 5 makes the lines look
# roughly 45 degrees on the wide, flat tiles. spacing is along x.
StatHatch <- ggproto("StatHatch", Stat,
  required_aes = c("x", "y"),
  compute_panel = function(data, scales, slope = 5, spacing = 0.07) {
    offs <- seq(-(0.5 + 0.5 * slope), 0.5 + 0.5 * slope, by = slope * spacing)
    u0 <- pmax(-0.5, (-0.5 - offs) / slope)   # line v = slope * u + off,
    u1 <- pmin( 0.5, ( 0.5 - offs) / slope)   # clipped to the unit tile
    keep <- u0 < u1
    seg <- data.frame(u0 = u0[keep], u1 = u1[keep], off = offs[keep])
    rbindlist(lapply(seq_len(nrow(data)), function(i) {
      x <- as.numeric(data$x[i]); y <- as.numeric(data$y[i])
      data.table(x = x + seg$u0, xend = x + seg$u1,
                 y = y + slope * seg$u0 + seg$off,
                 yend = y + slope * seg$u1 + seg$off,
                 PANEL = data$PANEL[i], group = i)
    }))
  })

# Legend key with the hatch drawn on the "Violated" level
draw_key_hatch <- function(data, params, size) {
  key <- draw_key_rect(data, params, size)
  if (!identical(toupper(data$fill), toupper(ident_colors[["Violated"]])))
    return(key)
  offs <- seq(-0.75, 0.75, by = 0.25)
  grid::grobTree(key, grid::segmentsGrob(
    x0 = pmax(0, -offs), y0 = pmax(0, offs),
    x1 = pmin(1, 1 - offs), y1 = pmin(1, 1 + offs),
    gp = grid::gpar(col = hatch_colour, lwd = 1.4)))
}

verdict_grid <- function(long, scenarios, labels, ests = anchor_ests) {
  v <- long[!is.na(bias) & scenario %in% scenarios & estimator %in% ests &
              scale %in% stacked_scales,
            .(rel_bias = median(bias / truth)),
            by = .(scale, scenario, estimator)]
  v[, ident := scenario %in% identified_scenarios[[as.character(scale[1])]],
    by = scale]
  v[, viol := scenario %in% violated_scenarios[[as.character(estimator[1])]],
    by = estimator]
  v[estimator == "SEM-IV" & scale == "X SD-scaled" &
      scenario %in% unit_only_scenarios, viol := FALSE]
  v[, `:=`(ident    = factor(fifelse(!ident, "Not identified",
                                      fifelse(viol, "Violated", "Identified")),
                              levels = ident_levels),
           scenario  = factor(scenario, levels = scenarios),
           estimator = factor(estimator, levels = rev(ests)),
           scale     = factor(scale, levels = stacked_scales))]

  # Tile fill, then hatching, then a white border on top so the hatch lines
  # stop at the gaps; the label sits on a patch of the tile's own colour.
  ggplot(v, aes(x = scenario, y = estimator, fill = ident)) +
    geom_tile(key_glyph = draw_key_hatch) +
    geom_segment(data = v[ident == "Violated"],
                 aes(x = scenario, y = estimator),
                 stat = StatHatch, inherit.aes = FALSE,
                 colour = hatch_colour, linewidth = 0.5) +
    geom_tile(fill = NA, colour = "white", linewidth = 0.9) +
    # + 0 turns -0 into 0, so a bias that rounds to zero prints as +0.0%
    geom_label(aes(label = sprintf("%+.1f%%", round(100 * rel_bias, 1) + 0)), size = 2.8,
               border.colour = NA, label.r = unit(0, "pt"),
               label.padding = unit(0.12, "lines"), show.legend = FALSE) +
    # Estimand headers centred above each block; the heights keep the 3 and 2
    # estimator rows equally tall.
    ggh4x::facet_manual(vars(scale), design = "A\nB",
                        heights = c(length(ests), sum(ests != "Anchor-Wald")),
                        scales = "free_y", strip.position = "top",
                        labeller = labeller(scale = as_labeller(stacked_facet_labels,
                                                                label_parsed))) +
    scale_fill_manual(values = ident_colors, labels = ident_labels,
                      drop = FALSE) +
    scale_x_discrete(labels = labels) +
    labs(x = NULL, y = NULL) +
    theme_pub +
    estimand_strip +
    theme(axis.line = element_blank(), axis.ticks = element_blank())
}

verdict_clean <- c(same_scenarios, differ_scenarios)
save_fig("SimFigure1",
         with_dags(verdict_grid(long_n2000, verdict_clean, scenario_labels),
                   verdict_clean, heights = c(1, 2.2)),
         width = 11.5, height = 5.4)

save_fig("fig_verdict_itemAnchor",
         with_dags(verdict_grid(long_n2000, item_anchor_scenarios,
                                item_anchor_scenario_labels),
                   item_anchor_scenarios, heights = c(1, 2.2)),
         width = 9, height = 5.2)


# === SimFigures 2-4: bias distributions, both estimands stacked =================
# One x entry per scenario, theta_I0 on top and theta_SD below, so the two
# verdicts of a scenario read as one column. n = 250.
#
# oob_keep on the per-facet y scales clips the drawing without dropping data;
# with the default oob the boxplot quartiles would be recomputed on the
# truncated sample. The limits are the widest whisker per row plus 5%, so every
# box and whisker is drawn and only outlier points beyond them are clipped.
whisker_limits <- function(d, pad = 0.05) {
  w <- d[, as.list(range(boxplot.stats(bias)$stats[c(1, 5)])),
         by = .(scale, scenario, estimator)]
  w[, .(lo = min(V1, 0), hi = max(V2, 0)), by = scale][
    , `:=`(lo = lo - pad * (hi - lo), hi = hi + pad * (hi - lo))]
}

# relative = TRUE plots (estimate - truth) / truth instead of estimate - truth;
# used for the contaminated anchor, where the truth on the I0 metric changes with
# the item drawn in each replication.
stacked_fig <- function(scenarios, labels = scenario_labels, relative = FALSE) {
  d <- long_n250[!is.na(bias) & estimator %in% anchor_ests &
                   scale %in% stacked_scales & scenario %in% scenarios]
  if (relative) d[, bias := bias / truth]
  d[, `:=`(scenario  = factor(scenario, levels = scenarios),
           estimator = factor(estimator, levels = anchor_ests),
           scale     = factor(scale, levels = stacked_scales))]

  lim     <- whisker_limits(d)
  ylim_I0 <- unlist(lim[scale == "I₀ anchor metric", .(lo, hi)])
  ylim_SD <- unlist(lim[scale == "X SD-scaled",      .(lo, hi)])

  # Anchor-Wald has no SD-scale estimate. Dodging is panel-local, so the SD row
  # would centre its two boxes and SEM-IV would land at a different x than in
  # the row above. One off-panel row per scenario reserves the Anchor-Wald slot:
  # it exists for the dodge, oob_keep keeps it in the data, and the explicit
  # facet limits clip it, so nothing is drawn.
  d <- rbind(d,
             CJ(scenario  = factor(scenarios, levels = scenarios),
                estimator = factor("Anchor-Wald", levels = anchor_ests),
                scale     = factor("X SD-scaled", levels = stacked_scales),
                bias      = 1e6),
             fill = TRUE)

  p <- ggplot(d, aes(x = scenario, y = bias, fill = estimator)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_boxplot(outlier.size = 0.3, lwd = 0.25,
                 position = position_dodge2(preserve = "single")) +
    facet_wrap(~ scale, ncol = 1, scales = "free_y",
               labeller = as_labeller(stacked_facet_labels, label_parsed)) +
    ggh4x::facetted_pos_scales(y = list(
      scale == "I₀ anchor metric" ~
        scale_y_continuous(limits = ylim_I0, oob = scales::oob_keep),
      scale == "X SD-scaled" ~
        scale_y_continuous(limits = ylim_SD, oob = scales::oob_keep))) +
    scale_fill_manual(values = estimator_colors[anchor_ests]) +
    scale_x_discrete(labels = labels) +
    labs(x = NULL, y = if (relative) "(Estimate − True Value) / True Value" else "Estimate − True Value") +
    theme_pub +
    estimand_strip

  with_dags(p, scenarios, heights = c(1, 4.5))
}

save_fig("SimFigure2", stacked_fig(same_scenarios),   width = 9, height = 8)
save_fig("SimFigure3", stacked_fig(differ_scenarios), width = 9, height = 8)
save_fig("SimFigure4", stacked_fig(item_anchor_scenarios, item_anchor_scenario_labels,
                                   relative = TRUE),
         width = 10.5, height = 8)
