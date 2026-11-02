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

# cairo_pdf: the default pdf device cannot encode ⱼ and →.
save_fig <- function(name, plot, ...)
  ggsave(file.path(OUT, paste0(name, ".pdf")), plot, device = grDevices::cairo_pdf, ...)

# Glyph strip below the figure, legend on top.
with_dags <- function(p, scenarios, heights)
  attach_dag_strip(p, scenarios, heights = heights)

# Estimand headers (theta_I0, theta_SD): large and centred.
estimand_strip <- theme(strip.text = element_text(size = 15, hjust = 0.5,
                                                  margin = margin(t = 6, b = 4)))

# Plotmath: the cairo sans face has no ₀ (U+2080).
stacked_scales       <- c("I₀ anchor metric", "X SD-scaled")
stacked_facet_labels <- c("I₀ anchor metric" = "bold('θ')[bold(italic(I))[bold(0)]]",
                          "X SD-scaled"      = "bold('θ')[bold(SD)]")
anchor_ests          <- c("SEM-IV", "Anchor-Wald", "Two-step FA-IV")

long_n2000 <- readRDS("sim_res/long_n2000.rds")
long_n250  <- readRDS("sim_res/long_n250.rds")


# === Glyph sheet (appendix) =====================================================
# All 14 scenarios as glyphs, plus the key. Rows are families; a column shows
# the same violation in both anchor regimes (all_clean has no twin).
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
  # no ₀: the bold default font lacks it
  "Contaminated anchor: an affected item becomes the anchor")

# One glyph; the key fills the rest of the row.
row_struct <- sheet_row("nosh_violated", "Structural (NOSH)")

# Saved near printed width (6.5in) so the text is not shrunk in LaTeX.
save_fig("dag_glyph_sheet",
         wrap_plots(row_clean, row_item,
                    (row_struct | glyph_key(text_size = 2.5)) +
                      plot_layout(widths = c(1, 6)),
                    ncol = 1, heights = c(1, 1, 1)),
         width = 8, height = 6.6)


# === Scenario groups ============================================================
# The eight clean-anchor scenarios, split by what the theory predicts.
# same: both estimands share a verdict; differ: only theta_I0 is identified.
same_scenarios   <- c("all_clean", "orthogonal_IUY", "nosh_violated")
differ_scenarios <- c("orthogonal_IUX", "DIF_in_z", "one_confounder",
                      "two_more_factors", "item_nosh_violated")

# The six contaminated-anchor designs (an affected item is the anchor)
item_anchor_scenarios <- c("orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
                           "one_confounder_itemAnchor", "two_more_factors_itemAnchor",
                           "DIF_in_z_itemAnchor", "item_nosh_violated_itemAnchor")


# === SimFigure1 and fig_verdict_itemAnchor: relative median bias ================
# Relative median bias per cell, n = 2000 (median: the Wald ratio has no
# finite moments). The fill is theory's prediction, not computed from the bias.
#
# Scenarios in which each estimand is identified, from theory.
identified_scenarios <- list(
  "I₀ anchor metric" = c("all_clean", "orthogonal_IUY", "orthogonal_IUX",
                         "DIF_in_z", "one_confounder", "two_more_factors",
                         "item_nosh_violated",
                         "orthogonal_IUY_itemAnchor", "orthogonal_IUX_itemAnchor",
                         "one_confounder_itemAnchor",
                         "two_more_factors_itemAnchor"),
  "X SD-scaled"      = c("all_clean", "orthogonal_IUY",
                         "orthogonal_IUY_itemAnchor"))

# Scenarios that violate a restriction the estimator imposes, from theory.
# Hatched where the estimand is identified.
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

# The item-Y restriction shifts only the I0 unit, so no SEM-IV hatch on the SD scale.
unit_only_scenarios <- c("orthogonal_IUY", "orthogonal_IUY_itemAnchor")

# Violated: identified blue with orange hatch; the fill is one digit off so
# draw_key_hatch() can tell the levels apart.
ident_levels <- c("Identified", "Violated", "Not identified")
ident_colors <- c(Identified = "#c6dbef", Violated = "#c6dbee",
                  "Not identified" = "#fdd0a2")
ident_labels <- c(Identified = "Identified",
                  Violated   = "Identified, estimator's\nassumptions violated",
                  "Not identified" = "Not identified")
hatch_colour <- "#fdae6b"

# Diagonal hatch segments per tile; a stat so it works with free-y facets.
# slope = 5 looks like about 45 degrees on the flat tiles.
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

  # Fill, hatch, then a white border that cuts the hatch at the gaps.
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
    # heights keep the 3 and 2 estimator rows equally tall.
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
# theta_I0 on top, theta_SD below; n = 250.
#
# y limits: widest whisker per row + 5%. oob_keep clips the drawing without
# recomputing the boxplot quartiles.
whisker_limits <- function(d, pad = 0.05) {
  w <- d[, as.list(range(boxplot.stats(bias)$stats[c(1, 5)])),
         by = .(scale, scenario, estimator)]
  w[, .(lo = min(V1, 0), hi = max(V2, 0)), by = scale][
    , `:=`(lo = lo - pad * (hi - lo), hi = hi + pad * (hi - lo))]
}

# relative = TRUE: bias / truth (contaminated anchor: truth varies per replication).
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

  # Anchor-Wald has no SD estimate, so the SD row would centre its two boxes and
  # misalign them with the I0 row. An off-panel Anchor-Wald row (clipped by the
  # y limits) reserves its slot.
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
