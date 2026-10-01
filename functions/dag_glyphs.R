# =============================================================================
# dag_glyphs.R — mini-DAG glyph strips for the scenario axis
#
# Sourced by 03_figures.R. Defines one main entry point:
#
#   attach_dag_strip(p, scenarios)   attach a glyph strip to an existing
#                                    scenario-axis figure
#
# plus dag_strip() (the strip alone) and glyph_key() (the legend). Nothing is
# read or written.
#
# Usage (the target figure needs no changes):
#   p <- ggplot(...) + ... + theme_pub
#   ggsave("... .pdf", attach_dag_strip(p, scenarios), width = 9, height = 6)
#
# `scenarios` must be the same vector, in the same order, that the figure uses
# for its x factor levels. The strip's x expansion defaults to ggplot's discrete
# default (add = 0.6 around 1..k), so the cells line up with the boxplot groups
# without touching the figure's scale_x_discrete().
#
# Design rules (they are what make the strip scannable):
#   * Every glyph carries the SAME grey skeleton — Z -> X -> Y, the X <- U -> Y
#     confounder that every scenario has (it is why we need an instrument), and
#     the indicators. Only the black marks differ, so the eye reads a row as
#     "what was switched on here".
#   * Two indicators (I0 and Ij) by default; a third (Ik) only where the
#     violation needs it to be read: a confounder shared by two items, or two
#     extra factors on different items.
#   * The anchor I0 is boxed in every glyph. The question the figure answers is
#     always "does the black edge touch the box?".
#   * Measurement violations live in the outer ring around the indicators;
#     structural (NOSH) violations are dashed bidirected arcs attached to the
#     midpoints of the *edges* whose coefficients covary — not to the nodes.
# =============================================================================
library(data.table)
library(ggplot2)
library(patchwork)
library(grid)

# --------------------------------------------------------------------- style
# Everything that controls how heavy the drawing looks lives here.
GLY <- list(
  skel_col  = "grey58",   # the assumed model
  skel_lw   = 0.45,
  viol_col  = "black",    # the edge that switches the violation on
  viol_lw   = 0.9,
  arc_lw    = 0.7,
  head_skel = 0.045,      # arrowhead length, inches
  head_viol = 0.055,
  node_size = 2.8,
  pad       = 0.045,      # edge trim at a node, in x-data units
  pad_frac  = 0.30,       # ... but never more than this share of the edge
  yx        = 0.42        # y-unit / x-unit physical size; makes the trim
)                         # isotropic on the page rather than in data units

# ------------------------------------------------------------ glyph geometry
# Rows: the X-Y confounder U on top, the causal row, the indicator row, and the
# extra factors (two_more_factors only) at the bottom.
ROW <- c(U = 0.92, causal = 0.70, item = 0.18, factor = -0.10)

NODES <- data.table(
  node   = c("Z",   "X",  "Y",  "U"),
  nx     = c(-0.40, 0.00, 0.40, 0.20),
  ny     = ROW[c("causal", "causal", "causal", "U")],
  anchor = FALSE,
  lab    = c("italic(Z)", "italic(X)", "italic(Y)", "italic(U)")
)

# Indicator layouts. I0 is always the leftmost, boxed.
ITEMS_2 <- data.table(node = c("I0", "Ij"), nx = c(-0.20, 0.20),
                      anchor = c(TRUE, FALSE),
                      lab = c("italic(I)[0]", "italic(I)[j]"))
ITEMS_3 <- data.table(node = c("I0", "Ij", "Ik"), nx = c(-0.28, 0.00, 0.28),
                      anchor = c(TRUE, FALSE, FALSE),
                      lab = c("italic(I)[0]", "italic(I)[j]", "italic(I)[k]"))
ITEMS_2[, ny := ROW[["item"]]]
ITEMS_3[, ny := ROW[["item"]]]
THREE_ITEMS <- c("one_confounder", "two_more_factors",
                 "two_more_factors_itemAnchor")

# Edges to an indicator a glyph does not draw are dropped by build_edges().
SKELETON <- data.table(
  from = c("Z", "X", "U", "U", "X",  "X",  "X"),
  to   = c("X", "Y", "X", "Y", "I0", "Ij", "Ik")
)

# Confounder slots sit *between* the indicator row and the causal row, so the
# violating edges stay short instead of sweeping across the whole glyph. Right
# slot = gold anchor (contamination at a non-anchor item), left slot = promoted
# anchor (contamination at I0). The pair reads as a mirror image.
UR <- c(nx =  0.40, ny = 0.38)
UL <- c(nx = -0.40, ny = 0.38)

EXTRA <- rbindlist(list(
  # --- gold anchor: the confounder hits Ij, away from the boxed anchor ------
  data.table(scenario = "orthogonal_IUY",
             node = "Uj", nx = UR[1], ny = UR[2], lab = "italic(U)[j]"),
  data.table(scenario = "orthogonal_IUX",
             node = "Uj", nx = UR[1], ny = UR[2], lab = "italic(U)[j]"),
  # Further out and up than UR: Ik sits at 0.28, and from UR the US -> Ik edge
  # would be too short to show a shaft.
  data.table(scenario = "one_confounder",
             node = "US", nx = 0.45, ny = 0.45, lab = "italic(U)[S]"),
  data.table(scenario = "two_more_factors",
             node = c("F1", "F2"), nx = c(0.00, 0.28), ny = ROW[["factor"]],
             lab = c("italic(F)[1]", "italic(F)[2]")),
  # --- promoted anchor: the same confounder now hits I0 ---------------------
  # Independent confounders: the promoted item brings its own U0 (left slot),
  # and every other item keeps its own Uj in the gold glyph's right slot. The
  # promoted glyph is its gold twin plus U0.
  data.table(scenario = "orthogonal_IUY_itemAnchor",
             node = c("U0", "Uj"), nx = c(UL[1], UR[1]), ny = c(UL[2], UR[2]),
             lab = c("italic(U)[0]", "italic(U)[j]")),
  data.table(scenario = "orthogonal_IUX_itemAnchor",
             node = c("U0", "Uj"), nx = c(UL[1], UR[1]), ny = c(UL[2], UR[2]),
             lab = c("italic(U)[0]", "italic(U)[j]")),
  data.table(scenario = "one_confounder_itemAnchor",
             node = "US", nx = UL[1], ny = UL[2], lab = "italic(U)[S]"),
  # F1 sits between I0 and Ij because the promoted anchor keeps loading on the
  # factor it shares with the rest of its block; F2 stays where the gold glyph
  # has it.
  data.table(scenario = "two_more_factors_itemAnchor",
             node = c("F1", "F2"), nx = c(-0.14, 0.28), ny = ROW[["factor"]],
             lab = c("italic(F)[1]", "italic(F)[2]"))
))

# Violating edges between nodes. Straight unless a curve is needed to keep two
# edges out of the same corridor: at this size a curve costs more legibility
# than the crossing it avoids.
EXTRA_EDGES <- rbindlist(list(
  data.table(scenario = "all_clean",
             from = character(), to = character(), curve = numeric()),
  # --- gold anchor ---------------------------------------------------------
  data.table(scenario = "orthogonal_IUY",   from = c("Uj", "Uj"),
             to = c("Ij", "Y")),
  data.table(scenario = "orthogonal_IUX",   from = c("Uj", "Uj"),
             to = c("Ij", "X")),
  data.table(scenario = "one_confounder",   from = c("US", "US", "US"),
             to = c("Ij", "Ik", "Y"), curve = c(0.16, 0, 0)),
  data.table(scenario = "two_more_factors", from = c("F1", "F2"),
             to = c("Ij", "Ik")),
  data.table(scenario = "DIF_in_z",         from = "Z", to = "Ij"),
  # --- promoted anchor: same picture, mirrored onto I0 ---------------------
  data.table(scenario = "orthogonal_IUY_itemAnchor",
             from = c("U0", "U0", "Uj", "Uj"), to = c("I0", "Y", "Ij", "Y")),
  data.table(scenario = "orthogonal_IUX_itemAnchor",
             from = c("U0", "U0", "Uj", "Uj"), to = c("I0", "X", "Ij", "X")),
  data.table(scenario = "one_confounder_itemAnchor",
             from = c("US", "US", "US"), to = c("I0", "Ij", "Y"),
             curve = c(0, -0.16, 0)),
  data.table(scenario = "two_more_factors_itemAnchor",
             from = c("F1", "F1", "F2"), to = c("I0", "Ij", "Ik")),
  # Every item has DIF, the promoted one included.
  data.table(scenario = "DIF_in_z_itemAnchor",         from = c("Z", "Z"),
             to = c("I0", "Ij"))
), fill = TRUE)
EXTRA_EDGES[is.na(curve), curve := 0]

# NOSH violations are covariances between *coefficients*, so they attach to the
# midpoints of two skeleton edges, not to nodes: a dashed bidirected arc from
# the Z->X edge to the X->Y edge (A4b), or to the X->I0 loading (A6b, item
# NOSH at the anchor). Positive
# curvature bows right of the direction of travel, so the A4b arc runs below
# the causal row: above it sits the X <- U -> Y confounder.
EDGE_ARCS <- rbindlist(list(
  data.table(scenario = "nosh_violated",
             a_from = "Z", a_to = "X", b_from = "X", b_to = "Y",
             curve = 0.55),
  data.table(scenario = "anchor_nosh_violated",
             a_from = "Z", a_to = "X", b_from = "X", b_to = "I0",
             curve = 0.45)
))

# A scenario with no entry anywhere would silently draw a clean skeleton, which
# would be a lie. dag_strip() warns instead.
# ("all_clean" is deliberately edge-free, so it is named rather than derived.)
GLYPH_SCENARIOS <- unique(c("all_clean", EXTRA_EDGES$scenario,
                            EDGE_ARCS$scenario))

# ------------------------------------------------------------------ drawing
# Trim a segment at both ends so arrowheads clear the node labels. The trim is
# measured on the page (via GLY$yx), otherwise a vertical edge is cut ~2.5x
# harder than a horizontal one, and it is capped at pad_frac of the edge so
# short edges (U -> Y, F -> I) keep a visible shaft.
trim_seg <- function(x1, y1, x2, y2, pad1 = GLY$pad, pad2 = GLY$pad,
                     yx = GLY$yx) {
  dx <- x2 - x1
  dy <- (y2 - y1) * yx
  len <- sqrt(dx^2 + dy^2); len[len == 0] <- 1
  ux <- dx / len; uy <- dy / len
  cap <- GLY$pad_frac * len
  pad1 <- pmin(pad1, cap); pad2 <- pmin(pad2, cap)
  data.table(x    = x1 + ux * pad1,  y    = y1 + uy * pad1 / yx,
             xend = x2 - ux * pad2,  yend = y2 - uy * pad2 / yx)
}

build_edges <- function(edges, nodes, kind) {
  if (is.null(edges) || nrow(edges) == 0) return(NULL)
  e <- merge(edges, nodes[, .(from = node, x1 = nx, y1 = ny)],
             by = "from", sort = FALSE)
  e <- merge(e, nodes[, .(to = node, x2 = nx, y2 = ny)],
             by = "to", sort = FALSE)
  cbind(trim_seg(e$x1, e$y1, e$x2, e$y2),
        data.table(curve = if ("curve" %in% names(e)) e$curve else 0,
                   kind = kind))
}

# Midpoint of a skeleton edge, used as an arc endpoint.
edge_mid <- function(nodes, a, b) {
  pa <- nodes[node == a]; pb <- nodes[node == b]
  list(x = (pa$nx + pb$nx) / 2, y = (pa$ny + pb$ny) / 2)
}

build_arcs <- function(arcs, nodes) {
  if (nrow(arcs) == 0) return(NULL)
  rbindlist(lapply(seq_len(nrow(arcs)), function(i) {
    a <- edge_mid(nodes, arcs$a_from[i], arcs$a_to[i])
    b <- edge_mid(nodes, arcs$b_from[i], arcs$b_to[i])
    # No padding: the arrowheads must land ON the two edges.
    cbind(trim_seg(a$x, a$y, b$x, b$y, pad1 = 0, pad2 = 0),
          data.table(curve = arcs$curve[i], kind = "arc"))
  }))
}

glyph_data <- function(scen_levels) {
  segs <- list(); nds <- list(); dts <- list()
  for (i in seq_along(scen_levels)) {
    sc <- scen_levels[i]
    if (is.na(sc)) next                       # empty slot, keeps the grid aligned
    nd <- rbind(NODES, if (sc %in% THREE_ITEMS) ITEMS_3 else ITEMS_2,
                EXTRA[scenario == sc, .(node, nx, ny, lab, anchor = FALSE)],
                fill = TRUE)
    arcs <- build_arcs(EDGE_ARCS[scenario == sc], nd)
    s <- rbindlist(list(
      build_edges(SKELETON, nd, "skeleton"),
      build_edges(EXTRA_EDGES[scenario == sc, .(from, to, curve)], nd,
                  "violation"),
      arcs
    ), fill = TRUE)
    if (!is.null(s) && nrow(s)) {
      s[, `:=`(x = x + i, xend = xend + i, scenario = sc)]
      segs[[length(segs) + 1L]] <- s
    }
    # Dots mark where an arc attaches, so it reads as "these two coefficients"
    # rather than as an arrow that stops in mid-air.
    if (!is.null(arcs) && nrow(arcs)) {
      dts[[length(dts) + 1L]] <- data.table(
        x = i + c(arcs$x, arcs$xend), y = c(arcs$y, arcs$yend))
    }
    nds[[length(nds) + 1L]] <- data.table(scenario = sc, node = nd$node,
                                          x = i + nd$nx,
                                          y = nd$ny, lab = nd$lab,
                                          anchor = nd$anchor)
  }
  list(segs  = rbindlist(segs, fill = TRUE),
       nodes = rbindlist(nds, fill = TRUE)[!is.na(lab)],
       dots  = rbindlist(dts))
}

# x_expand = 0.1 around limits [0.5, k+0.5] reproduces ggplot's discrete default
# (add = 0.6 around 1..k), which is what makes the strip line up with an
# untouched boxplot.
dag_strip <- function(scen_levels, k_slots = length(scen_levels),
                      text_size = GLY$node_size, ylim = c(-0.20, 0.98),
                      x_expand = 0.1) {
  unknown <- setdiff(na.omit(scen_levels), GLYPH_SCENARIOS)
  if (length(unknown))
    warning("no glyph defined for: ", paste(unknown, collapse = ", "),
            " — these cells will show a clean skeleton", call. = FALSE)

  g <- glyph_data(scen_levels)
  ar_s <- arrow(length = unit(GLY$head_skel, "in"), type = "closed")
  ar_v <- arrow(length = unit(GLY$head_viol, "in"), type = "closed")
  ar_b <- arrow(length = unit(GLY$head_viol, "in"), type = "closed",
                ends = "both")

  # geom_curve takes one curvature per layer, so curved edges are split by it.
  by_curve <- function(d, ...) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    lapply(split(d, d$curve), function(gg)
      geom_curve(data = gg, aes(x, y, xend = xend, yend = yend),
                 curvature = gg$curve[1], ...))
  }

  ggplot() +
    by_curve(g$segs[kind == "skeleton"],
             arrow = ar_s, linewidth = GLY$skel_lw, color = GLY$skel_col) +
    by_curve(g$segs[kind == "violation"],
             arrow = ar_v, linewidth = GLY$viol_lw, color = GLY$viol_col,
             lineend = "round") +
    by_curve(g$segs[kind == "arc"],
             arrow = ar_b, linewidth = GLY$arc_lw, color = GLY$viol_col,
             linetype = "22") +
    (if (nrow(g$dots)) geom_point(data = g$dots, aes(x, y), size = 0.55,
                                  color = GLY$viol_col)) +
    # White label boxes double as masks, so edges never run through a symbol.
    geom_label(data = g$nodes[anchor == FALSE], aes(x, y, label = lab),
               parse = TRUE, size = text_size, fill = "white",
               linewidth = 0, label.padding = unit(0.7, "pt"),
               label.r = unit(0, "pt")) +
    geom_label(data = g$nodes[anchor == TRUE], aes(x, y, label = lab),
               parse = TRUE, size = text_size, fill = "white",
               linewidth = 0.22, label.padding = unit(1.7, "pt"),
               label.r = unit(1.2, "pt")) +
    scale_x_continuous(limits = c(0.5, k_slots + 0.5),
                       expand = expansion(add = x_expand)) +
    scale_y_continuous(limits = ylim, expand = expansion(add = 0.02)) +
    theme_void() +
    theme(plot.margin = margin(2, 2, 0, 2))
}

# Hang a glyph strip above an existing scenario-axis figure. `scenarios` must
# match the figure's x factor levels, in order.
#
#   heights      strip : figure. Raise the first number for bigger glyphs.
#   x_expand     0.1 for an untouched discrete axis (ggplot's default), 0 if
#                the figure sets expansion(add = 0.5) itself.
#   trim_margin  drop the figure's top margin so the strip sits close.
#   labels_top   move the scenario names to the top axis, directly under the
#                glyphs, so names and DAGs sit together. The figure's own x
#                scale (labels included) is kept; only its position changes.
#   position     "top" (strip above the figure) or "bottom" (strip below it,
#                scenario names on the bottom axis just above the glyphs).
#                `heights` is always given strip-first; it is flipped for you.
attach_dag_strip <- function(p, scenarios, heights = c(1, 2.8),
                             x_expand = 0.1, trim_margin = TRUE,
                             labels_top = TRUE, position = c("top", "bottom")) {
  position <- match.arg(position)
  if (position == "bottom") {
    if (trim_margin)
      p <- p + theme(plot.margin = margin(5.5, 5.5, 0, 5.5))
    p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1,
                                              size = 9, lineheight = 0.95,
                                              margin = margin(t = 4)))
    return(p / dag_strip(scenarios, x_expand = x_expand) +
             plot_layout(heights = rev(heights), guides = "collect") &
             theme(legend.position = "right"))
  }
  if (trim_margin)
    p <- p + theme(plot.margin = margin(0, 5.5, 5.5, 5.5))
  if (labels_top) {
    sx <- p$scales$get_scales("x")
    sx <- if (is.null(sx)) scale_x_discrete() else sx$clone()
    sx$position <- "top"
    # Horizontal, centred names; no axis line or ticks up there, so the names
    # read as captions of the glyphs rather than as an axis of the boxplot.
    p <- suppressMessages(p + sx) +
      theme(axis.text.x.top  = element_text(angle = 0, hjust = 0.5, vjust = 0,
                                            size = 9, lineheight = 0.95,
                                            margin = margin(b = 4)),
            axis.line.x.top  = element_blank(),
            axis.ticks.x.top = element_blank())
  }
  dag_strip(scenarios, x_expand = x_expand) / p +
    plot_layout(heights = heights)
}

# ------------------------------------------------------------------- the key
# Standalone legend, for the glyph sheet or a figure that needs it inline. In
# the manuscript this belongs in the caption instead.
glyph_key <- function(text_size = 2.5) {
  seg <- function(x, ...) annotate("segment", x = x, xend = x + 0.055,
                                   y = 0.5, yend = 0.5, ...)
  txt <- function(x, lab) annotate("text", x = x + 0.075, y = 0.5, label = lab,
                                   hjust = 0, size = text_size, color = "grey20")
  ggplot() +
    seg(0.00, color = GLY$skel_col, linewidth = GLY$skel_lw,
        arrow = arrow(length = unit(GLY$head_skel, "in"), type = "closed")) +
    txt(0.00, "model as assumed") +
    seg(0.26, color = GLY$viol_col, linewidth = GLY$viol_lw,
        arrow = arrow(length = unit(GLY$head_viol, "in"), type = "closed")) +
    txt(0.26, "violating edge") +
    seg(0.50, color = GLY$viol_col, linewidth = GLY$arc_lw, linetype = "22",
        arrow = arrow(length = unit(GLY$head_viol, "in"), type = "closed",
                      ends = "both")) +
    txt(0.50, "covarying coefficients") +
    annotate("label", x = 0.795, y = 0.5, label = "italic(I)[0]", parse = TRUE,
             size = text_size, fill = "white", linewidth = 0.22,
             label.padding = unit(1.7, "pt"), label.r = unit(1.2, "pt")) +
    txt(0.76, "  anchor item") +
    scale_x_continuous(limits = c(-0.01, 1.0), expand = expansion(add = 0)) +
    scale_y_continuous(limits = c(0.2, 0.8), expand = expansion(add = 0)) +
    theme_void()
}
