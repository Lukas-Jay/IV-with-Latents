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
# =============================================================================
library(data.table)
library(ggplot2)
library(patchwork)
library(grid)

# Style
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
  yx        = 0.42        # y-unit / x-unit size on the page
)

# ------------------------------------------------------------ glyph geometry
# Rows, top to bottom: U, causal row, items, extra factors (two_more_factors).
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
  from  = c("Z", "X", "U", "U", "X",  "X",  "X"),
  to    = c("X", "Y", "X", "Y", "I0", "Ij", "Ik"),
  curve = 0
)

# Confounder slots between the item and causal rows, so the violating edges
# stay short. Right: contamination at Ij; left: contamination at I0.
UR <- c(nx =  0.40, ny = 0.38)
UL <- c(nx = -0.40, ny = 0.38)

EXTRA <- rbindlist(list(
  # --- clean anchor: the confounder hits Ij, away from the boxed anchor -----
  data.table(scenario = "orthogonal_IUY",
             node = "Uj", nx = UR[1], ny = UR[2], lab = "italic(U)[j]"),
  data.table(scenario = "orthogonal_IUX",
             node = "Uj", nx = UR[1], ny = UR[2], lab = "italic(U)[j]"),
  # Further out than UR, so the US -> Ik edge keeps a visible shaft.
  data.table(scenario = "one_confounder",
             node = "US", nx = 0.45, ny = 0.45, lab = "italic(U)[S]"),
  data.table(scenario = "two_more_factors",
             node = c("F1", "F2"), nx = c(0.00, 0.28), ny = ROW[["factor"]],
             lab = c("italic(F)[1]", "italic(F)[2]")),
  # --- contaminated anchor: the clean twin plus the anchor's confounder -----
  data.table(scenario = "orthogonal_IUY_itemAnchor",
             node = c("U0", "Uj"), nx = c(UL[1], UR[1]), ny = c(UL[2], UR[2]),
             lab = c("italic(U)[0]", "italic(U)[j]")),
  data.table(scenario = "orthogonal_IUX_itemAnchor",
             node = c("U0", "Uj"), nx = c(UL[1], UR[1]), ny = c(UL[2], UR[2]),
             lab = c("italic(U)[0]", "italic(U)[j]")),
  data.table(scenario = "one_confounder_itemAnchor",
             node = "US", nx = UL[1], ny = UL[2], lab = "italic(U)[S]"),
  # F1 between I0 and Ij: the anchor loads on F1 with the rest of its block.
  data.table(scenario = "two_more_factors_itemAnchor",
             node = c("F1", "F2"), nx = c(-0.14, 0.28), ny = ROW[["factor"]],
             lab = c("italic(F)[1]", "italic(F)[2]"))
))

# Violating edges between nodes; curved only where two edges would overlap.
EXTRA_EDGES <- rbindlist(list(
  data.table(scenario = "all_clean",
             from = character(), to = character(), curve = numeric()),
  # --- clean anchor --------------------------------------------------------
  data.table(scenario = "orthogonal_IUY",   from = c("Uj", "Uj"),
             to = c("Ij", "Y")),
  data.table(scenario = "orthogonal_IUX",   from = c("Uj", "Uj"),
             to = c("Ij", "X")),
  data.table(scenario = "one_confounder",   from = c("US", "US", "US"),
             to = c("Ij", "Ik", "Y"), curve = c(0.16, 0, 0)),
  data.table(scenario = "two_more_factors", from = c("F1", "F2"),
             to = c("Ij", "Ik")),
  data.table(scenario = "DIF_in_z",         from = "Z", to = "Ij"),
  # --- contaminated anchor: same picture, mirrored onto I0 -----------------
  data.table(scenario = "orthogonal_IUY_itemAnchor",
             from = c("U0", "U0", "Uj", "Uj"), to = c("I0", "Y", "Ij", "Y")),
  data.table(scenario = "orthogonal_IUX_itemAnchor",
             from = c("U0", "U0", "Uj", "Uj"), to = c("I0", "X", "Ij", "X")),
  data.table(scenario = "one_confounder_itemAnchor",
             from = c("US", "US", "US"), to = c("I0", "Ij", "Y"),
             curve = c(0, -0.16, 0)),
  data.table(scenario = "two_more_factors_itemAnchor",
             from = c("F1", "F1", "F2"), to = c("I0", "Ij", "Ik")),
  # Every item has DIF, the contaminated anchor included.
  data.table(scenario = "DIF_in_z_itemAnchor",         from = c("Z", "Z"),
             to = c("I0", "Ij"))
), fill = TRUE)
EXTRA_EDGES[is.na(curve), curve := 0]

# NOSH violations are covariances between coefficients: a dashed arc between
# the midpoints of Z->X and X->Y (A4b) or an X->I loading (A6b).
# Positive curvature bows the A4b arc below the causal row, away from U.
EDGE_ARCS <- rbindlist(list(
  data.table(scenario = "nosh_violated",
             a_from = "Z", a_to = "X", b_from = "X", b_to = "Y",
             curve = 0.55),
  data.table(scenario = "item_nosh_violated",
             a_from = "Z", a_to = "X", b_from = "X", b_to = "Ij",
             curve = 0),
  data.table(scenario = "item_nosh_violated_itemAnchor",
             a_from = "Z", a_to = "X", b_from = "X", b_to = c("I0", "Ij"),
             curve = c(0.45, 0))
))

# ------------------------------------------------------------------ drawing
# Trim both ends so arrowheads clear the node labels. Measured on the page
# (GLY$yx) and capped at pad_frac of the edge, so short edges keep a shaft.
trim_seg <- function(x1, y1, x2, y2, pad1 = GLY$pad, pad2 = GLY$pad,
                     yx = GLY$yx) {
  dx <- x2 - x1
  dy <- (y2 - y1) * yx
  len <- sqrt(dx^2 + dy^2)
  ux <- dx / len; uy <- dy / len
  cap <- GLY$pad_frac * len
  pad1 <- pmin(pad1, cap); pad2 <- pmin(pad2, cap)
  data.table(x    = x1 + ux * pad1,  y    = y1 + uy * pad1 / yx,
             xend = x2 - ux * pad2,  yend = y2 - uy * pad2 / yx)
}

build_edges <- function(edges, nodes, kind) {
  if (nrow(edges) == 0) return(NULL)
  e <- merge(edges, nodes[, .(from = node, x1 = nx, y1 = ny)],
             by = "from", sort = FALSE)
  e <- merge(e, nodes[, .(to = node, x2 = nx, y2 = ny)],
             by = "to", sort = FALSE)
  cbind(trim_seg(e$x1, e$y1, e$x2, e$y2), curve = e$curve, kind = kind)
}

# Midpoint of a skeleton edge, used as an arc endpoint.
edge_mid <- function(nodes, a, b) {
  pa <- nodes[node == a]; pb <- nodes[node == b]
  list(x = (pa$nx + pb$nx) / 2, y = (pa$ny + pb$ny) / 2)
}

build_arcs <- function(arcs, nodes) {
  rbindlist(lapply(seq_len(nrow(arcs)), function(i) {
    a <- edge_mid(nodes, arcs$a_from[i], arcs$a_to[i])
    b <- edge_mid(nodes, arcs$b_from[i], arcs$b_to[i])
    # No trim: the arrowheads land on the two edges.
    cbind(trim_seg(a$x, a$y, b$x, b$y, pad1 = 0, pad2 = 0),
          data.table(curve = arcs$curve[i], kind = "arc"))
  }))
}

glyph_data <- function(scen_levels) {
  segs <- list(); nds <- list()
  for (i in seq_along(scen_levels)) {
    sc <- scen_levels[i]
    if (is.na(sc)) next                       # empty slot, keeps the grid aligned
    nd <- rbind(NODES, if (sc %in% THREE_ITEMS) ITEMS_3 else ITEMS_2,
                EXTRA[scenario == sc, .(node, nx, ny, lab, anchor = FALSE)],
                fill = TRUE)
    s <- rbindlist(list(
      build_edges(SKELETON, nd, "skeleton"),
      build_edges(EXTRA_EDGES[scenario == sc, .(from, to, curve)], nd,
                  "violation"),
      build_arcs(EDGE_ARCS[scenario == sc], nd)
    ), fill = TRUE)
    segs[[i]] <- s[, `:=`(x = x + i, xend = xend + i)]
    nds[[i]] <- data.table(node = nd$node, x = i + nd$nx, y = nd$ny,
                           lab = nd$lab, anchor = nd$anchor)
  }
  segs <- rbindlist(segs)
  # Dots mark where an arc attaches to the two edges.
  arcs <- segs[kind == "arc"]
  list(segs  = segs,
       nodes = rbindlist(nds),
       dots  = data.table(x = c(arcs$x, arcs$xend), y = c(arcs$y, arcs$yend)))
}

# x_expand = 0.1 matches ggplot's discrete default, so the strip lines up with
# the figure's x axis.
dag_strip <- function(scen_levels, k_slots = length(scen_levels),
                      text_size = GLY$node_size, ylim = c(-0.20, 0.98),
                      x_expand = 0.1) {
  g <- glyph_data(scen_levels)
  ar_s <- arrow(length = unit(GLY$head_skel, "in"), type = "closed")
  ar_v <- arrow(length = unit(GLY$head_viol, "in"), type = "closed")
  ar_b <- arrow(length = unit(GLY$head_viol, "in"), type = "closed",
                ends = "both")

  # geom_curve takes one curvature per layer, so curved edges are split by it.
  by_curve <- function(d, ...) {
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
    geom_point(data = g$dots, aes(x, y), size = 0.55, color = GLY$viol_col) +
    # White label boxes mask the edges behind the symbols.
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

# Glyph strip below a scenario-axis figure, legend on top. `scenarios` = the
# figure's x levels, in order; heights = strip : figure.
attach_dag_strip <- function(p, scenarios, heights) {
  p <- p + theme(plot.margin = margin(5.5, 5.5, 0, 5.5),
                 axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1,
                                            size = 9, lineheight = 0.95,
                                            margin = margin(t = 4)))
  p / dag_strip(scenarios) +
    plot_layout(heights = rev(heights), guides = "collect") &
    theme(legend.position = "top", legend.direction = "horizontal",
          legend.margin = margin(b = 2))
}

# ------------------------------------------------------------------- the key
# Standalone legend for the glyph sheet.
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
