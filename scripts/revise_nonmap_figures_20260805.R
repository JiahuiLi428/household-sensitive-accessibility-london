# Rebuilds the dissertation's non-map statistical figures with a single
# low-saturation visual system and spatially cautious uncertainty summaries.
#
# Reads existing analysis outputs only. It does not rerun routing or alter the
# model pipeline. All graphics are drawn and exported in R.

library(data.table)
library(ggplot2)
library(grid)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
analysis_dir <- file.path(project_root, "data", "output", "analysis")
extension_table_dir <- file.path(project_root, "extension", "outputs", "tables")
out_dir <- file.path(project_root, "figures", "publication_nonmap_revised_20260805")
source_dir <- file.path(out_dir, "source_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

pal <- list(
  child_light = "#FBDDDD",
  child_dark = "#F09A9B",
  care_light = "#C4D9F1",
  care_dark = "#90B7E2",
  older_light = "#BBB9ED",
  older_dark = "#8086C0",
  ink = "#252525",
  muted = "#6B6B6B",
  rule = "#B8B8B8",
  pale_rule = "#E6E6E6",
  warm_bg = "#FBE8E8",
  warm2_bg = "#FFF1DE",
  cool_bg = "#EDF5FB",
  cool2_bg = "#E3EDF8"
)

group_cols <- c(
  "Children" = pal$child_dark,
  "Caregiving adults" = pal$care_dark,
  "Older adults" = pal$older_dark
)

scenario_cols <- c(
  "WALK 15" = pal$care_light,
  "WALK + PT 30" = pal$care_dark
)

gain_cols <- c(
  "Bottom 20%" = pal$care_light,
  "Top 20%" = pal$child_dark
)

theme_paper <- function(base_size = 6.5) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = pal$ink),
      axis.ticks = element_line(linewidth = 0.30, colour = pal$ink),
      axis.text = element_text(size = base_size - 0.6, colour = pal$ink),
      axis.title = element_text(size = base_size, colour = pal$ink),
      strip.background = element_blank(),
      strip.text = element_text(size = base_size + 0.2, face = "bold", colour = pal$ink),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 0.6, colour = pal$ink),
      legend.key.height = unit(3.2, "mm"),
      legend.key.width = unit(4.0, "mm"),
      plot.title = element_text(size = base_size + 0.5, face = "bold", colour = pal$ink),
      plot.subtitle = element_text(size = base_size - 0.4, colour = pal$muted),
      plot.caption = element_text(size = base_size - 1.0, colour = pal$muted, hjust = 0),
      panel.grid = element_blank(),
      plot.margin = margin(3, 4, 3, 3)
    )
}

open_and_draw <- function(path, type, draw_fun, width_mm, height_mm, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  if (type == "png") {
    ragg::agg_png(path, width = width_mm, height = height_mm, units = "mm", res = dpi,
                  background = "white")
  } else if (type == "tiff") {
    ragg::agg_tiff(path, width = width_mm, height = height_mm, units = "mm", res = dpi,
                   compression = "lzw", background = "white")
  } else if (type == "svg") {
    svglite::svglite(path, width = width_in, height = height_in,
                     system_fonts = list(sans = "Arial"))
  } else if (type == "pdf") {
    grDevices::cairo_pdf(path, width = width_in, height = height_in, family = "Arial")
  } else {
    stop("Unknown graphics type: ", type)
  }
  on.exit(dev.off(), add = TRUE)
  draw_fun()
}

save_page <- function(draw_fun, stem, width_mm, height_mm, dpi = 600) {
  for (type in c("svg", "pdf", "tiff", "png")) {
    open_and_draw(
      file.path(out_dir, paste0(stem, ".", type)), type, draw_fun,
      width_mm = width_mm, height_mm = height_mm, dpi = dpi
    )
  }
}

draw_layout <- function(plots, layout_matrix, widths = NULL, heights = NULL) {
  nr <- nrow(layout_matrix)
  nc <- ncol(layout_matrix)
  if (is.null(widths)) widths <- rep(1, nc)
  if (is.null(heights)) heights <- rep(1, nr)
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  pushViewport(viewport(layout = grid.layout(
    nr, nc,
    widths = unit(widths, "null"),
    heights = unit(heights, "null")
  )))
  for (i in seq_along(plots)) {
    pos <- which(layout_matrix == i, arr.ind = TRUE)
    if (nrow(pos) == 0) next
    print(
      plots[[i]],
      vp = viewport(
        layout.pos.row = seq.int(min(pos[, "row"]), max(pos[, "row"])),
        layout.pos.col = seq.int(min(pos[, "col"]), max(pos[, "col"]))
      )
    )
  }
  popViewport()
}

cluster_mean_ci <- function(dt, value_col, cluster_col = "borough_name",
                            reps = 1500L, seed = 20260805L) {
  x <- dt[is.finite(get(value_col)) & !is.na(get(cluster_col)),
          .(value = get(value_col), cluster = get(cluster_col))]
  if (nrow(x) == 0L) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  blocks <- split(x$value, x$cluster)
  block_names <- names(blocks)
  set.seed(seed)
  boot <- replicate(reps, {
    sampled <- sample(block_names, length(block_names), replace = TRUE)
    mean(unlist(blocks[sampled], use.names = FALSE), na.rm = TRUE)
  })
  c(
    mean = mean(x$value, na.rm = TRUE),
    lo = unname(quantile(boot, 0.025, na.rm = TRUE)),
    hi = unname(quantile(boot, 0.975, na.rm = TRUE))
  )
}

summarise_cluster_ci <- function(dt, by_cols, value_col, reps = 1500L,
                                 seed = 20260805L) {
  dt[, as.list(cluster_mean_ci(.SD, value_col, reps = reps, seed = seed + .GRP)),
     by = by_cols]
}

facility_labels <- c(
  education_schools = "Schools",
  food_retail_supermarkets = "Supermarkets",
  green_space_parks = "Parks",
  healthcare_gps = "GPs",
  healthcare_hospitals = "Hospitals",
  healthcare_pharmacies = "Pharmacies",
  transport_bus_stops = "Bus stops",
  transport_underground_metro_stations = "Underground / metro"
)

selected_facilities <- c(
  "green_space_parks",
  "transport_bus_stops",
  "education_schools",
  "food_retail_supermarkets",
  "healthcare_pharmacies",
  "healthcare_gps"
)

# -----------------------------------------------------------------------------
# Shared inference table: BH adjustment across the 16 facility-scenario tests.
# -----------------------------------------------------------------------------

eta_wide <- fread(file.path(analysis_dir, "walk_vs_transit_imd_inequality_effect_sizes.csv"))
eta_long <- rbind(
  eta_wide[, .(
    facility_category,
    scenario = "WALK 15",
    eta_sq_h = eta_sq_h_walk_15min,
    p_value = p_value_walk_15min,
    n = n_walk_15min
  )],
  eta_wide[, .(
    facility_category,
    scenario = "WALK + PT 30",
    eta_sq_h = eta_sq_h_walk_transit_30min,
    p_value = p_value_walk_transit_30min,
    n = n_walk_transit_30min
  )]
)
eta_long[, q_bh_16 := p.adjust(p_value, method = "BH")]
eta_long[, facility := unname(facility_labels[facility_category])]
eta_long[, significant_bh := q_bh_16 < 0.05]
setorder(eta_long, facility_category, scenario)
fwrite(eta_long, file.path(source_dir, "figure5_figure9_imd_effect_sizes_bh.csv"))

# -----------------------------------------------------------------------------
# Figure 5: three IMD distributions + walking-scenario effect-size ranking.
# -----------------------------------------------------------------------------

indices <- fread(file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv"))
dist_long <- melt(
  indices[!is.na(IMD_Decile)],
  id.vars = c("LSOA21CD", "IMD_Decile"),
  measure.vars = c("family_walk_index", "working_walk_index", "older_walk_index"),
  variable.name = "group_key",
  value.name = "accessibility_score"
)
dist_long[, group := factor(
  group_key,
  levels = c("family_walk_index", "working_walk_index", "older_walk_index"),
  labels = c("Children", "Caregiving adults", "Older adults")
)]
dist_long[, imd := factor(IMD_Decile, levels = 1:10)]
imd_n <- unique(dist_long[, .(n = uniqueN(LSOA21CD)), by = imd])
imd_labels <- setNames(paste0(imd_n$imd, "\n(n=", format(imd_n$n, big.mark = ","), ")"), imd_n$imd)
fwrite(dist_long, file.path(source_dir, "figure5_imd_distributions.csv"))

make_violin <- function(group_name, colour, tag) {
  ggplot(dist_long[group == group_name], aes(x = imd, y = accessibility_score)) +
    geom_violin(fill = colour, colour = pal$ink, linewidth = 0.22,
                alpha = 0.72, trim = FALSE, width = 0.90) +
    geom_boxplot(width = 0.17, fill = "white", colour = pal$ink,
                 linewidth = 0.28, outlier.shape = NA) +
    scale_x_discrete(labels = imd_labels) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                       expand = expansion(mult = c(0, 0.03))) +
    labs(title = paste0(tag, "  ", group_name), x = "IMD decile", y = "Accessibility score (0-100)") +
    theme_paper(6.3) +
    theme(axis.text.x = element_text(size = 4.5, lineheight = 0.88))
}

p5a <- make_violin("Children", pal$child_light, "a")
p5b <- make_violin("Caregiving adults", pal$care_light, "b")
p5c <- make_violin("Older adults", pal$older_light, "c")

eta_walk <- eta_long[
  scenario == "WALK 15" & facility_category %in% selected_facilities
]
eta_walk[, facility := factor(facility, levels = facility[order(eta_sq_h)])]
p5d <- ggplot(eta_walk, aes(x = eta_sq_h, y = facility)) +
  geom_col(aes(fill = significant_bh), width = 0.62, colour = pal$ink, linewidth = 0.24) +
  geom_text(aes(label = sprintf("%.3f", eta_sq_h)), hjust = -0.12,
            size = 1.85, colour = pal$ink) +
  scale_fill_manual(values = c(`TRUE` = pal$care_dark, `FALSE` = "#D9D9D9"), guide = "none") +
  scale_x_continuous(limits = c(0, max(eta_walk$eta_sq_h) * 1.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "d  IMD effect-size ranking (WALK 15)",
       x = expression(eta[H]^2), y = NULL,
       caption = "Filled = BH-adjusted q < 0.05; grey = not significant") +
  theme_paper(6.3) +
  theme(plot.caption = element_text(size = 4.8, colour = pal$muted))

draw5 <- function() draw_layout(list(p5a, p5b, p5c, p5d), matrix(1:4, 2, 2, byrow = TRUE))
save_page(draw5, "figure5_deprivation_accessibility_revised", 183, 148)

# -----------------------------------------------------------------------------
# Figure 8: current caregiving need-access quadrants with direct statistics.
# -----------------------------------------------------------------------------

care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))
need_cut <- median(care$care_need_score, na.rm = TRUE)
access_cut <- median(care$care_walk_index, na.rm = TRUE)
care[, quadrant := fcase(
  care_need_score >= need_cut & care_walk_index < access_cut, "High need / low access",
  care_need_score >= need_cut & care_walk_index >= access_cut, "High need / high access",
  care_need_score < need_cut & care_walk_index < access_cut, "Low need / low access",
  default = "Low need / high access"
)]
quad_summary <- care[, .(
  n = .N,
  mean_need = mean(care_need_score, na.rm = TRUE),
  mean_access = mean(care_walk_index, na.rm = TRUE),
  mean_mismatch = mean(care_mismatch_index, na.rm = TRUE)
), by = quadrant]
fwrite(quad_summary, file.path(source_dir, "figure8_quadrant_summary.csv"))

quad_label <- function(q) {
  r <- quad_summary[quadrant == q]
  sprintf("%s\nn = %s\nneed %.1f | access %.1f\nmismatch %.1f",
          q, format(r$n, big.mark = ","), r$mean_need, r$mean_access, r$mean_mismatch)
}

quad_point_cols <- c(
  "High need / low access" = pal$child_dark,
  "High need / high access" = "#D7A66B",
  "Low need / low access" = pal$care_light,
  "Low need / high access" = pal$care_dark
)

p8 <- ggplot(care, aes(x = care_walk_index, y = care_need_score)) +
  annotate("rect", xmin = -Inf, xmax = access_cut, ymin = need_cut, ymax = Inf,
           fill = pal$warm_bg) +
  annotate("rect", xmin = access_cut, xmax = Inf, ymin = need_cut, ymax = Inf,
           fill = pal$warm2_bg) +
  annotate("rect", xmin = -Inf, xmax = access_cut, ymin = -Inf, ymax = need_cut,
           fill = pal$cool_bg) +
  annotate("rect", xmin = access_cut, xmax = Inf, ymin = -Inf, ymax = need_cut,
           fill = pal$cool2_bg) +
  geom_point(aes(colour = quadrant), alpha = 0.28, size = 0.42, stroke = 0) +
  geom_vline(xintercept = access_cut, linetype = "dashed", linewidth = 0.35, colour = pal$muted) +
  geom_hline(yintercept = need_cut, linetype = "dashed", linewidth = 0.35, colour = pal$muted) +
  annotate("text", x = 16, y = 95, label = quad_label("High need / low access"),
           size = 2.05, lineheight = 0.95, fontface = "bold", colour = "#7D1D25") +
  annotate("text", x = 82, y = 95, label = quad_label("High need / high access"),
           size = 1.95, lineheight = 0.95, colour = "#76501F") +
  annotate("text", x = 16, y = 10, label = quad_label("Low need / low access"),
           size = 1.95, lineheight = 0.95, colour = "#315B78") +
  annotate("text", x = 82, y = 10, label = quad_label("Low need / high access"),
           size = 1.95, lineheight = 0.95, colour = "#234F7A") +
  scale_colour_manual(values = quad_point_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Caregiving adult accessibility score (0-100)",
       y = "Caregiving adult need score (0-100)") +
  theme_paper(6.6)

draw8 <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  print(p8, vp = viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98))
}
save_page(draw8, "figure8_need_access_quadrants_revised", 160, 120)

# -----------------------------------------------------------------------------
# Figure 9: paired scenario dumbbells for access and IMD effect sizes.
# -----------------------------------------------------------------------------

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
facility_lookup <- data.table(
  facility_category = selected_facilities,
  facility = unname(facility_labels[selected_facilities]),
  walk_col = paste0(selected_facilities, "_walk_15min"),
  transit_col = paste0(selected_facilities, "_walk_transit_30min")
)

score_time <- function(x, threshold) {
  score <- 100 * (1 - pmin(x, threshold) / threshold)
  score[is.na(x)] <- 0
  pmax(score, 0)
}

access_summary <- rbindlist(lapply(seq_len(nrow(facility_lookup)), function(i) {
  z <- facility_lookup[i]
  walk_score <- score_time(wide[[z$walk_col]], 15)
  transit_score <- score_time(wide[[z$transit_col]], 30)
  data.table(
    facility_category = z$facility_category,
    facility = z$facility,
    scenario = c("WALK 15", "WALK + PT 30"),
    value = c(mean(walk_score, na.rm = TRUE), mean(transit_score, na.rm = TRUE))
  )
}))
access_summary[, scenario := factor(scenario, levels = c("WALK 15", "WALK + PT 30"))]
fwrite(access_summary, file.path(source_dir, "figure9_facility_access_means.csv"))

access_order <- access_summary[scenario == "WALK 15"][order(value)]$facility
access_summary[, facility := factor(facility, levels = access_order)]
eta9 <- copy(eta_long[facility_category %in% selected_facilities])
eta9[, facility := factor(facility, levels = access_order)]
eta9[, scenario := factor(scenario, levels = c("WALK 15", "WALK + PT 30"))]

# Build paired scenario endpoints explicitly.
build_endpoints <- function(dt, value_col) {
  a <- dt[scenario == "WALK 15", .(facility, walk = get(value_col))]
  b <- dt[scenario == "WALK + PT 30", .(facility, transit = get(value_col))]
  merge(a, b, by = "facility")
}

ends_access <- build_endpoints(access_summary, "value")
p9a <- ggplot() +
  geom_segment(data = ends_access, aes(x = walk, xend = transit, y = facility, yend = facility),
               linewidth = 0.55, colour = pal$rule) +
  geom_point(data = access_summary, aes(x = value, y = facility, fill = scenario),
             shape = 21, size = 2.2, stroke = 0.35, colour = pal$ink) +
  geom_text(data = access_summary,
            aes(x = value, y = facility, label = sprintf("%.1f", value),
                hjust = ifelse(scenario == "WALK 15", 1.18, -0.18)),
            size = 1.65, colour = pal$ink) +
  scale_fill_manual(values = scenario_cols) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0.03, 0.08))) +
  labs(title = "a  Mean facility accessibility", x = "Mean score (0-100)", y = NULL) +
  theme_paper(6.4) +
  theme(legend.position = "bottom")

ends_eta <- build_endpoints(eta9, "eta_sq_h")
p9b <- ggplot() +
  geom_segment(data = ends_eta, aes(x = walk, xend = transit, y = facility, yend = facility),
               linewidth = 0.55, colour = pal$rule) +
  geom_point(data = eta9, aes(x = eta_sq_h, y = facility, fill = scenario),
             shape = 21, size = 2.2, stroke = 0.35, colour = pal$ink) +
  geom_text(data = eta9,
            aes(x = eta_sq_h, y = facility, label = sprintf("%.3f", eta_sq_h),
                hjust = ifelse(scenario == "WALK 15", 1.16, -0.16)),
            size = 1.65, colour = pal$ink) +
  scale_fill_manual(values = scenario_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 0.075), breaks = c(0, 0.02, 0.04, 0.06),
                     expand = expansion(mult = c(0.03, 0.10))) +
  labs(title = expression("b  IMD association (" * eta[H]^2 * ")"),
       x = expression(eta[H]^2), y = NULL,
       caption = "BH-adjusted q values are supplied with the source data") +
  theme_paper(6.4) +
  theme(legend.position = "none")

draw9 <- function() draw_layout(list(p9a, p9b), matrix(c(1, 2), 1, 2), widths = c(1, 1.02))
save_page(draw9, "figure9_scenario_comparison_revised", 183, 124.4)

# -----------------------------------------------------------------------------
# Figure 10: means, borough-block bootstrap CIs and deterministic point samples.
# -----------------------------------------------------------------------------

gain <- fread(file.path(analysis_dir, "transit_gain_winners_losers_lsoa.csv"))
gain <- gain[transit_gain_group %in% c("Bottom 20% gain", "Top 20% gain")]
gain[, group := factor(
  fifelse(transit_gain_group == "Bottom 20% gain", "Bottom 20%", "Top 20%"),
  levels = c("Bottom 20%", "Top 20%")
)]
gain[, inner_london_pct := as.numeric(inner_london) * 100]

metric_map10 <- c(
  transit_gain_mean = "Scenario difference\n(score points)",
  walk_service_basket_mean = "Walking baseline\n(score)",
  IMD_Decile = "Mean IMD\ndecile",
  inner_london_pct = "Inner London\nshare (%)",
  care_mismatch_index = "Care mismatch\n(score)"
)

gain_long <- melt(
  gain,
  id.vars = c("LSOA21CD", "borough_name", "group"),
  measure.vars = names(metric_map10),
  variable.name = "metric_key",
  value.name = "value"
)
gain_long[, metric := factor(metric_map10[metric_key], levels = metric_map10)]

ci10 <- gain_long[, as.list(cluster_mean_ci(.SD, "value", reps = 1800L,
                                             seed = 20260805L + .GRP)),
                  by = .(group, metric_key, metric)]
fwrite(ci10, file.path(source_dir, "figure10_borough_block_bootstrap_ci.csv"))

setorder(gain_long, metric_key, group, LSOA21CD)
raw10 <- gain_long[metric_key != "inner_london_pct",
                   .SD[unique(round(seq(1, .N, length.out = min(.N, 120L))))],
                   by = .(metric_key, metric, group)]

p10 <- ggplot(ci10, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.58, alpha = 0.78, colour = pal$ink, linewidth = 0.28) +
  geom_point(data = raw10,
             aes(x = group, y = value, colour = group),
             position = position_jitter(width = 0.13, height = 0, seed = 20260805),
             alpha = 0.20, size = 0.48, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.16, linewidth = 0.42, colour = pal$ink) +
  geom_text(aes(label = sprintf("%.1f", mean)), vjust = -0.55,
            size = 2.0, colour = pal$ink) +
  facet_wrap(~ metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = gain_cols, guide = "none") +
  scale_colour_manual(values = c("Bottom 20%" = "#5B88B2", "Top 20%" = "#C66E78"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +
  labs(x = NULL, y = NULL,
       caption = "Bars = mean; whiskers = 95% borough-block bootstrap CI; points = deterministic LSOA subsample") +
  theme_paper(6.4) +
  theme(
    strip.text = element_text(size = 6.1, face = "bold"),
    axis.text.x = element_text(size = 5.4, lineheight = 0.90),
    panel.grid.major.y = element_line(colour = pal$pale_rule, linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 5.0, colour = pal$muted)
  )

draw10 <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  print(p10, vp = viewport(x = 0.5, y = 0.5, width = 0.99, height = 0.99))
}
save_page(draw10, "figure10_high_low_difference_revised", 183, 152.5)

# -----------------------------------------------------------------------------
# Figure 11: three-panel need gradient with borough-block bootstrap CIs.
# -----------------------------------------------------------------------------

gradient <- fread(file.path(analysis_dir, "vulnerability_typology_lsoa.csv"))
gradient <- gradient[complete.cases(family_care_demand_z, family_care_mismatch_z,
                                    family_care_access_score, borough_name)]
gradient[, quintile := cut(
  family_care_demand_z,
  breaks = quantile(family_care_demand_z, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = paste0("Q", 1:5)
)]
metric_map11 <- c(
  family_care_mismatch_z = "a  Need-access mismatch",
  family_care_access_score = "b  Walking accessibility",
  IMD_Decile = "c  Mean IMD decile"
)
gradient_long <- melt(
  gradient,
  id.vars = c("LSOA21CD", "borough_name", "quintile"),
  measure.vars = names(metric_map11),
  variable.name = "metric_key",
  value.name = "value"
)
gradient_long[, metric := factor(metric_map11[metric_key], levels = metric_map11)]
ci11 <- gradient_long[, as.list(cluster_mean_ci(.SD, "value", reps = 1800L,
                                                 seed = 20260821L + .GRP)),
                      by = .(quintile, metric_key, metric)]
metric_n11 <- gradient_long[is.finite(value), .(n = .N), by = .(quintile, metric_key, metric)]
ci11 <- merge(ci11, metric_n11, by = c("quintile", "metric_key", "metric"), all.x = TRUE)
ci11[, quintile := factor(quintile, levels = paste0("Q", 1:5))]
metric_cols11 <- c(
  "a  Need-access mismatch" = pal$child_dark,
  "b  Walking accessibility" = pal$care_dark,
  "c  Mean IMD decile" = pal$older_dark
)
baseline11 <- gradient_long[, .(baseline = mean(value, na.rm = TRUE)), by = .(metric_key, metric)]
fwrite(ci11, file.path(source_dir, "figure11_borough_block_bootstrap_ci.csv"))

p11 <- ggplot(ci11, aes(x = quintile, y = mean, group = 1, colour = metric, fill = metric)) +
  geom_hline(data = baseline11, aes(yintercept = baseline),
             inherit.aes = FALSE, linetype = "dashed", linewidth = 0.30, colour = pal$rule) +
  geom_ribbon(aes(ymin = lo, ymax = hi, group = 1), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.72) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.11, linewidth = 0.38) +
  geom_point(shape = 21, size = 2.25, stroke = 0.45, colour = pal$ink) +
  geom_text(aes(label = sprintf("%.2f\nn=%s", mean, format(n, big.mark = ","))),
            vjust = -0.72, size = 1.50, lineheight = 0.88, colour = pal$ink) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = metric_cols11, guide = "none") +
  scale_fill_manual(values = metric_cols11, guide = "none") +
  scale_x_discrete(labels = paste0("Q", 1:5)) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.18))) +
  labs(x = "Caregiving-need proxy quintile", y = NULL,
       caption = "Bands and whiskers = 95% borough-block bootstrap CI; dashed line = London mean") +
  theme_paper(6.4) +
  theme(
    axis.text.x = element_text(size = 5.0, lineheight = 0.90),
    panel.grid.major.y = element_line(colour = pal$pale_rule, linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 5.0, colour = pal$muted)
  )

draw11 <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  print(p11, vp = viewport(x = 0.5, y = 0.5, width = 0.99, height = 0.99))
}
save_page(draw11, "figure11_care_need_gradient_revised", 183, 122)

# -----------------------------------------------------------------------------
# Appendix candidate S1: weight-sensitivity distributions (iteration order is
# deliberately not encoded as a time/trend axis).
# -----------------------------------------------------------------------------

sens <- fread(file.path(analysis_dir, "service_basket_weight_sensitivity_random_iterations.csv"))
sens[, group := factor(group,
                       levels = c("Children and Adolescents", "Caregiving Adults", "Older adults"),
                       labels = c("Children", "Caregiving adults", "Older adults"))]
sens_long <- melt(
  sens,
  id.vars = c("group", "iteration"),
  measure.vars = c("spearman_rank_correlation", "top20_mismatch_overlap_pct"),
  variable.name = "metric_key",
  value.name = "value"
)
sens_long[, metric := factor(
  metric_key,
  levels = c("spearman_rank_correlation", "top20_mismatch_overlap_pct"),
  labels = c("a  Spearman rank correlation", "b  Top-20% mismatch overlap (%)")
)]
sens_stats <- sens_long[, .(
  median = median(value, na.rm = TRUE),
  p05 = quantile(value, 0.05, na.rm = TRUE),
  p95 = quantile(value, 0.95, na.rm = TRUE)
), by = .(group, metric)]
fwrite(sens_stats, file.path(source_dir, "appendix_candidate_weight_sensitivity_summary.csv"))

p_s1 <- ggplot(sens_long, aes(x = group, y = value, fill = group)) +
  geom_violin(width = 0.82, trim = FALSE, alpha = 0.72, colour = pal$ink, linewidth = 0.25) +
  geom_errorbar(data = sens_stats, aes(x = group, ymin = p05, ymax = p95), width = 0.12,
                linewidth = 0.40, colour = pal$ink, inherit.aes = FALSE) +
  geom_point(data = sens_stats, aes(x = group, y = median), shape = 21, size = 2.0,
             fill = "white", colour = pal$ink, inherit.aes = FALSE) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = group_cols, guide = "none") +
  labs(x = NULL, y = NULL,
       caption = "White point = median; whiskers = 5th-95th percentiles across 1,000 random weights per group") +
  theme_paper(6.5) +
  theme(axis.text.x = element_text(size = 5.8),
        plot.caption = element_text(size = 5.0, colour = pal$muted))

draw_s1 <- function() {
  grid.newpage(); grid.rect(gp = gpar(fill = "white", col = NA))
  print(p_s1, vp = viewport(width = 0.98, height = 0.98))
}
save_page(draw_s1, "appendix_candidate_weight_sensitivity", 183, 92)

# -----------------------------------------------------------------------------
# Appendix candidate S2: centroid-grid agreement for caregiving adults.
# -----------------------------------------------------------------------------

cg <- fread(file.path(extension_table_dir, "centroid_grid_comparison.csv"))
cg <- cg[group == "caregiving_adults" & mode %in% c("walk_15min", "walk_transit_30min")]
cg[, scenario := factor(mode,
                        levels = c("walk_15min", "walk_transit_30min"),
                        labels = c("a  WALK 15", "b  WALK + PT 30"))]
cg_stats <- cg[, .(
  n = .N,
  pearson_r = cor(centroid_score, grid_informed_score, use = "complete.obs", method = "pearson"),
  spearman_rho = cor(centroid_score, grid_informed_score, use = "complete.obs", method = "spearman"),
  mae = mean(abs(centroid_score - grid_informed_score), na.rm = TRUE)
), by = scenario]
fwrite(cg_stats, file.path(source_dir, "appendix_candidate_centroid_grid_statistics.csv"))
label_s2 <- cg_stats[, .(
  scenario,
  x = 4,
  y = 96,
  label = sprintf("n = %s\nPearson r = %.3f\nSpearman rho = %.3f\nMAE = %.2f",
                  format(n, big.mark = ","), pearson_r, spearman_rho, mae)
)]

p_s2 <- ggplot(cg, aes(x = centroid_score, y = grid_informed_score)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.35, colour = pal$muted) +
  geom_point(colour = pal$care_dark, alpha = 0.20, size = 0.42, stroke = 0) +
  geom_text(data = label_s2, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 1.9,
            lineheight = 0.95, colour = pal$ink) +
  facet_wrap(~ scenario, nrow = 1) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  labs(x = "Centroid-based accessibility score", y = "Residential-grid-informed score",
       caption = "Dashed line indicates exact agreement") +
  theme_paper(6.5) +
  theme(plot.caption = element_text(size = 5.0, colour = pal$muted))

draw_s2 <- function() {
  grid.newpage(); grid.rect(gp = gpar(fill = "white", col = NA))
  print(p_s2, vp = viewport(width = 0.98, height = 0.98))
}
save_page(draw_s2, "appendix_candidate_centroid_grid_agreement", 183, 96)

message("Revised non-map figures written to: ", out_dir)
