# Figure-9-only runner, generated 2026-08-19.
# Reproduces figures/publication_additional_20260806/figure9_scenario_decomposition_revised.*
# and NOTHING else: the log sink, the LISA permutations and every fwrite() to
# data/output/analysis/additional_robustness_20260806/ have been removed.
# Requires ggplot2 >= 3.5 (uses legend.position.inside).

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(spdep)
  library(ggplot2)
  library(grid)
})

options(scipen = 999)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "UCLdissertation.Rproj"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate project root")
    current <- parent
  }
}

project_root <- find_project_root()
rel <- function(...) file.path(project_root, ...)
source(rel("scripts", "model", "00_config.R"))

analysis_out <- rel("data", "output", "analysis", "additional_robustness_20260806")
figure_out <- rel("figures", "publication_additional_20260806")
dir.create(analysis_out, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_out, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(analysis_out, "additional_robustness_20260806.log")
# sink disabled: figure-only run



message("Additional robustness analysis started: ", Sys.time())

palette <- c(
  children = "#4C78A8",
  caregiving = "#D9796A",
  older = "#6F63A8",
  walk = "#7A9CB8",
  pt15 = "#4B9388",
  pt30 = "#D9796A",
  neutral = "#D9D9D9",
  dark = "#2B2B2B",
  accent = "#B33A3A"
)

theme_pub <- function(base_size = 8.5) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      axis.text = element_text(colour = "#333333"),
      axis.title = element_text(colour = "#222222"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 0.5),
      strip.text = element_text(face = "bold", size = base_size),
      plot.title = element_text(face = "bold", size = base_size + 0.8),
      plot.subtitle = element_text(size = base_size - 0.3, colour = "#555555"),
      plot.caption = element_text(size = base_size - 1.2, colour = "#666666", hjust = 0),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

draw_two <- function(left, right, widths = c(1, 1)) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2, widths = unit(widths, "null"))))
  print(left, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(right, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  upViewport()
}

save_two_panel <- function(left, right, stem, width_mm = 183, height_mm = 105, widths = c(1, 1)) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  ragg::agg_png(paste0(stem, ".png"), width = width_in, height = height_in, units = "in", res = 600)
  draw_two(left, right, widths)
  dev.off()

  svglite::svglite(paste0(stem, ".svg"), width = width_in, height = height_in)
  draw_two(left, right, widths)
  dev.off()

  grDevices::cairo_pdf(paste0(stem, ".pdf"), width = width_in, height = height_in, family = "Arial")
  draw_two(left, right, widths)
  dev.off()

  ragg::agg_tiff(paste0(stem, ".tiff"), width = width_in, height = height_in, units = "in", res = 600)
  draw_two(left, right, widths)
  dev.off()
}

# -----------------------------------------------------------------------------
# 1. Threshold-matched WALK + PT 15 sensitivity from stored PT 30 nearest times
# -----------------------------------------------------------------------------

r5_wide <- fread(rel("data", "output", "r5r", "nearest_facility_times_full_wide.csv"))
indices <- fread(rel("data", "output", "analysis", "composite_service_basket_accessibility_indices.csv"))

pt30_time_cols <- c(
  school = "education_schools_walk_transit_30min",
  supermarket = "food_retail_supermarkets_walk_transit_30min",
  park = "green_space_parks_walk_transit_30min",
  gp = "healthcare_gps_walk_transit_30min",
  hospital = "healthcare_hospitals_walk_transit_30min",
  pharmacy = "healthcare_pharmacies_walk_transit_30min",
  bus_stop = "transport_bus_stops_walk_transit_30min",
  underground = "transport_underground_metro_stations_walk_transit_30min"
)

score_at_15 <- function(x) {
  score <- 100 * (1 - pmin(x, 15) / 15)
  score[is.na(score)] <- 0
  pmax(score, 0)
}

pt15_facility_scores <- r5_wide[, .(LSOA21CD, LSOA21NM)]
for (facility in names(pt30_time_cols)) {
  pt15_facility_scores[, (facility) := score_at_15(r5_wide[[pt30_time_cols[[facility]]]])]
}

weighted_score <- function(dt, weights) {
  score_columns <- names(weights)
  as.numeric(as.matrix(dt[, ..score_columns]) %*% weights)
}

pt15_facility_scores[, children_pt15_index := weighted_score(.SD, service_basket_weights$family)]
pt15_facility_scores[, caregiving_pt15_index := weighted_score(.SD, service_basket_weights$working)]
pt15_facility_scores[, older_pt15_index := weighted_score(.SD, service_basket_weights$older)]

pt15_lsoa <- merge(
  indices,
  pt15_facility_scores[, .(LSOA21CD, children_pt15_index, caregiving_pt15_index, older_pt15_index)],
  by = "LSOA21CD",
  all.x = TRUE
)

scenario_vectors <- list(
  "Children and adolescents" = list(
    "WALK 15" = pt15_lsoa$children_walk_index,
    "WALK + PT 15" = pt15_lsoa$children_pt15_index,
    "WALK + PT 30" = pt15_lsoa$children_transit_index
  ),
  "Caregiving adults" = list(
    "WALK 15" = pt15_lsoa$working_walk_index,
    "WALK + PT 15" = pt15_lsoa$caregiving_pt15_index,
    "WALK + PT 30" = pt15_lsoa$working_transit_index
  ),
  "Older adults" = list(
    "WALK 15" = pt15_lsoa$older_walk_index,
    "WALK + PT 15" = pt15_lsoa$older_pt15_index,
    "WALK + PT 30" = pt15_lsoa$older_transit_index
  )
)

scenario_summary <- rbindlist(lapply(names(scenario_vectors), function(group_name) {
  rbindlist(lapply(names(scenario_vectors[[group_name]]), function(scenario_name) {
    values <- scenario_vectors[[group_name]][[scenario_name]]
    data.table(
      group = group_name,
      scenario = scenario_name,
      n = sum(is.finite(values)),
      mean = mean(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      q1 = quantile(values, 0.25, na.rm = TRUE),
      q3 = quantile(values, 0.75, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE)
    )
  }))
}))

decomposition <- dcast(scenario_summary, group ~ scenario, value.var = "mean")
decomposition[, mode_gain_same_15 := `WALK + PT 15` - `WALK 15`]
decomposition[, threshold_gain_same_mode := `WALK + PT 30` - `WALK + PT 15`]
decomposition[, total_endpoint_difference := `WALK + PT 30` - `WALK 15`]
decomposition[, mode_share_pct := 100 * mode_gain_same_15 / total_endpoint_difference]


scenario_summary[, scenario := factor(scenario, levels = c("WALK 15", "WALK + PT 15", "WALK + PT 30"))]
scenario_summary[, group := factor(group, levels = c("Children and adolescents", "Caregiving adults", "Older adults"))]
group_cols <- c(
  "Children and adolescents" = palette[["children"]],
  "Caregiving adults" = palette[["caregiving"]],
  "Older adults" = palette[["older"]]
)

p9a <- ggplot(scenario_summary, aes(x = scenario, y = mean, group = group, colour = group)) +
  geom_line(linewidth = 0.7) +
  geom_linerange(aes(ymin = q1, ymax = q3), linewidth = 0.45, alpha = 0.75) +
  geom_point(size = 2.2) +
  annotate(
    "label", x = 1.55, y = 72,
    label = "PT gain at 15 min: +0.5 to +0.7\nThreshold gain to 30 min: +23.4 to +25.1",
    size = 2.25, hjust = 0, label.size = 0.2, colour = "#333333", fill = "white"
  ) +
  scale_colour_manual(values = group_cols) +
  scale_y_continuous(limits = c(0, 82), breaks = seq(0, 80, 20), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "a  Threshold-matched\nscenario decomposition",
    subtitle = "Means and interquartile ranges\n(n = 4,994 LSOAs)",
    x = NULL,
    y = "Service-basket accessibility (0-100)"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 18, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.03, 0.05),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.background = element_rect(fill = scales::alpha("white", 0.88), colour = NA),
    legend.key.height = unit(3.5, "mm"),
    legend.text = element_text(size = 6.8)
  )

eta_wide <- fread(rel("data", "output", "analysis", "walk_vs_transit_imd_inequality_effect_sizes.csv"))
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
eta_long[, significant_bh := q_bh_16 < 0.05]
eta_long[, facility := unname(facility_labels[facility_category])]
eta_long[, scenario := factor(scenario, levels = c("WALK 15", "WALK + PT 30"))]
eta_order <- eta_long[scenario == "WALK 15"][order(eta_sq_h)]$facility
eta_long[, facility := factor(facility, levels = eta_order)]
eta_segments <- dcast(eta_long, facility ~ scenario, value.var = "eta_sq_h")

p9b <- ggplot() +
  geom_segment(
    data = eta_segments,
    aes(x = `WALK 15`, xend = `WALK + PT 30`, y = facility, yend = facility),
    colour = "#BDBDBD", linewidth = 0.55
  ) +
  geom_point(
    data = eta_long,
    aes(x = eta_sq_h, y = facility, fill = scenario, shape = significant_bh),
    size = 2.2, colour = "#333333", stroke = 0.35
  ) +
  scale_fill_manual(values = c("WALK 15" = palette[["walk"]], "WALK + PT 30" = palette[["pt30"]])) +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 1), breaks = c(FALSE, TRUE),
                     labels = c(`TRUE` = "q < 0.05", `FALSE` = "q >= 0.05")) +
  scale_x_continuous(limits = c(0, max(eta_long$eta_sq_h) * 1.12), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "b  IMD association across\nregistered benchmarks",
    subtitle = "Kruskal-Wallis eta-squared; BH q-values\nuse the prespecified 16-test family",
    x = expression(eta[H]^2),
    y = NULL
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE,
                        override.aes = list(shape = 21, size = 2.6)),
    shape = guide_legend(nrow = 1, byrow = TRUE,
                         override.aes = list(fill = c(NA, "#666666"),
                                             colour = "#333333", size = 2.6))
  ) +
  theme_pub() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 6.6),
    legend.key.width = unit(3.8, "mm"),
    legend.key.height = unit(3.4, "mm")
  )

save_two_panel(
  p9a,
  p9b,
  file.path(figure_out, "figure9_scenario_decomposition_revised"),
  width_mm = 183,
  height_mm = 115,
  widths = c(1.08, 0.92)
)
# (source-table fwrite removed: this runner only produces the figure)