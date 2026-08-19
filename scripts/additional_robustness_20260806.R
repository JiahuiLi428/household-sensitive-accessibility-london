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
log_con <- file(log_path, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

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

fwrite(pt15_lsoa, file.path(analysis_out, "pt15_threshold_matched_lsoa_scores.csv"))
fwrite(scenario_summary, file.path(analysis_out, "pt15_scenario_summary.csv"))
fwrite(decomposition, file.path(analysis_out, "pt15_mode_threshold_decomposition.csv"))

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
fwrite(eta_long, file.path(analysis_out, "figure9_imd_effect_sizes_source.csv"))

# -----------------------------------------------------------------------------
# 2. Missing-IMD selection audit
# -----------------------------------------------------------------------------

enriched <- fread(rel("data", "output", "analysis", "r5r_accessibility_enriched_wide.csv"))
care <- fread(rel("data", "output", "analysis", "care_mobility_accessibility_lsoa.csv"))

imd_audit <- merge(
  enriched[, .(
    LSOA21CD,
    LSOA21NM,
    population_total,
    children_pct,
    older_adults_pct,
    IMD_Source
  )],
  care[, .(
    LSOA21CD,
    borough_name,
    london_ring,
    caregiver_household_intensity,
    care_need_score,
    care_walk_index,
    care_transit_index
  )],
  by = "LSOA21CD",
  all.x = TRUE
)
imd_audit <- merge(
  imd_audit,
  indices[, .(LSOA21CD, children_walk_index, older_walk_index)],
  by = "LSOA21CD",
  all.x = TRUE
)
imd_audit[, imd_status := fifelse(IMD_Source == "missing_imd2019_for_lsoa21", "Missing IMD", "Matched IMD")]

audit_vars <- c(
  children_pct = "Children aged 0-15 (%)",
  older_adults_pct = "Adults aged 65+ (%)",
  caregiver_household_intensity = "Dependent-child households (%)",
  care_need_score = "Caregiving need score",
  care_walk_index = "Caregiving WALK 15",
  care_transit_index = "Caregiving PT 30",
  children_walk_index = "Children WALK 15",
  older_walk_index = "Older-adult WALK 15"
)

imd_bias_summary <- rbindlist(lapply(names(audit_vars), function(variable) {
  missing_values <- imd_audit[imd_status == "Missing IMD"][[variable]]
  matched_values <- imd_audit[imd_status == "Matched IMD"][[variable]]
  n_missing <- sum(is.finite(missing_values))
  n_matched <- sum(is.finite(matched_values))
  pooled_sd <- sqrt(
    ((n_missing - 1) * var(missing_values, na.rm = TRUE) +
       (n_matched - 1) * var(matched_values, na.rm = TRUE)) /
      (n_missing + n_matched - 2)
  )
  test <- wilcox.test(missing_values, matched_values, exact = FALSE)
  data.table(
    variable,
    label = audit_vars[[variable]],
    matched_n = n_matched,
    missing_n = n_missing,
    matched_mean = mean(matched_values, na.rm = TRUE),
    missing_mean = mean(missing_values, na.rm = TRUE),
    mean_difference_missing_minus_matched = mean(missing_values, na.rm = TRUE) - mean(matched_values, na.rm = TRUE),
    standardized_mean_difference = (mean(missing_values, na.rm = TRUE) - mean(matched_values, na.rm = TRUE)) / pooled_sd,
    wilcoxon_p = test$p.value
  )
}))

imd_borough <- imd_audit[imd_status == "Missing IMD", .(
  missing_lsoas = .N,
  mean_population = mean(population_total, na.rm = TRUE),
  median_population = median(population_total, na.rm = TRUE)
), by = .(borough_name, london_ring)][order(-missing_lsoas)]

fwrite(imd_audit, file.path(analysis_out, "imd_missingness_lsoa_audit.csv"))
fwrite(imd_bias_summary, file.path(analysis_out, "imd_missingness_bias_summary.csv"))
fwrite(imd_borough, file.path(analysis_out, "imd_missingness_borough_distribution.csv"))

lsoas <- st_make_valid(st_read(rel("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE))
lsoa_audit_map <- merge(lsoas, imd_audit[, .(LSOA21CD, imd_status)], by = "LSOA21CD", all.x = TRUE)
boroughs <- aggregate(st_geometry(lsoa_audit_map), by = list(borough_name = sub(" [0-9]{3}[A-Z]$", "", lsoa_audit_map$LSOA21NM)), FUN = st_union)
boroughs <- st_sf(borough_name = boroughs$borough_name, geometry = st_sfc(boroughs$x, crs = st_crs(lsoa_audit_map)))

pca <- ggplot() +
  geom_sf(data = lsoa_audit_map, aes(fill = imd_status), colour = NA) +
  geom_sf(data = boroughs, fill = NA, colour = "white", linewidth = 0.18) +
  scale_fill_manual(values = c("Matched IMD" = "#E5E5E5", "Missing IMD" = palette[["accent"]]), drop = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "a  Spatial distribution of\nunmatched IMD records",
    subtitle = "181 of 4,994 LSOAs lack an IMD2019 match\nafter direct and best-fit linkage"
  ) +
  theme_void(base_family = "Arial", base_size = 8.5) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 9.3),
    plot.subtitle = element_text(size = 8.2, colour = "#555555"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

imd_bias_summary[, label := factor(label, levels = rev(label))]
pcb <- ggplot(imd_bias_summary, aes(x = standardized_mean_difference, y = label)) +
  geom_vline(xintercept = 0, colour = "#555555", linewidth = 0.35) +
  geom_vline(xintercept = c(-0.5, 0.5), colour = "#BDBDBD", linewidth = 0.35, linetype = "dashed") +
  geom_segment(aes(x = 0, xend = standardized_mean_difference, yend = label), colour = "#A6A6A6", linewidth = 0.55) +
  geom_point(aes(fill = standardized_mean_difference > 0), shape = 21, size = 2.5, colour = "#333333", stroke = 0.35) +
  geom_text(
    aes(label = sprintf("%+.2f", standardized_mean_difference)),
    hjust = ifelse(imd_bias_summary$standardized_mean_difference >= 0, -0.18, 1.18),
    size = 2.5
  ) +
  scale_fill_manual(values = c(`TRUE` = palette[["pt15"]], `FALSE` = palette[["accent"]]), guide = "none") +
  scale_x_continuous(limits = c(-1.75, 0.75), breaks = seq(-1.5, 0.5, 0.5)) +
  labs(
    title = "b  Difference between unmatched\nand matched LSOAs",
    subtitle = "Standardized mean difference; positive values\nare higher in the unmatched group",
    x = "Standardized mean difference",
    y = NULL
  ) +
  theme_pub() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

save_two_panel(
  pca,
  pcb,
  file.path(figure_out, "appendix_figure_C1_imd_missingness_audit"),
  width_mm = 183,
  height_mm = 102,
  widths = c(1, 1.05)
)

# -----------------------------------------------------------------------------
# 3. Group-specific grid-informed Local Moran permutation + BH-FDR sensitivity
# -----------------------------------------------------------------------------

grid_mismatch <- fread(rel("extension", "outputs", "tables", "grid_informed_mismatch.csv"))
compound_analytical <- fread(rel("extension", "outputs", "tables", "compound_mismatch.csv"))

classify_clusters <- function(x, lag_x, q) {
  centered <- x - mean(x, na.rm = TRUE)
  cluster <- rep("Not significant", length(x))
  sig <- !is.na(q) & q < 0.05
  cluster[sig & centered > 0 & lag_x > 0] <- "High-high mismatch cluster"
  cluster[sig & centered < 0 & lag_x < 0] <- "Low-low surplus cluster"
  cluster[sig & centered > 0 & lag_x < 0] <- "High-low spatial outlier"
  cluster[sig & centered < 0 & lag_x > 0] <- "Low-high spatial outlier"
  cluster
}

spdep::set.coresOption(1L)
groups <- c("children_and_adolescents", "caregiving_adults", "older_adults")
fdr_results <- list()

for (group_name in groups) {
  cache_path <- file.path(analysis_out, paste0("grid_lisa_fdr_", group_name, ".csv"))
  if (file.exists(cache_path)) {
    message("Using cached grid FDR result: ", group_name)
    fdr_results[[group_name]] <- fread(cache_path)
    next
  }

  message("Running 9,999 grid permutations for: ", group_name, " at ", Sys.time())
  dt <- grid_mismatch[group == group_name]
  g <- merge(lsoas, dt, by = "LSOA21CD", all.x = FALSE, all.y = FALSE)
  if (!"LSOA21NM" %in% names(g)) {
    g$LSOA21NM <- if ("LSOA21NM.x" %in% names(g)) g$LSOA21NM.x else g$LSOA21NM.y
  }
  g <- g[!is.na(g$grid_mean_mismatch), ]
  g <- st_make_valid(g)
  nb <- poly2nb(g, queen = TRUE)
  listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  x <- g$grid_mean_mismatch
  centered <- x - mean(x, na.rm = TRUE)
  lag_x <- lag.listw(listw, centered, zero.policy = TRUE)
  local_perm <- localmoran_perm(
    x,
    listw,
    nsim = 9999L,
    zero.policy = TRUE,
    alternative = "two.sided",
    iseed = 20260805L
  )
  if (!"Pr(folded) Sim" %in% colnames(local_perm)) {
    stop("Permutation p-value column not found for ", group_name)
  }
  p_perm <- as.numeric(local_perm[, "Pr(folded) Sim"])
  q_bh <- p.adjust(p_perm, method = "BH")
  cluster_fdr <- classify_clusters(x, lag_x, q_bh)

  result <- as.data.table(st_drop_geometry(g))[, .(
    LSOA21CD,
    LSOA21NM,
    borough_name,
    group,
    grid_mean_mismatch,
    grid_mean_lisa_cluster_analytical = grid_mean_lisa_cluster,
    grid_local_moran_i_permutation = as.numeric(local_perm[, "Ii"]),
    grid_local_moran_p_permutation = p_perm,
    grid_local_moran_q_bh = q_bh,
    grid_mean_lisa_cluster_fdr = cluster_fdr
  )]
  fwrite(result, cache_path)
  fdr_results[[group_name]] <- result
  message("Completed: ", group_name, " at ", Sys.time())
}

grid_fdr <- rbindlist(fdr_results, use.names = TRUE)
grid_fdr[, analytical_high_high := grid_mean_lisa_cluster_analytical == "High-high mismatch cluster"]
grid_fdr[, fdr_high_high := grid_mean_lisa_cluster_fdr == "High-high mismatch cluster"]

grid_fdr_summary <- grid_fdr[, .(
  n_lsoas = .N,
  analytical_high_high = sum(analytical_high_high),
  fdr_high_high = sum(fdr_high_high),
  analytical_high_high_retained_fdr = sum(analytical_high_high & fdr_high_high),
  retention_pct = 100 * sum(analytical_high_high & fdr_high_high) / sum(analytical_high_high),
  analytical_fdr_high_high_jaccard = sum(analytical_high_high & fdr_high_high) / sum(analytical_high_high | fdr_high_high)
), by = group]

fdr_flags <- dcast(
  grid_fdr[, .(LSOA21CD, group, fdr_high_high = as.integer(fdr_high_high))],
  LSOA21CD ~ group,
  value.var = "fdr_high_high",
  fill = 0
)
fdr_flags[, compound_count_fdr := rowSums(.SD), .SDcols = groups]
fdr_compound_summary <- fdr_flags[, .(n_lsoas = .N), by = compound_count_fdr][order(compound_count_fdr)]
fdr_compound <- merge(
  compound_analytical,
  fdr_flags,
  by = "LSOA21CD",
  all.x = TRUE
)

fwrite(grid_fdr, file.path(analysis_out, "grid_lisa_fdr_all_groups.csv"))
fwrite(grid_fdr_summary, file.path(analysis_out, "grid_lisa_fdr_summary.csv"))
fwrite(fdr_compound, file.path(analysis_out, "compound_mismatch_fdr_sensitivity.csv"))
fwrite(fdr_compound_summary, file.path(analysis_out, "compound_mismatch_fdr_summary.csv"))

care_fdr <- grid_fdr[group == "caregiving_adults"]
care_map <- merge(lsoas, care_fdr, by = "LSOA21CD", all.x = FALSE, all.y = FALSE)
care_map <- merge(
  care_map,
  grid_mismatch[group == "caregiving_adults", .(LSOA21CD, lisa_reclassification)],
  by = "LSOA21CD",
  all.x = TRUE
)

reclass_palette <- c(
  "stable high-high" = "#8C2D04",
  "new high-high" = "#D95F0E",
  "no longer high-high" = "#2B8CBE",
  "never high-high" = "#E5E5E5"
)
cluster_palette <- c(
  "High-high mismatch cluster" = "#B2182B",
  "Low-low surplus cluster" = "#2166AC",
  "High-low spatial outlier" = "#EF8A62",
  "Low-high spatial outlier" = "#67A9CF",
  "Not significant" = "#E5E5E5"
)

pd5a <- ggplot() +
  geom_sf(data = care_map, aes(fill = lisa_reclassification), colour = NA) +
  geom_sf(data = boroughs, fill = NA, colour = "white", linewidth = 0.18) +
  scale_fill_manual(values = reclass_palette, drop = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "a  Origin sensitivity under\nanalytical p < 0.05",
    subtitle = "Centroid-to-grid reclassification of\ncaregiving high-high clusters"
  ) +
  theme_void(base_family = "Arial", base_size = 8.3) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 5.8),
    legend.key.width = unit(3.2, "mm"),
    legend.key.height = unit(3.0, "mm"),
    plot.title = element_text(face = "bold", size = 9.1),
    plot.subtitle = element_text(size = 7.8, colour = "#555555"),
    plot.background = element_rect(fill = "white", colour = NA)
  ) + guides(fill = guide_legend(nrow = 2, byrow = TRUE))

pd5b <- ggplot() +
  geom_sf(data = care_map, aes(fill = grid_mean_lisa_cluster_fdr), colour = NA) +
  geom_sf(data = boroughs, fill = NA, colour = "white", linewidth = 0.18) +
  scale_fill_manual(values = cluster_palette, drop = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "b  Grid-informed clusters after\npermutation and BH-FDR",
    subtitle = "9,999 conditional permutations;\nq < 0.05; seed 20260805"
  ) +
  theme_void(base_family = "Arial", base_size = 8.3) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 5.8),
    legend.key.width = unit(3.2, "mm"),
    legend.key.height = unit(3.0, "mm"),
    plot.title = element_text(face = "bold", size = 9.1),
    plot.subtitle = element_text(size = 7.8, colour = "#555555"),
    plot.background = element_rect(fill = "white", colour = NA)
  ) + guides(fill = guide_legend(nrow = 3, byrow = TRUE))

save_two_panel(
  pd5a,
  pd5b,
  file.path(figure_out, "figure_D5_lisa_reclassification_fdr_revised"),
  width_mm = 183,
  height_mm = 118,
  widths = c(1, 1)
)

message("PT15 decomposition:")
print(decomposition)
message("IMD missingness audit:")
print(imd_bias_summary)
message("Grid FDR summary:")
print(grid_fdr_summary)
message("FDR compound summary:")
print(fdr_compound_summary)
message("Additional robustness analysis completed: ", Sys.time())
