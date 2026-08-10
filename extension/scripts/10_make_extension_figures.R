suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(ggplot2)
  library(scales)
})

set.seed(20260716)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, "UCLdissertation.Rproj")) ||
        (dir.exists(file.path(current, "data")) && dir.exists(file.path(current, "scripts")))
    ) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate project root from: ", start)
    current <- parent
  }
}

project_root <- find_project_root()
rel_path <- function(...) file.path(project_root, ...)

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
table_dir <- file.path(extension_dir, "outputs", "tables")
map_dir <- file.path(extension_dir, "outputs", "maps")
case_dir <- file.path(extension_dir, "outputs", "case_studies")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("10_make_extension_figures_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("10_make_extension_figures.R started at: ", started_at)

required_file <- function(path) {
  if (!file.exists(path)) stop("Required input does not exist: ", path)
  invisible(path)
}

add_decorations <- function(p, bbox, scalebar_m = 10000) {
  dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
  dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
  bbox[c("xmin", "xmax")] <- bbox[c("xmin", "xmax")] + c(-1, 1) * 0.03 * dx
  bbox[c("ymin", "ymax")] <- bbox[c("ymin", "ymax")] + c(-1, 1) * 0.03 * dy
  dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
  dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
  x0 <- as.numeric(bbox["xmin"] + 0.055 * dx)
  y0 <- as.numeric(bbox["ymin"] + 0.055 * dy)
  nx <- as.numeric(bbox["xmax"] - 0.065 * dx)
  ny <- as.numeric(bbox["ymax"] - 0.17 * dy)
  nh <- 0.115 * dy
  aw <- 0.027 * dx
  left <- data.frame(x = c(nx, nx - aw, nx, nx), y = c(ny + nh, ny - 0.32 * nh, ny, ny + nh))
  right <- data.frame(x = c(nx, nx + aw, nx, nx), y = c(ny + nh, ny - 0.32 * nh, ny, ny + nh))
  p +
    annotate("segment", x = x0, xend = x0 + scalebar_m / 2, y = y0, yend = y0, linewidth = 1.05, colour = "#111111") +
    annotate("segment", x = x0 + scalebar_m / 2, xend = x0 + scalebar_m, y = y0, yend = y0, linewidth = 1.05, colour = "white") +
    annotate("segment", x = x0, xend = x0 + scalebar_m, y = y0, yend = y0, linewidth = 0.18, colour = "#111111") +
    annotate("text", x = x0, y = y0 + 0.025 * dy, label = "0", size = 2.2, hjust = 0) +
    annotate("text", x = x0 + scalebar_m, y = y0 + 0.025 * dy, label = paste0(scalebar_m / 1000, " km"), size = 2.2, hjust = 1) +
    geom_polygon(data = left, aes(x, y), inherit.aes = FALSE, fill = "#111111", colour = "#111111", linewidth = 0.28) +
    geom_polygon(data = right, aes(x, y), inherit.aes = FALSE, fill = "white", colour = "#111111", linewidth = 0.28) +
    annotate("text", x = nx, y = ny + nh + 0.032 * dy, label = "N", fontface = "bold", size = 2.8) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      crs = 27700, datum = 4326, expand = FALSE
    )
}

save_fig <- function(plot, stem, width = 7.2, height = 6.7) {
  ggsave(file.path(map_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 450)
  ggsave(file.path(map_dir, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(map_dir, paste0(stem, ".svg")), plot, width = width, height = height, device = svglite::svglite)
  ggsave(file.path(map_dir, paste0(stem, ".tiff")), plot, width = width, height = height,
         dpi = 600, device = ragg::agg_tiff, compression = "lzw")
}

lsoas <- st_read(required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg")), quiet = TRUE)
thames <- st_transform(st_read(required_file(rel_path("data", "boundaries", "river_thames_osm.geojson")), quiet = TRUE), st_crs(lsoas))
comparison <- fread(required_file(file.path(table_dir, "centroid_grid_comparison.csv")))
stats <- fread(required_file(file.path(table_dir, "centroid_grid_statistics.csv")))
agg <- fread(required_file(file.path(table_dir, "lsoa_grid_aggregated_accessibility.csv")))
mismatch <- fread(required_file(file.path(table_dir, "grid_informed_mismatch.csv")))
lisa <- fread(required_file(file.path(table_dir, "lisa_comparison.csv")))
compound <- fread(required_file(file.path(table_dir, "compound_mismatch.csv")))
compound_borough <- fread(required_file(file.path(table_dir, "compound_mismatch_by_borough.csv")))
case_profiles <- fread(required_file(file.path(case_dir, "case_study_profiles_combined.csv")))
grid <- st_read(required_file(file.path(intermediate_dir, "london_grid_100m_residential.gpkg")), quiet = TRUE)
grid_scores <- as.data.table(arrow::read_parquet(required_file(file.path(intermediate_dir, "grid_accessibility_summary.parquet"))))

lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
boroughs <- aggregate(lsoas[, "borough_name"], by = list(borough_name_group = lsoas$borough_name), FUN = function(x) x[1])
london_bbox <- st_bbox(lsoas)

map_theme <- theme_void(base_size = 9, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 12, face = "bold", colour = "#222222"),
    plot.subtitle = element_text(size = 9, colour = "#555555"),
    panel.border = element_rect(fill = NA, colour = "#222222", linewidth = 0.38),
    panel.grid.major = element_line(colour = "#D6D6D2", linewidth = 0.18, linetype = 3),
    axis.text = element_text(size = 7, colour = "#222222"),
    axis.ticks = element_line(colour = "#222222", linewidth = 0.28),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8.2),
    legend.key.height = unit(0.28, "cm"),
    legend.key.width = unit(0.62, "cm"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = "#A7A7A1", linewidth = 0.22),
    legend.box.margin = margin(2, 3, 2, 3),
    plot.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(face = "bold", size = 9)
  )

# Figure D1: workflow diagram
workflow <- data.table(
  x = c(1, 2.7, 4.4, 6.1, 7.8, 9.5),
  y = 1,
  label = c("LSOA baseline", "100m grid", "OSM residential filter", "Grid R5r access", "Mismatch + LISA", "Cases + figures")
)
e1 <- ggplot(workflow, aes(x, y)) +
  geom_segment(data = workflow[-.N], aes(x = x + 0.45, xend = x + 1.25, y = y, yend = y),
               arrow = arrow(length = unit(0.18, "cm")), colour = "#5F6B6D") +
  geom_label(aes(label = label), fill = "#F4F1EA", colour = "#222222", label.size = 0.25, size = 3.1) +
  xlim(0.3, 10.3) +
  ylim(0.4, 1.6) +
  labs(title = "Figure D1. Extension workflow") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12))
save_fig(e1, "figure_D1_extension_workflow", width = 9, height = 2.2)

# Figure D2: centroid vs grid-informed accessibility
care_comp <- comparison[group == "caregiving_adults" & mode == "walk_15min"]
centroid_map <- merge(lsoas[, "LSOA21CD"], care_comp[, .(LSOA21CD = lsoa21cd, score = centroid_score)], by = "LSOA21CD", all.x = TRUE)
centroid_map <- st_sf(LSOA21CD = centroid_map$LSOA21CD, score = centroid_map$score, panel = "Centroid-based", geometry = st_geometry(centroid_map), crs = st_crs(lsoas))
grid_map <- merge(lsoas[, "LSOA21CD"], care_comp[, .(LSOA21CD = lsoa21cd, score = grid_informed_score)], by = "LSOA21CD", all.x = TRUE)
grid_map <- st_sf(LSOA21CD = grid_map$LSOA21CD, score = grid_map$score, panel = "Grid-informed", geometry = st_geometry(grid_map), crs = st_crs(lsoas))
e2_data <- rbind(centroid_map, grid_map)
e2 <- ggplot(e2_data) +
  geom_sf(aes(fill = score), colour = NA) +
  geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45) +
  geom_sf(data = boroughs, inherit.aes = FALSE, fill = NA, colour = "#FFFFFF", linewidth = 0.14) +
  facet_wrap(~panel, nrow = 1) +
  scale_fill_gradientn(colours = c("#6F9FBC", "#A9C7D8", "#DCE8EC", "#F5ECEE", "#E5BDC9", "#C985A2"),
                       limits = c(0, 100), breaks = seq(0, 100, 25),
                       name = "Accessibility score (0–100)", na.value = "#ECEBE7") +
  labs(title = "Figure D2. Centroid-based and grid-informed accessibility",
       subtitle = "Caregiving adults; modelled WALK ≤15 min; common scale; EPSG:27700") +
  map_theme
e2 <- add_decorations(e2, london_bbox)
save_fig(e2, "figure_D2_centroid_grid_accessibility", width = 10, height = 5.8)

# Figure D3: centroid minus grid difference
diff_data <- merge(lsoas, care_comp[, .(LSOA21CD = lsoa21cd, centroid_minus_grid)], by = "LSOA21CD", all.x = TRUE)
lim <- quantile(abs(diff_data$centroid_minus_grid), 0.98, na.rm = TRUE)
e3 <- ggplot(diff_data) +
  geom_sf(aes(fill = centroid_minus_grid), colour = NA) +
  geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45) +
  geom_sf(data = boroughs, inherit.aes = FALSE, fill = NA, colour = "#FFFFFF", linewidth = 0.14) +
  scale_fill_gradientn(colours = c("#6794B2", "#A9C8D8", "#F8F6F2", "#E8C0CC", "#C57494"),
                       limits = c(-lim, lim), oob = squish, name = "Centroid − grid\n(score points)", na.value = "#ECEBE7") +
  labs(title = "Figure D3. Centroid minus grid-informed accessibility",
       subtitle = "Positive = centroid overestimation; caregiving adults; WALK ≤15 min; EPSG:27700") +
  map_theme
e3 <- add_decorations(e3, london_bbox)
save_fig(e3, "figure_D3_centroid_minus_grid_difference")

# Figure D4: within-LSOA variation
var_data <- merge(lsoas, care_comp[, .(LSOA21CD = lsoa21cd, within_lsoa_sd, grid_lowest_decile_mean)], by = "LSOA21CD", all.x = TRUE)
e4 <- ggplot(var_data) +
  geom_sf(aes(fill = within_lsoa_sd), colour = NA) +
  geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45) +
  geom_sf(data = boroughs, inherit.aes = FALSE, fill = NA, colour = "#FFFFFF", linewidth = 0.14) +
  scale_fill_gradientn(colours = c("#778CB8", "#AEBAD2", "#DEE2EA", "#F4EFD9", "#E8D58F", "#D6B84E"),
                       name = "Within-LSOA SD\n(score points)", na.value = "#ECEBE7") +
  labs(title = "Figure D4. Within-LSOA variation",
       subtitle = "Caregiving adults; 100 m residential origins; WALK ≤15 min; EPSG:27700") +
  map_theme
e4 <- add_decorations(e4, london_bbox)
save_fig(e4, "figure_D4_within_lsoa_variation")

# Figure D5: LISA reclassification
care_mismatch <- mismatch[group == "caregiving_adults"]
e5_data <- merge(lsoas, care_mismatch[, .(LSOA21CD, lisa_reclassification)], by = "LSOA21CD", all.x = TRUE)
e5_data$lisa_reclassification[is.na(e5_data$lisa_reclassification)] <- "never high-high"
reclass_palette <- c(
  "Stable deficit (both methods)" = "#B87380",
  "New deficit (grid only)" = "#C9AD74",
  "Removed deficit (centroid only)" = "#8FA5B8",
  "Never high-high" = "#ECEBE7"
)
e5_data$lisa_reclassification <- factor(
  e5_data$lisa_reclassification,
  levels = c("stable high-high", "new high-high", "no longer high-high", "never high-high"),
  labels = names(reclass_palette)
)
e5 <- ggplot(e5_data) +
  geom_sf(aes(fill = lisa_reclassification), colour = NA) +
  geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45) +
  geom_sf(data = boroughs, inherit.aes = FALSE, fill = NA, colour = "#FFFFFF", linewidth = 0.14) +
  scale_fill_manual(values = reclass_palette, name = NULL, drop = FALSE) +
  labs(title = "Figure D5. Which deficit clusters change after grid refinement?",
       subtitle = "Stable = both methods; new = grid only; removed = centroid only") +
  map_theme
e5 <- add_decorations(e5, london_bbox)
save_fig(e5, "figure_D5_lisa_reclassification")

# Figure D6: compound mismatch
e6_data <- merge(lsoas, compound[, .(LSOA21CD, compound_class)], by = "LSOA21CD", all.x = TRUE)
e6_data$compound_class[is.na(e6_data$compound_class)] <- "0 = no high-high mismatch"
compound_palette <- c("No group" = "#ECEBE7", "1 group" = "#AFC8D3",
                      "2 groups" = "#C9AD74", "3 groups" = "#B87380")
e6_data$compound_class <- factor(
  e6_data$compound_class,
  levels = c("0 = no high-high mismatch", "1 = single-group mismatch", "2 = two-group compound mismatch", "3 = three-group compound mismatch"),
  labels = names(compound_palette)
)
e6 <- ggplot(e6_data) +
  geom_sf(aes(fill = compound_class), colour = NA) +
  geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45) +
  geom_sf(data = boroughs, inherit.aes = FALSE, fill = NA, colour = "#FFFFFF", linewidth = 0.14) +
  scale_fill_manual(values = compound_palette, name = NULL, drop = FALSE) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "Figure D6. How many household groups share a local deficit?",
       subtitle = "Warmer tones indicate broader compound mismatch across household roles") +
  map_theme
e6 <- add_decorations(e6, london_bbox)
save_fig(e6, "figure_D6_compound_mismatch")

# Figure D7: three case comparison metric panel
case_metric <- melt(
  case_profiles[, .(
    case_name,
    `Care need` = care_need,
    `Grid mean access` = grid_mean_access,
    `Lowest-decile access` = lowest_decile_access,
    `Within-LSOA SD` = within_lsoa_variation,
    `Mismatch z-score` = mismatch_score,
    `Compound count` = compound_count
  )],
  id.vars = "case_name",
  variable.name = "metric",
  value.name = "value"
)
case_metric[, case_name := factor(case_name, levels = case_profiles$case_name)]
metric_palette <- c(
  "Case 1 Barking Riverside / Thames View" = "#789FB5",
  "Case 2 compound mismatch" = "#C17F96",
  "Case 3 positive counter-case" = "#8FA98F"
)
e7 <- ggplot(case_metric, aes(x = case_name, y = value, fill = case_name)) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = round(value, 1)), hjust = -0.08, size = 2.7, colour = "#222222") +
  coord_flip(clip = "off") +
  facet_wrap(~metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = metric_palette, guide = "none") +
  labs(
    title = "Figure D7. Three case-study comparison",
    subtitle = "Need, accessibility, internal variation, mismatch and compound status",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "#555555"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 7),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(8, 18, 8, 8)
  )
save_fig(e7, "figure_D7_case_study_comparison", width = 11, height = 5.8)

# Final tables
grid_area <- fread(required_file(file.path(table_dir, "grid_area_summary.csv")))
res_sum <- fread(required_file(file.path(table_dir, "residential_grid_summary.csv")))
table_e1 <- data.table(
  metric = c("total_100m_grid_cells", "residential_grid_cells", "lsoas_with_residential_grids", "mean_residential_grids_per_lsoa", "residential_filter"),
  value = c(
    grid_area[metric == "retained_grid_count", value],
    sum(res_sum$retained_grid_count, na.rm = TRUE),
    nrow(res_sum[retained_grid_count > 0]),
    round(mean(res_sum$retained_grid_count, na.rm = TRUE), 2),
    "OSM residential buildings and residential land-use"
  )
)
fwrite(table_e1, file.path(table_dir, "table_D1_grid_construction_summary.csv"))
fwrite(stats, file.path(table_dir, "table_D2_centroid_grid_comparison_statistics.csv"))
fwrite(lisa, file.path(table_dir, "table_D3_lisa_reclassification.csv"))
fwrite(compound_borough, file.path(table_dir, "table_D4_compound_mismatch_by_borough.csv"))
table_e5 <- case_profiles[, .(
  case_name,
  case_type,
  care_need = round(care_need, 2),
  child_need = round(child_need, 2),
  older_need = round(older_need, 2),
  centroid_access = round(centroid_access, 2),
  grid_mean_access = round(grid_mean_access, 2),
  lowest_decile_access = round(lowest_decile_access, 2),
  within_lsoa_variation = round(within_lsoa_variation, 2),
  mismatch_z = round(mismatch_score, 2),
  lisa_class,
  compound_type,
  main_service_deficit,
  policy_implication
)]
fwrite(table_e5, file.path(table_dir, "table_D5_case_study_comparison.csv"))

# A4-facing compact derivatives; the full analytical tables above remain the
# repository record and are not discarded.
table_d3_compact <- lisa[, .(
  household_group = group,
  classification = lisa_reclassification,
  n_lsoas = N,
  share_pct = round(share_lsoas_pct, 1)
)]
fwrite(table_d3_compact, file.path(table_dir, "table_D3_lisa_reclassification_compact.csv"))

table_d4_compact <- compound_borough[, .(
  borough,
  two_group_n = two_group_count,
  three_group_n = three_group_count,
  any_compound_pct = round(100 * (two_group_count + three_group_count) / borough_lsoa_count, 1)
)][order(-any_compound_pct)]
fwrite(table_d4_compact, file.path(table_dir, "table_D4_compound_mismatch_by_borough_compact.csv"))

table_d5a <- table_e5[, .(
  case_name,
  care_need,
  centroid_access,
  grid_mean_access,
  lowest_decile_access,
  within_lsoa_variation,
  mismatch_z
)]
table_d5b <- table_e5[, .(
  case_name,
  lisa_class,
  compound_type,
  main_service_deficit,
  policy_implication
)]
fwrite(table_d5a, file.path(table_dir, "table_D5a_case_metrics_compact.csv"))
fwrite(table_d5b, file.path(table_dir, "table_D5b_case_interpretation_compact.csv"))

# Reports
writeLines(c(
  "# Extension methodology",
  "",
  "This extension keeps the original LSOA analysis and adds a 100m residential-grid robustness layer for accessibility.",
  "",
  "The LSOA scale is retained because the Census-derived need indicators are area-level proxies. The 100m grid improves the spatial precision of origins for accessibility only; it does not create new individual-level need estimates.",
  "",
  "Grid cells were constructed in EPSG:27700 and assigned to LSOAs by centroid location. Residential origins were filtered using OSM residential building footprints and residential land-use polygons. Residential weights use intersected residential building footprint area where available, with land-use-only residential cells retained at weight 1.",
  "",
  "Grid accessibility was calculated using the original R5r network, POI categories, travel-time settings, decay function and basket weights. Grid scores were aggregated back to LSOAs using mean, median, p10, lowest-decile mean, within-LSOA SD/IQR/min/max, and weighted mean/median.",
  "",
  "Centroid-grid comparison uses Pearson and Spearman correlations, absolute differences, RMSE, lowest-accessibility quintile overlap, high-mismatch quintile overlap, Jaccard indices and threshold sensitivity at 0.25, 0.50 and 0.75 SD.",
  "",
  "Grid-informed mismatch keeps the original need score and z-score mismatch logic, replacing only centroid-based accessibility with grid-informed LSOA accessibility. LISA uses queen contiguity, row-standardised spatial weights and p < 0.05, matching the original approach.",
  "",
  "Compound mismatch counts whether children/adolescents, caregiving adults and older adults are each in grid-informed high-high mismatch clusters. Case studies were selected from automatic candidate lists: one fixed Barking Riverside / Thames View case, one compound mismatch case and one positive counter-case matched on need-related variables."
), file.path(extension_dir, "extension_methodology.md"), useBytes = TRUE)

writeLines(c(
  "# Extension results summary",
  "",
  paste0("The full 100m grid contains ", table_e1[metric == "total_100m_grid_cells", value], " cells; OSM residential filtering retained ", table_e1[metric == "residential_grid_cells", value], " residential-origin cells."),
  "",
  paste0("Caregiving-adult centroid-grid Pearson correlation is ", round(stats[group == "caregiving_adults" & mode == "walk_15min", pearson_correlation], 3), " for walk-only and ", round(stats[group == "caregiving_adults" & mode == "walk_transit_30min", pearson_correlation], 3), " for walk+public transport."),
  "",
  paste0("Lowest-accessibility quintile Jaccard overlap for caregiving adults is ", round(stats[group == "caregiving_adults" & mode == "walk_15min", lowest_accessibility_20_jaccard], 3), " in the walk-only scenario, showing that centroid and grid-informed approaches identify overlapping but not identical low-access areas."),
  "",
  paste0("Grid-informed high-high mismatch counts are: children/adolescents ", mismatch[group == "children_and_adolescents" & grid_mean_lisa_cluster == "High-high mismatch cluster", .N], ", caregiving adults ", mismatch[group == "caregiving_adults" & grid_mean_lisa_cluster == "High-high mismatch cluster", .N], ", and older adults ", mismatch[group == "older_adults" & grid_mean_lisa_cluster == "High-high mismatch cluster", .N], "."),
  "",
  paste0("Compound mismatch identifies ", compound[compound_count == 2, .N], " two-group and ", compound[compound_count == 3, .N], " three-group high-high mismatch LSOAs."),
  "",
  "The confirmed case studies are Barking Riverside / Thames View, Hillingdon 003D as the compound mismatch case, and Barking and Dagenham 015F as the positive counter-case."
), file.path(extension_dir, "extension_results_summary.md"), useBytes = TRUE)

writeLines(c(
  "# Extension limitations",
  "",
  "- The grid improves accessibility-origin precision, not population-need precision.",
  "- Census need remains an area-level proxy and is not redistributed to 100m cells.",
  "- Grid results depend on the quality and completeness of OSM residential building and land-use tags.",
  "- A 100m grid is not a true individual residential origin or observed trip origin.",
  "- The extension is a robustness and sensitivity analysis, not a full replacement of the original LSOA model.",
  "- Area-level results must not be interpreted as individual behaviour or individual accessibility outcomes.",
  "- OSM building footprint area is not equivalent to household count, population or floor area.",
  "- LISA clusters describe spatial association, not causality."
), file.path(extension_dir, "extension_limitations.md"), useBytes = TRUE)

writeLines(c(
  "# Care-sensitive accessibility extension",
  "",
  "This directory contains the completed extension workflow for multi-scale robustness and comparative neighbourhood analysis in London.",
  "",
  "Key outputs are in `outputs/tables`, `outputs/maps` and `outputs/case_studies`. Original dissertation scripts and outputs are not overwritten.",
  "",
  "Completed stages: audit, 100m grid construction, OSM residential filtering, full grid accessibility for three basket-weight groups, LSOA aggregation, centroid-grid comparison, grid-informed mismatch/LISA, compound mismatch, case selection, case-study profiles and extension figures/tables.",
  "",
  "Run order: `01_build_100m_grid.R`, `02_filter_residential_grid.R`, `03_run_grid_accessibility.R`, helper `derive_all_group_grid_baskets.R`, then `04` through `10`."
), file.path(extension_dir, "README.md"), useBytes = TRUE)

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
