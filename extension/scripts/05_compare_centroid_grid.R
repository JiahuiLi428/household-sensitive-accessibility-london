suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(ggplot2)
})

set.seed(20260716)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, "UCLdissertation.Rproj")) ||
        (dir.exists(file.path(current, "data")) && dir.exists(file.path(current, "scripts")))
    ) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate project root from: ", start)
    }
    current <- parent
  }
}

project_root <- find_project_root()
rel_path <- function(...) file.path(project_root, ...)
source(rel_path("scripts", "model", "00_config.R"))

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--test" %in% args) "test" else "full"
suffix <- if (run_mode == "test") "_test" else ""

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
table_dir <- file.path(extension_dir, "outputs", "tables")
map_dir <- file.path(extension_dir, "outputs", "maps")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("05_compare_centroid_grid_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("05_compare_centroid_grid.R started at: ", started_at)
message("Project root: ", project_root)
message("Run mode: ", run_mode)

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required input does not exist: ", path)
  }
  invisible(path)
}

required_fields <- function(x, fields, object_name) {
  missing <- setdiff(fields, names(x))
  if (length(missing) > 0) {
    stop(object_name, " is missing required fields: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

safe_cor <- function(x, y, method) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3) {
    return(NA_real_)
  }
  as.numeric(cor(x[keep], y[keep], method = method))
}

jaccard <- function(a, b) {
  a[is.na(a)] <- FALSE
  b[is.na(b)] <- FALSE
  den <- sum(a | b)
  if (den == 0) {
    return(NA_real_)
  }
  sum(a & b) / den
}

agg_path <- required_file(file.path(table_dir, paste0("lsoa_grid_aggregated_accessibility", suffix, ".csv")))
care_path <- required_file(rel_path("data", "output", "analysis", "care_mobility_accessibility_lsoa.csv"))
continuous_path <- required_file(rel_path("data", "output", "analysis", "continuous_need_access_mismatch_indices.csv"))
composite_path <- required_file(rel_path("data", "output", "analysis", "composite_service_basket_accessibility_indices.csv"))
lsoa_path <- required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg"))
thames_path <- rel_path("data", "boundaries", "river_thames_osm.geojson")

agg <- fread(agg_path)
care <- fread(care_path)
continuous <- fread(continuous_path)
composite <- fread(composite_path)
lsoas <- st_read(lsoa_path, quiet = TRUE)

required_fields(
  agg,
  c(
    "lsoa21cd", "group", "mode", "grid_mean", "grid_median",
    "grid_lowest_decile_mean", "grid_sd", "weighted_grid_mean",
    "weighted_grid_median", "valid_grid_count"
  ),
  "LSOA grid aggregated accessibility"
)
required_fields(care, c("LSOA21CD", "care_need_score", "care_walk_index", "care_transit_index"), "care mobility output")
required_fields(continuous, c("LSOA21CD", "children_need_score", "children_access_score", "older_need_score", "older_access_score"), "continuous mismatch output")
required_fields(
  composite,
  c("LSOA21CD", "children_walk_index", "children_transit_index", "older_walk_index", "older_transit_index"),
  "composite service basket output"
)

centroid <- merge(
  care[, .(
    LSOA21CD,
    borough_name,
    london_ring,
    IMD_Decile,
    care_need_score,
    care_walk_index,
    care_transit_index
  )],
  continuous[, .(
    LSOA21CD,
    children_need_score,
    children_access_score,
    older_need_score,
    older_access_score
  )],
  by = "LSOA21CD",
  all.x = TRUE
)
centroid <- merge(
  centroid,
  composite[, .(
    LSOA21CD,
    children_walk_index,
    children_transit_index,
    older_walk_index,
    older_transit_index
  )],
  by = "LSOA21CD",
  all.x = TRUE
)

comparisons <- list()
valid_group_names <- unique(agg$group[!is.na(agg$group) & nzchar(agg$group)])
valid_mode_names <- unique(agg$mode[!is.na(agg$mode) & nzchar(agg$mode)])
for (group_name in valid_group_names) {
  for (mode_name in valid_mode_names) {
    part <- agg[group == group_name & mode == mode_name]
    part <- merge(part, centroid, by.x = "lsoa21cd", by.y = "LSOA21CD", all.x = TRUE)

    if (group_name == "caregiving_adults") {
      part[, need_score := care_need_score]
      part[, centroid_score := if (mode_name == "walk_transit_30min") care_transit_index else care_walk_index]
    } else if (group_name %in% c("children_and_adolescents", "children", "family")) {
      part[, need_score := children_need_score]
      part[, centroid_score := if (mode_name == "walk_transit_30min") children_transit_index else children_walk_index]
    } else if (group_name %in% c("older_adults", "older")) {
      part[, need_score := older_need_score]
      part[, centroid_score := if (mode_name == "walk_transit_30min") older_transit_index else older_walk_index]
    } else {
      warning("Unsupported group for centroid comparison: ", group_name)
      next
    }

    part[, grid_informed_score := fifelse(
      !is.na(weighted_grid_mean) & residential_weight_sum > 0,
      weighted_grid_mean,
      grid_mean
    )]
    part[, centroid_minus_grid := centroid_score - grid_informed_score]
    part[, abs_difference := abs(centroid_minus_grid)]
    part[, within_lsoa_sd := grid_sd]
    diff_sd <- sd(part$centroid_minus_grid, na.rm = TRUE)
    part[, standardised_difference := if (is.finite(diff_sd) && diff_sd > 0) centroid_minus_grid / diff_sd else NA_real_]
    part[, original_centroid_mismatch := need_score - centroid_score]
    part[, preliminary_grid_mismatch := need_score - grid_informed_score]

    part[, centroid_low20 := centroid_score <= quantile(centroid_score, 0.20, na.rm = TRUE)]
    part[, grid_mean_low20 := grid_informed_score <= quantile(grid_informed_score, 0.20, na.rm = TRUE)]
    part[, grid_lowest_decile_low20 := grid_lowest_decile_mean <= quantile(grid_lowest_decile_mean, 0.20, na.rm = TRUE)]
    part[, centroid_high_mismatch20 := original_centroid_mismatch >= quantile(original_centroid_mismatch, 0.80, na.rm = TRUE)]
    part[, grid_high_mismatch20 := preliminary_grid_mismatch >= quantile(preliminary_grid_mismatch, 0.80, na.rm = TRUE)]

    for (threshold in c(0.25, 0.50, 0.75)) {
      part[
        ,
        paste0("overestimate_", gsub("\\.", "_", as.character(threshold))) :=
          standardised_difference >= threshold
      ]
      part[
        ,
        paste0("underestimate_", gsub("\\.", "_", as.character(threshold))) :=
          standardised_difference <= -threshold
      ]
    }

    part[, area_class := "Other"]
    part[centroid_low20 & grid_mean_low20, area_class := "Stable low-access area"]
    part[!grid_mean_low20 & grid_lowest_decile_low20, area_class := "Hidden local pocket"]
    part[abs(standardised_difference) >= 0.50 & standardised_difference > 0, area_class := "Centroid overestimation"]
    part[abs(standardised_difference) >= 0.50 & standardised_difference < 0, area_class := "Centroid underestimation"]

    comparisons[[paste(group_name, mode_name, sep = "__")]] <- part
  }
}

comparison <- rbindlist(comparisons, use.names = TRUE, fill = TRUE)
comparison_out <- comparison[
  ,
  .(
    lsoa21cd,
    LSOA21NM,
    borough_name,
    london_ring,
    IMD_Decile,
    group,
    mode,
    need_score,
    centroid_score,
    grid_mean,
    grid_median,
    grid_lowest_decile_mean,
    weighted_grid_mean,
    weighted_grid_median,
    grid_informed_score,
    within_lsoa_sd,
    valid_grid_count,
    centroid_minus_grid,
    abs_difference,
    standardised_difference,
    original_centroid_mismatch,
    preliminary_grid_mismatch,
    centroid_low20,
    grid_mean_low20,
    grid_lowest_decile_low20,
    centroid_high_mismatch20,
    grid_high_mismatch20,
    area_class,
    overestimate_0_25,
    underestimate_0_25,
    overestimate_0_5,
    underestimate_0_5,
    overestimate_0_75,
    underestimate_0_75
  )
]

stats <- comparison[
  ,
  .(
    n_lsoas = sum(is.finite(centroid_score) & is.finite(grid_informed_score)),
    pearson_correlation = safe_cor(centroid_score, grid_informed_score, "pearson"),
    spearman_rank_correlation = safe_cor(centroid_score, grid_informed_score, "spearman"),
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    median_absolute_difference = median(abs_difference, na.rm = TRUE),
    rmse = sqrt(mean((centroid_minus_grid)^2, na.rm = TRUE)),
    lowest_accessibility_20_overlap_count = sum(centroid_low20 & grid_mean_low20, na.rm = TRUE),
    lowest_accessibility_20_jaccard = jaccard(centroid_low20, grid_mean_low20),
    highest_mismatch_20_overlap_count = sum(centroid_high_mismatch20 & grid_high_mismatch20, na.rm = TRUE),
    highest_mismatch_20_jaccard = jaccard(centroid_high_mismatch20, grid_high_mismatch20),
    overestimate_0_25_count = sum(overestimate_0_25, na.rm = TRUE),
    underestimate_0_25_count = sum(underestimate_0_25, na.rm = TRUE),
    overestimate_0_5_count = sum(overestimate_0_5, na.rm = TRUE),
    underestimate_0_5_count = sum(underestimate_0_5, na.rm = TRUE),
    overestimate_0_75_count = sum(overestimate_0_75, na.rm = TRUE),
    underestimate_0_75_count = sum(underestimate_0_75, na.rm = TRUE)
  ),
  by = .(group, mode)
]

comparison_path <- file.path(table_dir, paste0("centroid_grid_comparison", suffix, ".csv"))
stats_path <- file.path(table_dir, paste0("centroid_grid_statistics", suffix, ".csv"))
fwrite(comparison_out, comparison_path)
fwrite(stats, stats_path)

message("Wrote centroid-grid comparison: ", comparison_path)
message("Wrote centroid-grid statistics: ", stats_path)
print(stats)

if (run_mode == "full" && nrow(comparison_out[group == "caregiving_adults" & mode == "walk_15min"]) > 0) {
  map_data <- merge(
    lsoas,
    comparison_out[group == "caregiving_adults" & mode == "walk_15min"],
    by.x = "LSOA21CD",
    by.y = "lsoa21cd",
    all.x = TRUE
  )
  thames <- NULL
  if (file.exists(thames_path)) {
    thames <- st_transform(st_read(thames_path, quiet = TRUE), st_crs(map_data))
  }
  boroughs <- aggregate(map_data[, "borough_name"], by = list(borough_name_group = map_data$borough_name), FUN = function(x) x[1])
  diff_lim <- quantile(abs(map_data$centroid_minus_grid), 0.98, na.rm = TRUE)
  base_theme <- theme_void(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "#555555"),
      legend.position = c(0.18, 0.20),
      legend.background = element_rect(fill = "white", colour = "#D0D0D0", linewidth = 0.2),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  diff_map <- ggplot(map_data) +
    geom_sf(aes(fill = centroid_minus_grid), colour = NA) +
    geom_sf(data = boroughs, fill = NA, colour = "#FFFFFF", linewidth = 0.15) +
    scale_fill_gradientn(
      colours = c("#2166AC", "#67A9CF", "#F7F7F7", "#F4A582", "#B2182B"),
      limits = c(-diff_lim, diff_lim),
      oob = scales::squish,
      na.value = "#EDEDED",
      name = "Centroid\nminus grid"
    ) +
    labs(
      title = "Centroid minus grid-informed accessibility",
      subtitle = "Caregiving adults, walk-only scenario"
    ) +
    base_theme
  if (!is.null(thames)) {
    diff_map <- diff_map + geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45)
  }
  ggsave(file.path(map_dir, "centroid_grid_difference_map.png"), diff_map, width = 7.2, height = 6.7, dpi = 300)
  ggsave(file.path(map_dir, "centroid_grid_difference_map.pdf"), diff_map, width = 7.2, height = 6.7)

  pocket_map <- ggplot(map_data) +
    geom_sf(aes(fill = area_class == "Hidden local pocket"), colour = NA) +
    geom_sf(data = boroughs, fill = NA, colour = "#FFFFFF", linewidth = 0.15) +
    scale_fill_manual(
      values = c("TRUE" = "#7B3294", "FALSE" = "#ECEBE7"),
      labels = c("FALSE" = "Other", "TRUE" = "Hidden local pocket"),
      name = NULL,
      drop = FALSE
    ) +
    labs(
      title = "Hidden local pockets of low accessibility",
      subtitle = "Grid lowest-decile low but grid mean not in the lowest quintile"
    ) +
    base_theme
  if (!is.null(thames)) {
    pocket_map <- pocket_map + geom_sf(data = thames, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45)
  }
  ggsave(file.path(map_dir, "hidden_local_pockets_map.png"), pocket_map, width = 7.2, height = 6.7, dpi = 300)
  ggsave(file.path(map_dir, "hidden_local_pockets_map.pdf"), pocket_map, width = 7.2, height = 6.7)
  message("Wrote Phase 5 draft maps to: ", map_dir)
}

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
