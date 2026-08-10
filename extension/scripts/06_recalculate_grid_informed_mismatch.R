suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(spdep)
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
table_dir <- file.path(extension_dir, "outputs", "tables")
map_dir <- file.path(extension_dir, "outputs", "maps")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("06_recalculate_grid_informed_mismatch_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("06_recalculate_grid_informed_mismatch.R started at: ", started_at)
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

classify_lisa <- function(x, listw) {
  x_centered <- x - mean(x, na.rm = TRUE)
  lag_x <- lag.listw(listw, x_centered, zero.policy = TRUE)
  local <- localmoran(x, listw, zero.policy = TRUE)
  p <- as.numeric(local[, "Pr(z != E(Ii))"])
  cluster <- rep("Not significant", length(x))
  sig <- p < 0.05
  cluster[sig & x_centered > 0 & lag_x > 0] <- "High-high mismatch cluster"
  cluster[sig & x_centered < 0 & lag_x < 0] <- "Low-low surplus cluster"
  cluster[sig & x_centered > 0 & lag_x < 0] <- "High-low spatial outlier"
  cluster[sig & x_centered < 0 & lag_x > 0] <- "Low-high spatial outlier"
  list(
    local_moran_i = as.numeric(local[, "Ii"]),
    local_moran_p = p,
    spatial_lag_mismatch = lag_x,
    lisa_cluster = cluster
  )
}

agg_path <- required_file(file.path(table_dir, paste0("lsoa_grid_aggregated_accessibility", suffix, ".csv")))
care_path <- required_file(rel_path("data", "output", "analysis", "care_mobility_accessibility_lsoa.csv"))
continuous_path <- required_file(rel_path("data", "output", "analysis", "continuous_need_access_mismatch_indices.csv"))
composite_path <- required_file(rel_path("data", "output", "analysis", "composite_service_basket_accessibility_indices.csv"))
lsoa_path <- required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg"))
river_path <- rel_path("data", "boundaries", "river_thames_osm.geojson")

agg <- fread(agg_path)
care <- fread(care_path)
continuous <- fread(continuous_path)
composite <- fread(composite_path)
lsoas <- st_read(lsoa_path, quiet = TRUE)

required_fields(
  agg,
  c("lsoa21cd", "group", "mode", "grid_mean", "grid_lowest_decile_mean", "weighted_grid_mean", "residential_weight_sum", "valid_grid_count"),
  "LSOA grid aggregated accessibility"
)
required_fields(care, c("LSOA21CD", "care_need_score", "care_walk_index"), "care mobility output")
required_fields(continuous, c("LSOA21CD", "children_need_score", "older_need_score"), "continuous mismatch output")
required_fields(composite, c("LSOA21CD", "children_walk_index", "older_walk_index"), "composite service basket output")

centroid <- merge(
  care[, .(LSOA21CD, LSOA21NM, borough_name, london_ring, IMD_Decile, care_need_score, care_walk_index)],
  continuous[, .(LSOA21CD, children_need_score, older_need_score)],
  by = "LSOA21CD",
  all.x = TRUE
)
centroid <- merge(
  centroid,
  composite[, .(LSOA21CD, children_walk_index, older_walk_index)],
  by = "LSOA21CD",
  all.x = TRUE
)

group_specs <- data.table(
  group = c("children_and_adolescents", "caregiving_adults", "older_adults"),
  need_col = c("children_need_score", "care_need_score", "older_need_score"),
  centroid_access_col = c("children_walk_index", "care_walk_index", "older_walk_index")
)

all_results <- list()
global_results <- list()

for (i in seq_len(nrow(group_specs))) {
  spec <- group_specs[i]
  message("Recalculating mismatch and LISA for group: ", spec$group)
  grid_group <- agg[group == spec$group & mode == "walk_15min"]
  if (nrow(grid_group) == 0) {
    warning("Skipping group without walk_15min grid rows: ", spec$group)
    next
  }

  dt <- merge(
    centroid,
    grid_group[, .(
      lsoa21cd,
      grid_mean,
      grid_lowest_decile_mean,
      weighted_grid_mean,
      residential_weight_sum,
      valid_grid_count
    )],
    by.x = "LSOA21CD",
    by.y = "lsoa21cd",
    all.x = TRUE
  )
  dt[, group := spec$group]
  dt[, need_score := get(spec$need_col)]
  dt[, original_centroid_access := get(spec$centroid_access_col)]
  dt[, grid_mean_access := fifelse(!is.na(weighted_grid_mean) & residential_weight_sum > 0, weighted_grid_mean, grid_mean)]
  dt[, grid_lowest_decile_access := grid_lowest_decile_mean]
  dt[, need_z := zscore(need_score)]
  dt[, original_centroid_access_z := zscore(original_centroid_access)]
  dt[, grid_mean_access_z := zscore(grid_mean_access)]
  dt[, grid_lowest_decile_access_z := zscore(grid_lowest_decile_access)]
  dt[, original_centroid_mismatch := need_z - original_centroid_access_z]
  dt[, grid_mean_mismatch := need_z - grid_mean_access_z]
  dt[, grid_lowest_decile_mismatch := need_z - grid_lowest_decile_access_z]
  dt[, mismatch_shift_grid_minus_original := grid_mean_mismatch - original_centroid_mismatch]

  g <- merge(lsoas, dt[, !"LSOA21NM"], by = "LSOA21CD", all.x = FALSE, all.y = FALSE)
  g <- g[!is.na(g$grid_mean_mismatch), ]
  g <- st_make_valid(g)

  nb <- poly2nb(g, queen = TRUE)
  listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  lisa_original <- classify_lisa(g$original_centroid_mismatch, listw)
  lisa_grid <- classify_lisa(g$grid_mean_mismatch, listw)
  lisa_lowest <- classify_lisa(g$grid_lowest_decile_mismatch, listw)
  global_grid <- moran.test(g$grid_mean_mismatch, listw, zero.policy = TRUE)
  global_lowest <- moran.test(g$grid_lowest_decile_mismatch, listw, zero.policy = TRUE)

  g$original_lisa_cluster <- lisa_original$lisa_cluster
  g$original_local_moran_i <- lisa_original$local_moran_i
  g$original_local_moran_p <- lisa_original$local_moran_p
  g$grid_mean_lisa_cluster <- lisa_grid$lisa_cluster
  g$grid_mean_local_moran_i <- lisa_grid$local_moran_i
  g$grid_mean_local_moran_p <- lisa_grid$local_moran_p
  g$grid_mean_spatial_lag_mismatch <- lisa_grid$spatial_lag_mismatch
  g$grid_lowest_decile_lisa_cluster <- lisa_lowest$lisa_cluster
  g$grid_lowest_decile_local_moran_i <- lisa_lowest$local_moran_i
  g$grid_lowest_decile_local_moran_p <- lisa_lowest$local_moran_p

  g$original_high_high <- g$original_lisa_cluster == "High-high mismatch cluster"
  g$grid_high_high <- g$grid_mean_lisa_cluster == "High-high mismatch cluster"
  g$lisa_reclassification <- "never high-high"
  g$lisa_reclassification[g$original_high_high & g$grid_high_high] <- "stable high-high"
  g$lisa_reclassification[!g$original_high_high & g$grid_high_high] <- "new high-high"
  g$lisa_reclassification[g$original_high_high & !g$grid_high_high] <- "no longer high-high"

  out_dt <- as.data.table(st_drop_geometry(g))[
    ,
    .(
      LSOA21CD,
      LSOA21NM,
      borough_name,
      london_ring,
      IMD_Decile,
      group,
      need_score,
      need_z,
      original_centroid_access,
      grid_mean_access,
      grid_lowest_decile_access,
      valid_grid_count,
      original_centroid_mismatch,
      grid_mean_mismatch,
      grid_lowest_decile_mismatch,
      mismatch_shift_grid_minus_original,
      original_lisa_cluster,
      grid_mean_lisa_cluster,
      grid_lowest_decile_lisa_cluster,
      original_local_moran_i,
      original_local_moran_p,
      grid_mean_local_moran_i,
      grid_mean_local_moran_p,
      grid_mean_spatial_lag_mismatch,
      lisa_reclassification
    )
  ]
  all_results[[spec$group]] <- out_dt

  global_results[[spec$group]] <- data.table(
    group = spec$group,
    mismatch_variant = c("grid_mean_mismatch", "grid_lowest_decile_mismatch"),
    global_moran_i = c(
      as.numeric(global_grid$estimate["Moran I statistic"]),
      as.numeric(global_lowest$estimate["Moran I statistic"])
    ),
    expected_i = c(
      as.numeric(global_grid$estimate["Expectation"]),
      as.numeric(global_lowest$estimate["Expectation"])
    ),
    variance_i = c(
      as.numeric(global_grid$estimate["Variance"]),
      as.numeric(global_lowest$estimate["Variance"])
    ),
    z_score = c(as.numeric(global_grid$statistic), as.numeric(global_lowest$statistic)),
    p_value = c(global_grid$p.value, global_lowest$p.value),
    n_lsoas = nrow(g)
  )
}

result <- rbindlist(all_results, use.names = TRUE)
global_dt <- rbindlist(global_results, use.names = TRUE)

summary_dt <- result[
  ,
  .(
    n_lsoas = .N,
    mean_original_mismatch = round(mean(original_centroid_mismatch, na.rm = TRUE), 3),
    mean_grid_mean_mismatch = round(mean(grid_mean_mismatch, na.rm = TRUE), 3),
    mean_lowest_decile_mismatch = round(mean(grid_lowest_decile_mismatch, na.rm = TRUE), 3),
    original_high_high_count = sum(original_lisa_cluster == "High-high mismatch cluster", na.rm = TRUE),
    grid_high_high_count = sum(grid_mean_lisa_cluster == "High-high mismatch cluster", na.rm = TRUE)
  ),
  by = group
]

reclass_counts <- copy(result)
reclass_counts[, comparison := "original_to_grid_mean"]
reclass_counts <- reclass_counts[, .N, by = .(group, comparison, lisa_reclassification)]
cluster_counts <- copy(result)
cluster_counts[, comparison := "grid_mean_lisa_cluster"]
cluster_counts[, lisa_reclassification := grid_mean_lisa_cluster]
cluster_counts <- cluster_counts[, .N, by = .(group, comparison, lisa_reclassification)]
lisa_comparison <- rbind(reclass_counts, cluster_counts, fill = TRUE)
lisa_comparison[, share_lsoas_pct := round(100 * N / sum(N), 2), by = .(group, comparison)]

fwrite(result, file.path(table_dir, paste0("grid_informed_mismatch", suffix, ".csv")))
fwrite(summary_dt, file.path(table_dir, paste0("grid_informed_mismatch_summary", suffix, ".csv")))
fwrite(global_dt, file.path(table_dir, paste0("grid_informed_global_moran", suffix, ".csv")))
fwrite(lisa_comparison, file.path(table_dir, paste0("lisa_comparison", suffix, ".csv")))

message("Wrote grid-informed mismatch and LISA comparison outputs.")
print(summary_dt)
print(global_dt)

if (run_mode == "full") {
  map_dt <- result[group == "caregiving_adults"]
  map_data <- merge(lsoas, map_dt, by = "LSOA21CD", all.x = FALSE, all.y = FALSE)
  river <- NULL
  if (file.exists(river_path)) {
    river <- st_transform(st_read(river_path, quiet = TRUE), st_crs(map_data))
  }
  borough <- aggregate(map_data[, "borough_name"], by = list(borough_name_group = map_data$borough_name), FUN = function(x) x[1])
  cluster_palette <- c(
    "High-high mismatch cluster" = "#B2182B",
    "Low-low surplus cluster" = "#2166AC",
    "High-low spatial outlier" = "#EF8A62",
    "Low-high spatial outlier" = "#67A9CF",
    "Not significant" = "#ECE7DF"
  )
  reclass_palette <- c(
    "stable high-high" = "#8C2D04",
    "new high-high" = "#D95F0E",
    "no longer high-high" = "#2B8CBE",
    "never high-high" = "#ECEBE7"
  )
  map_theme <- theme_void(base_size = 9) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "#555555"),
      legend.position = c(0.20, 0.20),
      legend.background = element_rect(fill = "white", colour = "#D0D0D0", linewidth = 0.2),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  lisa_map <- ggplot(map_data) +
    geom_sf(aes(fill = grid_mean_lisa_cluster), colour = NA) +
    geom_sf(data = borough, fill = NA, colour = "#FFFFFF", linewidth = 0.16) +
    scale_fill_manual(values = cluster_palette, name = NULL, drop = FALSE) +
    labs(
      title = "Grid-informed caregiving mismatch LISA",
      subtitle = "Local Moran's I clusters using grid-informed accessibility"
    ) +
    map_theme
  if (!is.null(river)) {
    lisa_map <- lisa_map + geom_sf(data = river, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45)
  }
  ggsave(file.path(map_dir, "grid_informed_lisa_map.png"), lisa_map, width = 7.2, height = 6.7, dpi = 300)
  ggsave(file.path(map_dir, "grid_informed_lisa_map.pdf"), lisa_map, width = 7.2, height = 6.7)

  reclass_map <- ggplot(map_data) +
    geom_sf(aes(fill = lisa_reclassification), colour = NA) +
    geom_sf(data = borough, fill = NA, colour = "#FFFFFF", linewidth = 0.16) +
    scale_fill_manual(values = reclass_palette, name = NULL, drop = FALSE) +
    labs(
      title = "LISA high-high reclassification",
      subtitle = "Original centroid-based result compared with grid-informed mismatch"
    ) +
    map_theme
  if (!is.null(river)) {
    reclass_map <- reclass_map + geom_sf(data = river, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45)
  }
  ggsave(file.path(map_dir, "lisa_reclassification_map.png"), reclass_map, width = 7.2, height = 6.7, dpi = 300)
  ggsave(file.path(map_dir, "lisa_reclassification_map.pdf"), reclass_map, width = 7.2, height = 6.7)
}

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
