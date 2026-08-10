suppressPackageStartupMessages({
  library(sf)
  library(data.table)
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

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--full" %in% args) "full" else "test"

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
table_dir <- file.path(extension_dir, "outputs", "tables")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("02_filter_residential_grid_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("02_filter_residential_grid.R started at: ", started_at)
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

suffix <- if (run_mode == "test") "_test" else ""
grid_path <- required_file(file.path(intermediate_dir, paste0("london_grid_100m", suffix, ".gpkg")))
centroid_path <- required_file(file.path(intermediate_dir, paste0("london_grid_100m_centroids", suffix, ".gpkg")))
thames_path <- rel_path("data", "boundaries", "river_thames_osm.geojson")
osm_residential_path <- file.path(intermediate_dir, "osm_residential_features.gpkg")

grid <- st_read(grid_path, quiet = TRUE)
centroids <- st_read(centroid_path, quiet = TRUE)
required_fields(grid, c("grid_id", "lsoa21cd", "centroid_x", "centroid_y"), "100m grid")
required_fields(centroids, c("grid_id", "lsoa21cd", "centroid_x", "centroid_y"), "100m grid centroids")

message("Input grid cells: ", nrow(grid))

grid$residential_flag <- FALSE
grid$residential_weight <- 0
grid$residential_filter_method <- NA_character_
grid$filter_limitation <- NA_character_

centroids$residential_flag <- FALSE
centroids$residential_weight <- 0
centroids$residential_filter_method <- NA_character_
centroids$filter_limitation <- NA_character_

thames_removed <- 0L
feature_mode <- "fallback"

if (file.exists(osm_residential_path)) {
  feature_mode <- "osm_residential_buildings_and_landuse"
  message("Using OSM residential features: ", osm_residential_path)
  buildings <- st_read(osm_residential_path, layer = "residential_buildings", quiet = TRUE)
  landuse <- st_read(osm_residential_path, layer = "residential_landuse", quiet = TRUE)
  buildings <- st_transform(st_make_valid(buildings), st_crs(grid))
  landuse <- st_transform(st_make_valid(landuse), st_crs(grid))

  grid_bbox <- st_as_sfc(st_bbox(grid))
  buildings <- buildings[lengths(st_intersects(buildings, grid_bbox)) > 0, ]
  landuse <- landuse[lengths(st_intersects(landuse, grid_bbox)) > 0, ]
  message("Residential buildings intersecting grid bbox: ", nrow(buildings))
  message("Residential landuse polygons intersecting grid bbox: ", nrow(landuse))

  building_area <- data.table(grid_id = grid$grid_id, residential_building_area_sqm = 0)
  if (nrow(buildings) > 0) {
    building_hits <- suppressWarnings(st_intersection(
      grid[, c("grid_id")],
      buildings[, c("feature_class")]
    ))
    if (nrow(building_hits) > 0) {
      area_dt <- data.table(
        grid_id = building_hits$grid_id,
        area_sqm = as.numeric(st_area(building_hits))
      )
      building_area <- area_dt[, .(residential_building_area_sqm = sum(area_sqm, na.rm = TRUE)), by = grid_id]
    }
  }

  landuse_flag <- data.table(grid_id = grid$grid_id, residential_landuse_flag = FALSE)
  if (nrow(landuse) > 0) {
    landuse_hits <- lengths(st_intersects(grid, landuse)) > 0
    landuse_flag$residential_landuse_flag <- landuse_hits
  }

  attrs <- merge(
    data.table(grid_id = grid$grid_id),
    building_area,
    by = "grid_id",
    all.x = TRUE
  )
  attrs <- merge(attrs, landuse_flag, by = "grid_id", all.x = TRUE)
  attrs[is.na(residential_building_area_sqm), residential_building_area_sqm := 0]
  attrs[is.na(residential_landuse_flag), residential_landuse_flag := FALSE]
  attrs[, residential_flag := residential_building_area_sqm > 0 | residential_landuse_flag]
  attrs[, residential_weight := fifelse(
    residential_building_area_sqm > 0,
    residential_building_area_sqm,
    fifelse(residential_landuse_flag, 1, 0)
  )]

  grid$residential_building_area_sqm <- attrs$residential_building_area_sqm
  grid$residential_landuse_flag <- attrs$residential_landuse_flag
  grid$residential_flag <- attrs$residential_flag
  grid$residential_weight <- attrs$residential_weight
  grid$residential_filter_method <- "osm_residential_building_area_or_residential_landuse"
  grid$filter_limitation <- "OSM residential tags are incomplete and building footprint area is not population or floor area."

  centroids$residential_building_area_sqm <- attrs$residential_building_area_sqm
  centroids$residential_landuse_flag <- attrs$residential_landuse_flag
  centroids$residential_flag <- attrs$residential_flag
  centroids$residential_weight <- attrs$residential_weight
  centroids$residential_filter_method <- "osm_residential_building_area_or_residential_landuse"
  centroids$filter_limitation <- "OSM residential tags are incomplete and building footprint area is not population or floor area."
} else if (file.exists(thames_path)) {
  message("OSM residential features not found; falling back to preliminary all-grid-minus-Thames-buffer filter.")
  grid$residential_flag <- TRUE
  grid$residential_weight <- 1
  grid$residential_filter_method <- "preliminary_all_grid_minus_thames_buffer"
  grid$filter_limitation <- "No full-London residential building, residential land-use or population raster available in current project."

  centroids$residential_flag <- TRUE
  centroids$residential_weight <- 1
  centroids$residential_filter_method <- "preliminary_all_grid_minus_thames_buffer"
  centroids$filter_limitation <- "No full-London residential building, residential land-use or population raster available in current project."

  thames <- st_read(thames_path, quiet = TRUE)
  thames <- st_transform(thames, st_crs(grid))
  # Only a Thames centreline is available, not a waterbody polygon. The buffer is
  # deliberately flagged as preliminary and conservative for testing.
  thames_buffer <- st_buffer(st_union(st_geometry(thames)), dist = 120)
  in_thames_buffer <- as.logical(st_intersects(centroids, thames_buffer, sparse = FALSE)[, 1])
  thames_removed <- sum(in_thames_buffer, na.rm = TRUE)
  message("Cells flagged inside approximate Thames buffer: ", thames_removed)
  if (thames_removed > 0) {
    grid$residential_flag[in_thames_buffer] <- FALSE
    grid$residential_weight[in_thames_buffer] <- 0
    centroids$residential_flag[in_thames_buffer] <- FALSE
    centroids$residential_weight[in_thames_buffer] <- 0
  }
} else {
  message("Thames reference file not found; no water exclusion applied.")
  grid$residential_flag <- TRUE
  grid$residential_weight <- 1
  grid$residential_filter_method <- "preliminary_all_grid_no_filter"
  grid$filter_limitation <- "No residential or waterbody layer available."

  centroids$residential_flag <- TRUE
  centroids$residential_weight <- 1
  centroids$residential_filter_method <- "preliminary_all_grid_no_filter"
  centroids$filter_limitation <- "No residential or waterbody layer available."
}

res_grid <- grid[grid$residential_flag %in% TRUE, ]
res_centroids <- centroids[centroids$residential_flag %in% TRUE, ]

message("Residential-valid grid cells retained: ", nrow(res_grid))
message("Grid cells excluded by preliminary filter: ", nrow(grid) - nrow(res_grid))

out_grid_path <- file.path(intermediate_dir, paste0("london_grid_100m_residential", suffix, ".gpkg"))
out_centroid_path <- file.path(intermediate_dir, paste0("london_grid_100m_residential_centroids", suffix, ".gpkg"))
summary_path <- file.path(table_dir, paste0("residential_grid_summary", suffix, ".csv"))

if (file.exists(out_grid_path)) {
  message("Replacing previous extension test/output file: ", out_grid_path)
  invisible(file.remove(out_grid_path))
}
if (file.exists(out_centroid_path)) {
  message("Replacing previous extension test/output file: ", out_centroid_path)
  invisible(file.remove(out_centroid_path))
}

st_write(res_grid, out_grid_path, quiet = TRUE)
st_write(res_centroids, out_centroid_path, quiet = TRUE)

summary <- as.data.table(st_drop_geometry(grid))[
  ,
  .(
    total_grid_count = .N,
    retained_grid_count = sum(residential_flag),
    excluded_grid_count = sum(!residential_flag),
    residential_weight_sum = sum(residential_weight, na.rm = TRUE),
    residential_building_area_sqm = if ("residential_building_area_sqm" %in% names(.SD)) {
      sum(residential_building_area_sqm, na.rm = TRUE)
    } else {
      NA_real_
    },
    residential_landuse_grid_count = if ("residential_landuse_flag" %in% names(.SD)) {
      sum(residential_landuse_flag, na.rm = TRUE)
    } else {
      NA_integer_
    },
    filter_method = unique(residential_filter_method)[1],
    limitation = unique(filter_limitation)[1]
  ),
  by = lsoa21cd
]
summary[, run_mode := run_mode]
summary[, thames_buffer_removed_total := thames_removed]
summary[, feature_mode := feature_mode]
fwrite(summary, summary_path)

message("Wrote preliminary residential grid: ", out_grid_path)
message("Wrote preliminary residential centroids: ", out_centroid_path)
message("Wrote residential grid summary: ", summary_path)

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
