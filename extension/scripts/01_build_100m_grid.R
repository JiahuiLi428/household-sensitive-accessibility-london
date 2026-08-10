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
if (!run_mode %in% c("test", "full")) {
  stop("run_mode must be 'test' or 'full'.")
}

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
table_dir <- file.path(extension_dir, "outputs", "tables")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("01_build_100m_grid_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("01_build_100m_grid.R started at: ", started_at)
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

lsoa_path <- required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg"))
lsoas <- st_read(lsoa_path, quiet = TRUE)
required_fields(lsoas, c("LSOA21CD", "LSOA21NM"), "LSOA boundary")

if (is.na(st_crs(lsoas)$epsg) || st_crs(lsoas)$epsg != 27700) {
  message("Transforming LSOA boundary to EPSG:27700 for grid construction.")
  lsoas <- st_transform(lsoas, 27700)
}

lsoas <- st_make_valid(lsoas)
lsoas[, "lsoa21cd"] <- lsoas$LSOA21CD

case_lsoas <- c(
  "E01000095", "E01000094", "E01034478", "E01000096",
  "E01000090", "E01034476", "E01000014"
)
inner_london_boroughs <- c(
  "Camden", "City of London", "Greenwich", "Hackney",
  "Hammersmith and Fulham", "Islington", "Kensington and Chelsea",
  "Lambeth", "Lewisham", "Southwark", "Tower Hamlets",
  "Wandsworth", "Westminster"
)

lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
lsoas$london_ring <- ifelse(lsoas$borough_name %in% inner_london_boroughs, "Inner London", "Outer London")

if (run_mode == "test") {
  inner_id <- lsoas$LSOA21CD[lsoas$london_ring == "Inner London"][1]
  outer_id <- lsoas$LSOA21CD[
    lsoas$london_ring == "Outer London" &
      lsoas$borough_name != "Barking and Dagenham" &
      !(lsoas$LSOA21CD %in% case_lsoas)
  ][1]
  barking_id <- case_lsoas[case_lsoas %in% lsoas$LSOA21CD][1]
  selected_ids <- unique(c(inner_id, outer_id, barking_id))
  study_lsoas <- lsoas[lsoas$LSOA21CD %in% selected_ids, ]
  message("Test LSOAs: ", paste(study_lsoas$LSOA21CD, collapse = ", "))
} else {
  study_lsoas <- lsoas
}

if (nrow(study_lsoas) == 0) {
  stop("No LSOAs selected for grid construction.")
}

study_union <- st_union(st_geometry(study_lsoas))
study_union <- st_make_valid(st_as_sf(data.frame(id = 1), geometry = st_sfc(study_union, crs = st_crs(lsoas))))

message("Creating 100m grid over selected study area bounding box.")
grid_geom <- st_make_grid(study_union, cellsize = c(100, 100), square = TRUE, what = "polygons")
bbox_grid_count <- length(grid_geom)
grid <- st_sf(grid_tmp_id = seq_along(grid_geom), geometry = grid_geom, crs = st_crs(lsoas))

grid_centroids <- st_centroid(grid)
inside_matrix <- st_intersects(grid_centroids, study_union, sparse = FALSE)
inside_idx <- as.logical(inside_matrix[, 1])
retained_grid <- grid[inside_idx, ]
retained_centroids <- grid_centroids[inside_idx, ]

message("Bounding-box grid cells: ", bbox_grid_count)
message("Retained cells with centroid inside selected study area: ", nrow(retained_grid))
message("Cells excluded because centroid falls outside selected study area: ", bbox_grid_count - nrow(retained_grid))

joined_centroids <- st_join(
  retained_centroids,
  study_lsoas[, c("LSOA21CD", "LSOA21NM", "borough_name", "london_ring")],
  join = st_within,
  left = TRUE
)

coords <- st_coordinates(joined_centroids)
joined_dt <- as.data.table(st_drop_geometry(joined_centroids))
joined_dt[, centroid_x := coords[, "X"]]
joined_dt[, centroid_y := coords[, "Y"]]
joined_dt[, lsoa21cd := LSOA21CD]

unmatched_count <- sum(is.na(joined_dt$lsoa21cd))
message("Centroid-to-LSOA unmatched grid cells: ", unmatched_count)

retained_grid$grid_id <- sprintf("grid_%s_%07d", ifelse(run_mode == "test", "test", "lon"), seq_len(nrow(retained_grid)))
retained_grid$lsoa21cd <- joined_dt$lsoa21cd
retained_grid$centroid_x <- joined_dt$centroid_x
retained_grid$centroid_y <- joined_dt$centroid_y

centroid_out <- st_sf(
  grid_id = retained_grid$grid_id,
  lsoa21cd = retained_grid$lsoa21cd,
  centroid_x = retained_grid$centroid_x,
  centroid_y = retained_grid$centroid_y,
  geometry = st_geometry(joined_centroids),
  crs = st_crs(lsoas)
)

grid_out <- retained_grid[, c("grid_id", "lsoa21cd", "centroid_x", "centroid_y", "geometry")]

area_values <- as.numeric(st_area(grid_out))
area_summary <- data.table(
  metric = c(
    "run_mode",
    "selected_lsoa_count",
    "bbox_grid_count",
    "retained_grid_count",
    "unmatched_grid_count",
    "expected_grid_area_sqm",
    "min_grid_area_sqm",
    "median_grid_area_sqm",
    "max_grid_area_sqm"
  ),
  value = c(
    run_mode,
    as.character(nrow(study_lsoas)),
    as.character(bbox_grid_count),
    as.character(nrow(grid_out)),
    as.character(unmatched_count),
    "10000",
    sprintf("%.2f", min(area_values, na.rm = TRUE)),
    sprintf("%.2f", median(area_values, na.rm = TRUE)),
    sprintf("%.2f", max(area_values, na.rm = TRUE))
  )
)

lsoa_grid_counts <- as.data.table(st_drop_geometry(study_lsoas))[
  ,
  .(lsoa21cd = LSOA21CD, LSOA21NM, borough_name, london_ring)
]
grid_count_dt <- as.data.table(st_drop_geometry(grid_out))[
  ,
  .(
    grid_count = .N,
    unmatched_grid_count = sum(is.na(lsoa21cd)),
    min_grid_area_sqm = round(min(area_values), 2),
    median_grid_area_sqm = round(median(area_values), 2),
    max_grid_area_sqm = round(max(area_values), 2)
  ),
  by = lsoa21cd
]
lsoa_grid_counts <- merge(lsoa_grid_counts, grid_count_dt, by = "lsoa21cd", all.x = TRUE)
lsoa_grid_counts[is.na(grid_count), `:=`(
  grid_count = 0L,
  unmatched_grid_count = 0L,
  min_grid_area_sqm = NA_real_,
  median_grid_area_sqm = NA_real_,
  max_grid_area_sqm = NA_real_
)]

suffix <- if (run_mode == "test") "_test" else ""
grid_path <- file.path(intermediate_dir, paste0("london_grid_100m", suffix, ".gpkg"))
centroid_path <- file.path(intermediate_dir, paste0("london_grid_100m_centroids", suffix, ".gpkg"))
summary_path <- file.path(table_dir, paste0("grid_summary", suffix, ".csv"))
area_summary_path <- file.path(table_dir, paste0("grid_area_summary", suffix, ".csv"))

if (file.exists(grid_path)) {
  message("Replacing previous extension test/output file: ", grid_path)
  invisible(file.remove(grid_path))
}
if (file.exists(centroid_path)) {
  message("Replacing previous extension test/output file: ", centroid_path)
  invisible(file.remove(centroid_path))
}

st_write(grid_out, grid_path, quiet = TRUE)
st_write(centroid_out, centroid_path, quiet = TRUE)
fwrite(lsoa_grid_counts, summary_path)
fwrite(area_summary, area_summary_path)

message("Wrote grid polygons: ", grid_path)
message("Wrote grid centroids: ", centroid_path)
message("Wrote LSOA grid summary: ", summary_path)
message("Wrote area/run summary: ", area_summary_path)

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
