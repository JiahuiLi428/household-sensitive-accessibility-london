options(java.parameters = "-Xmx10G")

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(r5r)
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
setwd(project_root)
source(file.path("scripts", "model", "00_config.R"))

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--full" %in% args) "full" else "test"
parse_arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^", flag, "="), "", hit[1])
}
chunk_size <- as.integer(parse_arg_value("--chunk-size", if (run_mode == "full") "10000" else "1000000"))
if (!is.finite(chunk_size) || chunk_size < 1) {
  stop("--chunk-size must be a positive integer.")
}

extension_dir <- project_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
table_dir <- file.path(extension_dir, "outputs", "tables")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("03_run_grid_accessibility_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("03_run_grid_accessibility.R started at: ", started_at)
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

make_origins <- function() {
  suffix <- if (run_mode == "test") "_test" else ""
  centroid_path <- required_file(file.path(intermediate_dir, paste0("london_grid_100m_residential_centroids", suffix, ".gpkg")))
  centroids <- st_read(centroid_path, quiet = TRUE)
  required_fields(
    centroids,
    c("grid_id", "lsoa21cd", "centroid_x", "centroid_y", "residential_flag", "residential_weight"),
    "residential grid centroids"
  )
  centroids <- centroids[centroids$residential_flag %in% TRUE, ]
  if (nrow(centroids) == 0) {
    stop("No residential/preliminary grid centroids available for test.")
  }
  centroids_4326 <- st_transform(centroids, 4326)
  coords <- st_coordinates(centroids_4326)
  origins <- data.table(
    id = centroids_4326$grid_id,
    grid_id = centroids_4326$grid_id,
    lsoa21cd = centroids_4326$lsoa21cd,
    lon = coords[, "X"],
    lat = coords[, "Y"],
    residential_weight = centroids_4326$residential_weight,
    residential_filter_method = centroids_4326$residential_filter_method
  )
  required_fields(origins, c("id", "grid_id", "lsoa21cd", "lon", "lat"), "origins")
  setorder(origins, lsoa21cd, grid_id)
  origins
}

make_destinations <- function(category, file_name, origins) {
  path <- required_file(file.path(POI_DIR, file_name))
  destinations <- fread(path)
  required_fields(destinations, c("lon", "lat"), paste0("POI file ", file_name))
  destinations <- destinations[!is.na(lon) & !is.na(lat)]
  destinations[, id := paste(category, seq_len(.N), sep = "_")]
  if (!"name" %in% names(destinations)) {
    destinations[, name := id]
  }
  destinations <- destinations[, .(id, name, lon, lat)]

  if (run_mode == "test") {
    # Test mode uses a local destination subset for speed only.
    origin_coords <- origins[, .(origin_lon = lon, origin_lat = lat)]
    destinations[, min_origin_distance_sq := {
      d <- rep(Inf, .N)
      for (i in seq_len(nrow(origin_coords))) {
        candidate <- (lon - origin_coords$origin_lon[i])^2 + (lat - origin_coords$origin_lat[i])^2
        d <- pmin(d, candidate)
      }
      d
    }]
    cap <- if (category == "transport_bus_stops") 1000L else 300L
    setorder(destinations, min_origin_distance_sq)
    destinations <- destinations[1:min(.N, cap)]
    destinations[, min_origin_distance_sq := NULL]
  }
  destinations
}

nearest_times <- function(ttm, origins, category, scenario_name) {
  result <- origins[, .(grid_id, lsoa21cd)]
  if (nrow(ttm) == 0) {
    result[, `:=`(
      facility_type = category,
      mode = scenario_name,
      travel_time = NA_real_,
      reachable_destination_count = 0L
    )]
    return(result)
  }
  tt_col <- intersect(c("travel_time", "travel_time_p50"), names(ttm))[1]
  if (is.na(tt_col)) {
    stop("Could not find travel time column in r5r output.")
  }
  origin_ids <- origins$id
  from_matches <- sum(ttm$from_id %in% origin_ids)
  to_matches <- sum(ttm$to_id %in% origin_ids)
  origin_col <- if (from_matches >= to_matches) "from_id" else "to_id"

  nearest <- as.data.table(ttm)[
    ,
    .(
      travel_time = suppressWarnings(min(get(tt_col), na.rm = TRUE)),
      reachable_destination_count = .N
    ),
    by = .(grid_id = get(origin_col))
  ]
  nearest[is.infinite(travel_time), travel_time := NA_real_]

  result <- nearest[result, on = "grid_id"]
  result[, `:=`(
    facility_type = category,
    mode = scenario_name
  )]
  result[is.na(reachable_destination_count), reachable_destination_count := 0L]
  result
}

write_table <- function(dt, base_path) {
  rds_path <- paste0(base_path, ".rds")
  saveRDS(dt, rds_path)
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(dt, paste0(base_path, ".parquet"))
    message("Wrote Parquet: ", paste0(base_path, ".parquet"))
  } else {
    message("arrow is not available; wrote RDS output instead of Parquet for: ", base_path)
  }
  if (run_mode == "test") {
    fwrite(dt, paste0(base_path, ".csv"))
  }
}

read_table <- function(base_path) {
  parquet_path <- paste0(base_path, ".parquet")
  rds_path <- paste0(base_path, ".rds")
  csv_path <- paste0(base_path, ".csv")
  if (file.exists(parquet_path)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Parquet input exists but package 'arrow' is not available: ", parquet_path)
    }
    return(as.data.table(arrow::read_parquet(parquet_path)))
  }
  if (file.exists(rds_path)) {
    return(as.data.table(readRDS(rds_path)))
  }
  if (file.exists(csv_path)) {
    return(fread(csv_path))
  }
  stop("No supported table input exists for base path: ", base_path)
}

table_exists <- function(base_path) {
  file.exists(paste0(base_path, ".parquet")) ||
    file.exists(paste0(base_path, ".rds")) ||
    file.exists(paste0(base_path, ".csv"))
}

score_grid_results <- function(raw_long, group_name, weights) {
  long <- copy(raw_long)
  long[, group := group_name]
  long[, facility_score_name := facility_score_map[facility_type]]
  threshold_map <- vapply(scenario_definitions, function(x) x$threshold_min, numeric(1))
  long[, threshold_min := threshold_map[mode]]
  long[, facility_score := score_time(travel_time, threshold_min)]
  long[, weight := as.numeric(weights[facility_score_name])]
  long[, weighted_score := facility_score * weight]

  basket <- long[
    ,
    .(
      basket_score = sum(weighted_score, na.rm = TRUE),
      reached_facility_count = sum(!is.na(travel_time)),
      missing_facility_count = sum(is.na(travel_time)),
      mean_travel_time = mean(travel_time, na.rm = TRUE)
    ),
    by = .(grid_id, lsoa21cd, group, mode)
  ]
  long <- merge(long, basket[, .(grid_id, group, mode, basket_score)], by = c("grid_id", "group", "mode"), all.x = TRUE)
  setcolorder(
    long,
    c(
      "grid_id", "lsoa21cd", "group", "mode", "facility_type", "travel_time",
      "facility_score", "weighted_score", "basket_score",
      "reachable_destination_count", "weight", "threshold_min"
    )
  )
  list(long = long, basket = basket)
}

origins <- make_origins()
origin_filter_method <- paste(unique(origins$residential_filter_method), collapse = "; ")
message("Origins: ", nrow(origins), " grid centroids across ", uniqueN(origins$lsoa21cd), " LSOAs.")
message("Origin filter method: ", origin_filter_method)
message("Chunk size: ", chunk_size)

scenario_names <- c("walk_15min", "walk_transit_30min")
group_name <- "caregiving_adults"
weights <- caregiving_weights

message("Setting up R5 network from: ", R5_NETWORK_DIR)
r5_network <- setup_r5(data_path = R5_NETWORK_DIR, verbose = TRUE, overwrite = FALSE)

origin_chunks <- split(origins, ceiling(seq_len(nrow(origins)) / chunk_size))
message("Origin chunks: ", length(origin_chunks))

suffix <- if (run_mode == "test") "_test" else ""
partial_dir <- file.path(intermediate_dir, paste0("grid_accessibility_partials", suffix))
dir.create(partial_dir, recursive = TRUE, showWarnings = FALSE)

for (chunk_index in seq_along(origin_chunks)) {
  chunk_origins <- as.data.table(origin_chunks[[chunk_index]])
  chunk_long_base <- file.path(partial_dir, sprintf("grid_accessibility_long_chunk_%03d", chunk_index))
  chunk_basket_base <- file.path(partial_dir, sprintf("grid_accessibility_summary_chunk_%03d", chunk_index))
  if (table_exists(chunk_long_base) && table_exists(chunk_basket_base)) {
    message("Skipping completed chunk ", chunk_index, "/", length(origin_chunks), ": ", chunk_long_base)
    next
  }

  message("Processing chunk ", chunk_index, "/", length(origin_chunks), " origins: ", nrow(chunk_origins))
  chunk_results <- list()

  for (category in names(facility_files)) {
    destinations <- make_destinations(category, facility_files[[category]], chunk_origins)
    message("Destinations for ", category, ": ", nrow(destinations))

    for (scenario_name in scenario_names) {
      scenario <- scenario_definitions[[scenario_name]]
      message("Running grid routing: chunk ", chunk_index, " / ", category, " / ", scenario_name)
      ttm <- travel_time_matrix(
        r5r_network = r5_network,
        origins = chunk_origins[, .(id, lon, lat)],
        destinations = destinations[, .(id, lon, lat)],
        mode = scenario$mode,
        departure_datetime = r5_departure_datetime,
        time_window = r5_time_window_min,
        percentiles = r5_percentile,
        max_walk_time = scenario$max_walk_time,
        max_trip_duration = scenario$max_trip_duration,
        walk_speed = r5_walk_speed_kmh,
        max_rides = r5_max_rides,
        n_threads = max(1L, parallel::detectCores(logical = FALSE) - 1L),
        progress = run_mode == "test"
      )
      key <- paste(category, scenario_name, sep = "__")
      chunk_results[[key]] <- nearest_times(
        ttm,
        chunk_origins,
        category,
        scenario_name
      )
      rm(ttm)
      invisible(gc())
    }
  }

  raw_chunk_long <- rbindlist(chunk_results, use.names = TRUE)
  scored <- score_grid_results(raw_chunk_long, group_name, weights)
  write_table(scored$long, chunk_long_base)
  write_table(scored$basket, chunk_basket_base)
  message("Completed and wrote chunk ", chunk_index, "/", length(origin_chunks))
  rm(chunk_results, raw_chunk_long, scored)
  invisible(gc())
}

chunk_long_bases <- file.path(partial_dir, sprintf("grid_accessibility_long_chunk_%03d", seq_along(origin_chunks)))
chunk_basket_bases <- file.path(partial_dir, sprintf("grid_accessibility_summary_chunk_%03d", seq_along(origin_chunks)))
missing_chunks <- which(!vapply(chunk_long_bases, table_exists, logical(1)) | !vapply(chunk_basket_bases, table_exists, logical(1)))
if (length(missing_chunks) > 0) {
  stop("Missing partial chunk outputs: ", paste(missing_chunks, collapse = ", "))
}

message("Combining chunk outputs.")
long <- rbindlist(lapply(chunk_long_bases, read_table), use.names = TRUE)
basket <- rbindlist(lapply(chunk_basket_bases, read_table), use.names = TRUE)

summary_dt <- basket[
  ,
  .(
    grid_count = .N,
    mean_basket_score = round(mean(basket_score, na.rm = TRUE), 2),
    median_basket_score = round(median(basket_score, na.rm = TRUE), 2),
    min_basket_score = round(min(basket_score, na.rm = TRUE), 2),
    max_basket_score = round(max(basket_score, na.rm = TRUE), 2),
    mean_missing_facilities = round(mean(missing_facility_count, na.rm = TRUE), 2),
    any_na_basket = any(is.na(basket_score))
  ),
  by = .(lsoa21cd, group, mode)
]

long_base <- file.path(intermediate_dir, paste0("grid_accessibility_long", suffix))
summary_base <- file.path(intermediate_dir, paste0("grid_accessibility_summary", suffix))
write_table(long, long_base)
write_table(basket, summary_base)
fwrite(summary_dt, file.path(table_dir, paste0("grid_accessibility", suffix, "_summary.csv")))

finished_at <- Sys.time()
elapsed <- round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2)
gc_info <- capture.output(gc())

report_path <- file.path(log_dir, paste0("grid_accessibility", suffix, "_report.md"))
report <- c(
  paste0("# Grid accessibility ", run_mode, " report"),
  "",
  paste0("Run time: ", format(started_at), " to ", format(finished_at)),
  paste0("Elapsed seconds: ", elapsed),
  "",
  "## Inputs",
  "",
  paste0("- Origins: ", nrow(origins), " OSM residential-filtered 100m grid centroids"),
  if (run_mode == "test") {
    paste0("- LSOAs: ", paste(unique(origins$lsoa21cd), collapse = ", "))
  } else {
    paste0("- LSOAs represented by residential grid origins: ", uniqueN(origins$lsoa21cd))
  },
  paste0("- Residential filter method: ", origin_filter_method),
  paste0("- Origin chunks: ", length(origin_chunks), " (chunk size ", chunk_size, ")"),
  "- Group: caregiving adults",
  "- Modes: walk_15min; walk_transit_30min",
  if (run_mode == "test") {
    "- Destination subset: nearest 300 per non-bus facility and nearest 1000 bus stops for this test run only."
  } else {
    "- Destination subset: none; all POIs in the original facility files were used."
  },
  "",
  "## Output checks",
  "",
  paste0("- Long rows: ", nrow(long)),
  paste0("- Summary rows: ", nrow(basket)),
  paste0("- Any NA basket score: ", any(is.na(basket$basket_score))),
  paste0("- Any infinite travel time: ", any(is.infinite(long$travel_time))),
  "",
  "## LSOA/mode summary",
  "",
  paste(capture.output(print(summary_dt)), collapse = "\n"),
  "",
  "## Memory snapshot from gc()",
  "",
  "```text",
  gc_info,
  "```",
  "",
  "## Notes",
  "",
  if (run_mode == "test") {
    "- This is a test run only."
  } else {
    "- This is the first full-London grid run for caregiving adults only."
  },
  if (requireNamespace("arrow", quietly = TRUE)) {
    "- `arrow` is installed; outputs were written as Parquet plus RDS copies."
  } else {
    "- `arrow` is not currently installed, so outputs were written as RDS rather than Parquet."
  },
  "- Residential filtering uses OSM residential building and residential land-use tags. This is better than all-grid origins but remains a proxy, not a population grid."
)
writeLines(report, report_path, useBytes = TRUE)

message("Wrote long test output base: ", long_base)
message("Wrote summary test output base: ", summary_base)
message("Wrote table summary: ", file.path(table_dir, "grid_accessibility_test_summary.csv"))
message("Wrote test report: ", report_path)
message("Finished at: ", finished_at)
message("Elapsed seconds: ", elapsed)
