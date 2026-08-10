# Computes nearest-facility travel times for each configured routing scenario.
# Reads: London network, LSOA origins and POI files.
# Writes: checkpointed and final R5r long/wide travel-time tables.

options(java.parameters = "-Xmx10G")

library(data.table)
library(r5r)
library(sf)

cmd_args <- commandArgs(trailingOnly = FALSE)
script_arg <- cmd_args[grepl("^-f$", cmd_args)]
script_path <- if (length(script_arg) == 1) {
  cmd_args[which(grepl("^-f$", cmd_args)) + 1]
} else {
  "scripts/model/01_route_r5r.R"
}
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "scripts", "model", "00_config.R"))

data_dir <- DATA_DIR
r5_dir <- R5_NETWORK_DIR
poi_dir <- POI_DIR
out_dir <- R5_OUTPUT_DIR
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if (length(args) >= 1) args[1] else "test"
if (!run_mode %in% c("test", "full")) {
  stop("First argument must be 'test' or 'full'.")
}

departure_datetime <- r5_departure_datetime
scenarios <- lapply(scenario_definitions, function(x) {
  x[c("mode", "max_trip_duration", "max_walk_time")]
})

make_origins <- function() {
  lsoas <- st_read(file.path(data_dir, "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
  points <- st_point_on_surface(lsoas)
  points <- st_transform(points, 4326)
  coords <- st_coordinates(points)
  origins <- data.table(
    id = points$LSOA21CD,
    LSOA21NM = points$LSOA21NM,
    lon = coords[, "X"],
    lat = coords[, "Y"]
  )

  if (run_mode == "test") {
    setorder(origins, id)
    origins <- origins[1:20]
  }
  origins
}

make_destinations <- function(category, file_name, origins) {
  destinations <- fread(file.path(poi_dir, file_name))
  destinations <- destinations[!is.na(lon) & !is.na(lat)]
  destinations[, id := paste(category, seq_len(.N), sep = "_")]
  destinations <- destinations[, .(id, name, lon, lat)]

  if (run_mode == "test") {
    origin_coords <- origins[, .(origin_lon = lon, origin_lat = lat)]
    destinations[, min_origin_distance_sq := {
      d <- rep(Inf, .N)
      for (i in seq_len(nrow(origin_coords))) {
        candidate <- (lon - origin_coords$origin_lon[i])^2 + (lat - origin_coords$origin_lat[i])^2
        d <- pmin(d, candidate)
      }
      d
    }]
    setorder(destinations, min_origin_distance_sq)
    destinations <- destinations[1:min(.N, 120)]
    destinations[, min_origin_distance_sq := NULL]
  }
  destinations
}

nearest_times <- function(ttm, origins, category, scenario_name) {
  if (nrow(ttm) == 0) {
    result <- origins[, .(id, LSOA21NM)]
    result[, `:=`(
      facility_category = category,
      scenario = scenario_name,
      nearest_travel_time_min = NA_real_,
      reachable_destination_count = 0L
    )]
    setnames(result, "id", "LSOA21CD")
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
      nearest_travel_time_min = min(get(tt_col), na.rm = TRUE),
      reachable_destination_count = .N
    ),
    by = .(origin_id = get(origin_col))
  ]
  nearest[is.infinite(nearest_travel_time_min), nearest_travel_time_min := NA_real_]

  result <- origins[, .(LSOA21CD = id, LSOA21NM)]
  result <- nearest[result, on = c(origin_id = "LSOA21CD")]
  setnames(result, "origin_id", "LSOA21CD")
  result[, facility_category := category]
  result[, scenario := scenario_name]
  result[is.na(reachable_destination_count), reachable_destination_count := 0L]
  setcolorder(
    result,
    c(
      "LSOA21CD",
      "LSOA21NM",
      "facility_category",
      "scenario",
      "nearest_travel_time_min",
      "reachable_destination_count"
    )
  )
  result
}

message("Setting up R5 network from: ", r5_dir)
r5_network <- setup_r5(data_path = r5_dir, verbose = TRUE, overwrite = FALSE)

origins <- make_origins()
message("Origins: ", nrow(origins))

all_results <- list()
for (category in names(facility_files)) {
  destinations <- make_destinations(category, facility_files[[category]], origins)
  message("Destinations for ", category, ": ", nrow(destinations))

  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]
    message("Running ", run_mode, ": ", category, " / ", scenario_name)

    ttm <- travel_time_matrix(
      r5r_network = r5_network,
      origins = origins[, .(id, lon, lat)],
      destinations = destinations[, .(id, lon, lat)],
      mode = scenario$mode,
      departure_datetime = departure_datetime,
      time_window = r5_time_window_min,
      percentiles = r5_percentile,
      max_walk_time = scenario$max_walk_time,
      max_trip_duration = scenario$max_trip_duration,
      walk_speed = r5_walk_speed_kmh,
      max_rides = r5_max_rides,
      n_threads = max(1L, parallel::detectCores(logical = FALSE) - 1L),
      progress = run_mode == "test"
    )

    all_results[[paste(category, scenario_name, sep = "__")]] <- nearest_times(
      ttm,
      origins,
      category,
      scenario_name
    )

    partial <- rbindlist(all_results, use.names = TRUE)
    fwrite(partial, file.path(out_dir, paste0("nearest_facility_times_", run_mode, "_long_partial.csv")))
  }
}

combined <- rbindlist(all_results, use.names = TRUE)
wide <- dcast(
  combined,
  LSOA21CD + LSOA21NM ~ facility_category + scenario,
  value.var = "nearest_travel_time_min"
)

combined_file <- file.path(out_dir, paste0("nearest_facility_times_", run_mode, "_long.csv"))
wide_file <- file.path(out_dir, paste0("nearest_facility_times_", run_mode, "_wide.csv"))
fwrite(combined, combined_file)
fwrite(wide, wide_file)

message("Saved: ", combined_file)
message("Saved: ", wide_file)
message("Done.")
