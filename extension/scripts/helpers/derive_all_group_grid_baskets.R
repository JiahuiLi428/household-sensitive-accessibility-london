suppressPackageStartupMessages({
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
setwd(project_root)
source(file.path("scripts", "model", "00_config.R"))

extension_dir <- project_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("derive_all_group_grid_baskets_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("derive_all_group_grid_baskets.R started at: ", started_at)

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

read_table <- function(base_path) {
  parquet_path <- paste0(base_path, ".parquet")
  rds_path <- paste0(base_path, ".rds")
  if (file.exists(parquet_path)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Parquet input exists but package 'arrow' is not available: ", parquet_path)
    }
    return(as.data.table(arrow::read_parquet(parquet_path)))
  }
  if (file.exists(rds_path)) {
    return(as.data.table(readRDS(rds_path)))
  }
  stop("No supported input exists for base path: ", base_path)
}

write_table <- function(dt, base_path) {
  saveRDS(dt, paste0(base_path, ".rds"))
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(dt, paste0(base_path, ".parquet"))
  } else {
    warning("Package 'arrow' is unavailable; wrote RDS only: ", base_path)
  }
}

long_base <- file.path(intermediate_dir, "grid_accessibility_long")
summary_base <- file.path(intermediate_dir, "grid_accessibility_summary")
required_file(paste0(long_base, ".parquet"))
required_file(paste0(summary_base, ".parquet"))

care_long_backup <- file.path(intermediate_dir, "grid_accessibility_long_caregiving_only")
care_summary_backup <- file.path(intermediate_dir, "grid_accessibility_summary_caregiving_only")
if (!file.exists(paste0(care_long_backup, ".parquet"))) {
  file.copy(paste0(long_base, ".parquet"), paste0(care_long_backup, ".parquet"), overwrite = FALSE)
}
if (!file.exists(paste0(care_summary_backup, ".parquet"))) {
  file.copy(paste0(summary_base, ".parquet"), paste0(care_summary_backup, ".parquet"), overwrite = FALSE)
}
if (!file.exists(paste0(care_long_backup, ".rds")) && file.exists(paste0(long_base, ".rds"))) {
  file.copy(paste0(long_base, ".rds"), paste0(care_long_backup, ".rds"), overwrite = FALSE)
}
if (!file.exists(paste0(care_summary_backup, ".rds")) && file.exists(paste0(summary_base, ".rds"))) {
  file.copy(paste0(summary_base, ".rds"), paste0(care_summary_backup, ".rds"), overwrite = FALSE)
}

long <- read_table(long_base)
required_fields(
  long,
  c("grid_id", "lsoa21cd", "mode", "facility_type", "travel_time", "reachable_destination_count"),
  "grid accessibility long table"
)

base_long <- unique(long[, .(
  grid_id,
  lsoa21cd,
  mode,
  facility_type,
  travel_time,
  reachable_destination_count
)])
base_long[, facility_score_name := facility_score_map[facility_type]]
threshold_map <- vapply(scenario_definitions, function(x) x$threshold_min, numeric(1))
base_long[, threshold_min := threshold_map[mode]]
base_long[, facility_score := score_time(travel_time, threshold_min)]

group_weights <- list(
  children_and_adolescents = service_basket_weights$family,
  caregiving_adults = caregiving_weights,
  older_adults = service_basket_weights$older
)

all_long <- list()
all_basket <- list()
for (group_name in names(group_weights)) {
  weights <- group_weights[[group_name]]
  group_long <- copy(base_long)
  group_long[, group := group_name]
  group_long[, weight := as.numeric(weights[facility_score_name])]
  group_long[, weighted_score := facility_score * weight]
  group_basket <- group_long[
    ,
    .(
      basket_score = sum(weighted_score, na.rm = TRUE),
      reached_facility_count = sum(!is.na(travel_time)),
      missing_facility_count = sum(is.na(travel_time)),
      mean_travel_time = mean(travel_time, na.rm = TRUE)
    ),
    by = .(grid_id, lsoa21cd, group, mode)
  ]
  group_long <- merge(
    group_long,
    group_basket[, .(grid_id, group, mode, basket_score)],
    by = c("grid_id", "group", "mode"),
    all.x = TRUE
  )
  setcolorder(
    group_long,
    c(
      "grid_id", "lsoa21cd", "group", "mode", "facility_type", "travel_time",
      "facility_score", "weighted_score", "basket_score",
      "reachable_destination_count", "weight", "threshold_min",
      "facility_score_name"
    )
  )
  all_long[[group_name]] <- group_long
  all_basket[[group_name]] <- group_basket
  message("Derived grid baskets for group: ", group_name)
}

long_out <- rbindlist(all_long, use.names = TRUE)
basket_out <- rbindlist(all_basket, use.names = TRUE)
write_table(long_out, long_base)
write_table(basket_out, summary_base)

message("Wrote all-group long rows: ", nrow(long_out))
message("Wrote all-group summary rows: ", nrow(basket_out))
message("Backed up caregiving-only output base: ", care_long_backup)

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
