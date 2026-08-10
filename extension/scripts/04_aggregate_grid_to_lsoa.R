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
run_mode <- if ("--test" %in% args) "test" else "full"
suffix <- if (run_mode == "test") "_test" else ""

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
table_dir <- file.path(extension_dir, "outputs", "tables")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("04_aggregate_grid_to_lsoa_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("04_aggregate_grid_to_lsoa.R started at: ", started_at)
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

weighted_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  w <- w[keep]
  o <- order(x)
  x <- x[o]
  w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

basket_base <- file.path(intermediate_dir, paste0("grid_accessibility_summary", suffix))
grid_path <- required_file(file.path(intermediate_dir, paste0("london_grid_100m_residential_centroids", suffix, ".gpkg")))
lsoa_path <- required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg"))

basket <- read_table(basket_base)
grid <- st_read(grid_path, quiet = TRUE)
lsoas <- st_read(lsoa_path, quiet = TRUE)

required_fields(
  basket,
  c("grid_id", "lsoa21cd", "group", "mode", "basket_score"),
  "grid accessibility summary"
)
required_fields(
  grid,
  c("grid_id", "lsoa21cd", "residential_weight"),
  "residential grid centroids"
)
required_fields(lsoas, c("LSOA21CD", "LSOA21NM"), "LSOA boundary")

grid_attrs <- as.data.table(st_drop_geometry(grid))[
  ,
  .(grid_id, residential_weight)
]
basket <- merge(basket, grid_attrs, by = "grid_id", all.x = TRUE)
basket[is.na(residential_weight), residential_weight := 0]

message("Input grid accessibility rows: ", nrow(basket))
message("Unique grids: ", uniqueN(basket$grid_id))
message("Unique LSOAs in grid output: ", uniqueN(basket$lsoa21cd))

agg <- basket[
  ,
  {
    scores <- basket_score
    valid <- is.finite(scores)
    valid_scores <- scores[valid]
    valid_weights <- residential_weight[valid]
    n_valid <- length(valid_scores)
    lowest_n <- if (n_valid > 0) max(1L, ceiling(n_valid * 0.10)) else 0L
    lowest_scores <- if (n_valid > 0) sort(valid_scores)[seq_len(lowest_n)] else numeric()
    weighted_mean <- if (n_valid > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
      weighted.mean(valid_scores, valid_weights, na.rm = TRUE)
    } else {
      NA_real_
    }
    .(
      grid_mean = mean(valid_scores, na.rm = TRUE),
      grid_median = median(valid_scores, na.rm = TRUE),
      grid_p10 = as.numeric(quantile(valid_scores, probs = 0.10, na.rm = TRUE, names = FALSE, type = 7)),
      grid_lowest_decile_mean = mean(lowest_scores, na.rm = TRUE),
      grid_sd = sd(valid_scores, na.rm = TRUE),
      grid_iqr = IQR(valid_scores, na.rm = TRUE),
      grid_min = min(valid_scores, na.rm = TRUE),
      grid_max = max(valid_scores, na.rm = TRUE),
      valid_grid_count = n_valid,
      weighted_grid_mean = weighted_mean,
      weighted_grid_median = weighted_median(valid_scores, valid_weights),
      residential_weight_sum = sum(valid_weights, na.rm = TRUE)
    )
  },
  by = .(lsoa21cd, group, mode)
]

numeric_cols <- setdiff(names(agg), c("lsoa21cd", "group", "mode"))
for (col in numeric_cols) {
  set(agg, which(is.infinite(agg[[col]])), col, NA_real_)
}

lsoa_attrs <- as.data.table(st_drop_geometry(lsoas))[, .(lsoa21cd = LSOA21CD, LSOA21NM)]
out <- merge(lsoa_attrs, agg, by = "lsoa21cd", all.x = TRUE)

csv_path <- file.path(table_dir, paste0("lsoa_grid_aggregated_accessibility", suffix, ".csv"))
gpkg_path <- file.path(intermediate_dir, paste0("lsoa_grid_aggregated_accessibility", suffix, ".gpkg"))

fwrite(out, csv_path)

gpkg_data <- merge(lsoas, agg, by.x = "LSOA21CD", by.y = "lsoa21cd", all.x = TRUE)
if (file.exists(gpkg_path)) {
  message("Replacing previous extension output file: ", gpkg_path)
  invisible(file.remove(gpkg_path))
}
st_write(gpkg_data, gpkg_path, quiet = TRUE)

qa_input <- out[!is.na(group) & !is.na(mode)]
qa <- qa_input[
  ,
  .(
    lsoa_rows = .N,
    missing_accessibility_rows = sum(is.na(valid_grid_count)),
    zero_valid_grid_rows = sum(!is.na(valid_grid_count) & valid_grid_count == 0),
    mean_valid_grid_count = round(mean(valid_grid_count, na.rm = TRUE), 2),
    median_valid_grid_count = round(median(valid_grid_count, na.rm = TRUE), 2)
  ),
  by = .(group, mode)
]
qa_path <- file.path(table_dir, paste0("lsoa_grid_aggregation_qa", suffix, ".csv"))
fwrite(qa, qa_path)

message("Wrote LSOA aggregated accessibility table: ", csv_path)
message("Wrote LSOA aggregated accessibility GPKG: ", gpkg_path)
message("Wrote QA table: ", qa_path)
print(qa)

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
