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

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
table_dir <- file.path(extension_dir, "outputs", "tables")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("08_select_case_studies_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("08_select_case_studies.R started at: ", started_at)

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

standardise <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    return(rep(0, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / s
}

case_lsoas <- c(
  "E01000095", "E01000094", "E01034478", "E01000096",
  "E01000090", "E01034476", "E01000014"
)

compound <- fread(required_file(file.path(table_dir, "compound_mismatch.csv")))
mismatch <- fread(required_file(file.path(table_dir, "grid_informed_mismatch.csv")))
comparison <- fread(required_file(file.path(table_dir, "centroid_grid_comparison.csv")))
care <- fread(required_file(rel_path("data", "output", "analysis", "care_mobility_accessibility_lsoa.csv")))
continuous <- fread(required_file(rel_path("data", "output", "analysis", "continuous_need_access_mismatch_indices.csv")))
lsoas <- st_read(required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg")), quiet = TRUE)

required_fields(compound, c("LSOA21CD", "compound_count", "compound_type", "child_hh", "caregiver_hh", "older_hh"), "compound mismatch")
required_fields(mismatch, c("LSOA21CD", "group", "grid_mean_mismatch", "grid_mean_lisa_cluster", "grid_mean_access", "grid_lowest_decile_access"), "grid-informed mismatch")
required_fields(comparison, c("lsoa21cd", "group", "mode", "centroid_score", "grid_informed_score", "grid_lowest_decile_mean", "within_lsoa_sd"), "centroid-grid comparison")
required_fields(care, c("LSOA21CD", "LSOA21NM", "borough_name", "london_ring", "population_total", "IMD_Decile", "care_need_score"), "care mobility output")
required_fields(continuous, c("LSOA21CD", "children_need_score", "older_need_score"), "continuous mismatch output")

lsoa_area <- as.data.table(st_drop_geometry(lsoas))
lsoa_area[, area_sqkm := as.numeric(st_area(lsoas)) / 1e6]

base <- merge(
  care[, .(LSOA21CD, LSOA21NM, borough_name, london_ring, population_total, IMD_Decile, care_need_score)],
  continuous[, .(LSOA21CD, child_need = children_need_score, older_need = older_need_score)],
  by = "LSOA21CD",
  all.x = TRUE
)
base <- merge(base, lsoa_area[, .(LSOA21CD, area_sqkm)], by = "LSOA21CD", all.x = TRUE)
base[, population_density := population_total / area_sqkm]
base[, outer_inner_london := ifelse(london_ring == "Inner London", 0, 1)]
base <- merge(
  base,
  compound[, .(LSOA21CD, compound_count, compound_type, child_hh, caregiver_hh, older_hh)],
  by = "LSOA21CD",
  all.x = TRUE
)
base[is.na(compound_count), `:=`(
  compound_count = 0L,
  compound_type = "none",
  child_hh = 0L,
  caregiver_hh = 0L,
  older_hh = 0L
)]

care_mismatch <- mismatch[group == "caregiving_adults", .(
  LSOA21CD,
  caregiving_grid_mean_mismatch = grid_mean_mismatch,
  caregiving_lisa = grid_mean_lisa_cluster,
  caregiving_grid_access = grid_mean_access,
  caregiving_lowest_decile_access = grid_lowest_decile_access
)]
base <- merge(base, care_mismatch, by = "LSOA21CD", all.x = TRUE)
care_compare <- comparison[group == "caregiving_adults" & mode == "walk_15min", .(
  LSOA21CD = lsoa21cd,
  caregiving_centroid_access = centroid_score,
  caregiving_grid_access_compare = grid_informed_score,
  caregiving_lowest_decile_compare = grid_lowest_decile_mean,
  within_lsoa_variation = within_lsoa_sd
)]
base <- merge(base, care_compare, by = "LSOA21CD", all.x = TRUE)

facility_deficits <- NULL
long_path <- file.path(intermediate_dir, "grid_accessibility_long.parquet")
if (file.exists(long_path) && requireNamespace("arrow", quietly = TRUE)) {
  long <- as.data.table(arrow::read_parquet(long_path))
  facility_deficits <- long[
    group == "caregiving_adults" & mode == "walk_15min",
    .(mean_facility_score = mean(facility_score, na.rm = TRUE)),
    by = .(LSOA21CD = lsoa21cd, facility_type)
  ][order(LSOA21CD, mean_facility_score)]
  facility_deficits <- facility_deficits[, .(
    weakest_facility_category = facility_type[1],
    weakest_facility_score = round(mean_facility_score[1], 2)
  ), by = LSOA21CD]
}
if (is.null(facility_deficits)) {
  facility_deficits <- data.table(LSOA21CD = base$LSOA21CD, weakest_facility_category = NA_character_, weakest_facility_score = NA_real_)
}
base <- merge(base, facility_deficits, by = "LSOA21CD", all.x = TRUE)

case1_profile <- base[LSOA21CD %in% case_lsoas]
case1_anchor <- case1_profile[
  ,
  .(
    care_need_score = mean(care_need_score, na.rm = TRUE),
    child_need = mean(child_need, na.rm = TRUE),
    older_need = mean(older_need, na.rm = TRUE),
    IMD_Decile = mean(IMD_Decile, na.rm = TRUE),
    population_density = mean(population_density, na.rm = TRUE),
    outer_inner_london = mean(outer_inner_london, na.rm = TRUE)
  )
]

case2_pool <- base[
  !(LSOA21CD %in% case_lsoas) &
    compound_count >= 2 &
    caregiving_lisa == "High-high mismatch cluster" &
    is.finite(within_lsoa_variation)
]
case2_pool[, rank_score :=
  0.40 * standardise(compound_count) +
    0.30 * standardise(within_lsoa_variation) +
    0.20 * standardise(caregiving_grid_mean_mismatch) +
    0.10 * standardise(-weakest_facility_score)
]
case2_candidates <- case2_pool[order(-rank_score)][1:min(.N, 5)]
case2_candidates[, candidate_case := "Case 2 compound mismatch"]
case2_candidates[, ranking_reason := paste0(
  "compound_count=", compound_count,
  "; caregiving LISA high-high; within-LSOA SD=", round(within_lsoa_variation, 2),
  "; weakest category=", weakest_facility_category
)]

match_vars <- c("care_need_score", "child_need", "older_need", "IMD_Decile", "population_density", "outer_inner_london")
case3_pool <- base[
  !(LSOA21CD %in% c(case_lsoas, case2_candidates$LSOA21CD[1])) &
    compound_count == 0 &
    caregiving_lisa != "High-high mismatch cluster" &
    caregiving_grid_access > quantile(caregiving_grid_access, 0.60, na.rm = TRUE) &
    caregiving_grid_mean_mismatch < median(caregiving_grid_mean_mismatch, na.rm = TRUE)
]
for (v in match_vars) {
  s <- sd(base[[v]], na.rm = TRUE)
  case3_pool[, paste0(v, "_dist") := if (is.finite(s) && s > 0) ((get(v) - case1_anchor[[v]]) / s)^2 else 0]
}
dist_cols <- paste0(match_vars, "_dist")
case3_pool[, match_distance := sqrt(rowSums(.SD, na.rm = TRUE)), .SDcols = dist_cols]
case3_candidates <- case3_pool[order(match_distance, -caregiving_grid_access)][1:min(.N, 5)]
case3_candidates[, candidate_case := "Case 3 positive counter-case"]
case3_candidates[, ranking_reason := paste0(
  "nearest-neighbour need match distance=", round(match_distance, 3),
  "; higher grid access=", round(caregiving_grid_access, 2),
  "; not high-high; compound_type=", compound_type
)]

case1_out <- case1_profile[, .(
  candidate_case = "Case 1 Barking Riverside / Thames View",
  LSOA21CD,
  LSOA21NM,
  borough_name,
  care_need_score,
  child_need,
  older_need,
  IMD_Decile,
  population_density,
  caregiving_centroid_access,
  caregiving_grid_access,
  caregiving_lowest_decile_access,
  within_lsoa_variation,
  caregiving_grid_mean_mismatch,
  caregiving_lisa,
  compound_count,
  compound_type,
  weakest_facility_category,
  weakest_facility_score,
  ranking_reason = "Fixed case retained by design brief; Barking Riverside / Thames View LSOA set."
)]

candidate_cols <- names(case1_out)
case2_out <- case2_candidates[, ..candidate_cols]
case3_out <- case3_candidates[, ..candidate_cols]
candidates <- rbindlist(list(case1_out, case2_out, case3_out), use.names = TRUE, fill = TRUE)

candidate_path <- file.path(table_dir, "case_study_candidates.csv")
fwrite(candidates, candidate_path)

final_selection <- rbindlist(list(
  case1_out[, .SD[which.max(caregiving_grid_mean_mismatch)], by = candidate_case],
  case2_out[1],
  case3_out[1]
), use.names = TRUE, fill = TRUE)
final_selection[, selection_status := "pending human confirmation"]
final_path <- file.path(table_dir, "final_case_selection.csv")
fwrite(final_selection, final_path)

message("Wrote case-study candidates: ", candidate_path)
message("Wrote provisional final selection: ", final_path)
message("Top Case 2 candidates:")
print(case2_out[, .(LSOA21CD, LSOA21NM, borough_name, compound_count, compound_type, within_lsoa_variation, weakest_facility_category)][1:min(.N, 5)])
message("Top Case 3 candidates:")
print(case3_out[, .(LSOA21CD, LSOA21NM, borough_name, compound_count, compound_type, caregiving_grid_access, caregiving_grid_mean_mismatch)][1:min(.N, 5)])

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
