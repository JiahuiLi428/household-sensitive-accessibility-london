# Recalculates WALK-15 service-basket scores under group-specific walking speeds.
# Reads: enriched LSOA accessibility output from step 02.
# Writes: reproducible summary and caregiving-diagnostic CSV files.

library(data.table)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))

time_col_by_score <- setNames(
  paste0(model_facilities$facility_category, "_walk_15min"),
  model_facilities$score_name
)

walking_index <- function(dt, weights, speed_kmh) {
  score_dt <- as.data.table(lapply(names(weights), function(score_name) {
    travel_time <- dt[[time_col_by_score[[score_name]]]] * r5_walk_speed_kmh / speed_kmh
    score_time(travel_time, scenario_definitions$walk_15min$threshold_min)
  }))
  setnames(score_dt, names(weights))
  as.numeric(as.matrix(score_dt) %*% unname(weights))
}

groups <- list(
  "Children and Adolescents" = list(weights = service_basket_weights$family, speed = 3.24),
  "Caregiving Adults" = list(weights = service_basket_weights$working, speed = 3.24),
  "Older adults" = list(weights = service_basket_weights$older, speed = 2.70)
)

summary_rows <- list()
index_values <- list()

for (group_name in names(groups)) {
  group <- groups[[group_name]]
  baseline <- walking_index(wide, group$weights, r5_walk_speed_kmh)
  sensitivity <- walking_index(wide, group$weights, group$speed)
  index_values[[group_name]] <- list(baseline = baseline, sensitivity = sensitivity)
  summary_rows[[group_name]] <- data.table(
    group = group_name,
    n_lsoas = sum(is.finite(baseline) & is.finite(sensitivity)),
    baseline_speed_kmh = r5_walk_speed_kmh,
    sensitivity_speed_kmh = group$speed,
    baseline_mean = round(mean(baseline, na.rm = TRUE), 2),
    sensitivity_mean = round(mean(sensitivity, na.rm = TRUE), 2),
    absolute_change = round(mean(sensitivity - baseline, na.rm = TRUE), 2)
  )
}

summary_dt <- rbindlist(summary_rows)
fwrite(summary_dt, file.path(analysis_dir, "walking_speed_sensitivity_summary.csv"))

care_baseline <- index_values[["Caregiving Adults"]]$baseline
care_sensitivity <- index_values[["Caregiving Adults"]]$sensitivity
case_ids <- c(
  "E01000095", "E01000094", "E01034478", "E01000096",
  "E01000090", "E01034476", "E01000014"
)
case_selector <- wide$LSOA21CD %chin% case_ids

diagnostics <- data.table(
  metric = c(
    "share_lsoas_below_20_pct",
    "barking_case_mean_score"
  ),
  baseline = c(
    round(mean(care_baseline < 20, na.rm = TRUE) * 100, 2),
    round(mean(care_baseline[case_selector], na.rm = TRUE), 2)
  ),
  sensitivity = c(
    round(mean(care_sensitivity < 20, na.rm = TRUE) * 100, 2),
    round(mean(care_sensitivity[case_selector], na.rm = TRUE), 2)
  )
)
fwrite(diagnostics, file.path(analysis_dir, "walking_speed_sensitivity_caregiving_diagnostics.csv"))

print(summary_dt)
print(diagnostics)
