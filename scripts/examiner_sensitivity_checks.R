library(data.table)
library(sf)
library(spdep)

source(file.path("scripts", "model", "00_config.R"))

out_dir <- file.path(ANALYSIS_DIR, "examiner_checks")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(ANALYSIS_DIR, "r5r_accessibility_enriched_wide.csv"))
add_facility_scores(wide)
add_care_mobility_scores(wide)
wide[, care_access_z := zscore(care_walk_index)]

lsoa <- st_read(project_path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
lsoa <- st_make_valid(lsoa)
g0 <- merge(lsoa[, "LSOA21CD"], wide, by = "LSOA21CD", all = FALSE)
g0 <- g0[!is.na(g0$care_access_z), ]
nb <- poly2nb(g0, queen = TRUE)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

weight_sets <- list(
  baseline_050_030_020 = c(0.50, 0.30, 0.20),
  equal_thirds = c(1/3, 1/3, 1/3),
  household_dominant_060_020_020 = c(0.60, 0.20, 0.20),
  balanced_refinement_040_030_030 = c(0.40, 0.30, 0.30)
)

components <- c(
  "caregiver_household_intensity",
  "lone_parent_intensity",
  "children_pct"
)
component_matrix <- sapply(components, function(x) rescale_0_100(g0[[x]]))

classify_lisa <- function(mismatch) {
  xc <- mismatch - mean(mismatch, na.rm = TRUE)
  lagx <- lag.listw(listw, xc, zero.policy = TRUE)
  local <- localmoran(mismatch, listw, zero.policy = TRUE)
  p <- as.numeric(local[, "Pr(z != E(Ii))"])
  p < 0.05 & xc > 0 & lagx > 0
}

scenario_records <- list()
scenario_members <- list()
for (nm in names(weight_sets)) {
  w <- weight_sets[[nm]]
  need <- as.numeric(component_matrix %*% w)
  need_z <- zscore(need)
  mismatch <- need_z - g0$care_access_z
  hh <- classify_lisa(mismatch)
  high_need_low_access <- need_z >= median(need_z, na.rm = TRUE) &
    g0$care_access_z < median(g0$care_access_z, na.rm = TRUE)
  scenario_records[[nm]] <- data.table(
    weighting = nm,
    caregiver_household_weight = w[1],
    lone_parent_weight = w[2],
    children_share_weight = w[3],
    high_high_n = sum(hh),
    high_need_low_access_n = sum(high_need_low_access),
    mismatch_mean = mean(mismatch),
    mismatch_sd = sd(mismatch)
  )
  scenario_members[[nm]] <- data.table(
    LSOA21CD = g0$LSOA21CD,
    mismatch = mismatch,
    high_high = hh,
    high_need_low_access = high_need_low_access
  )
}

baseline <- scenario_members[["baseline_050_030_020"]]
summary_dt <- rbindlist(scenario_records)
summary_dt[, `:=`(
  mismatch_correlation_with_baseline = sapply(
    weighting,
    function(nm) cor(scenario_members[[nm]]$mismatch, baseline$mismatch)
  ),
  high_high_jaccard_with_baseline = sapply(
    weighting,
    function(nm) {
      x <- scenario_members[[nm]]$high_high
      y <- baseline$high_high
      sum(x & y) / sum(x | y)
    }
  ),
  quadrant_jaccard_with_baseline = sapply(
    weighting,
    function(nm) {
      x <- scenario_members[[nm]]$high_need_low_access
      y <- baseline$high_need_low_access
      sum(x & y) / sum(x | y)
    }
  )
)]
fwrite(summary_dt, file.path(out_dir, "care_need_weight_sensitivity_summary.csv"))

long <- fread(file.path(ANALYSIS_DIR, "r5r_accessibility_enriched_long.csv"))
run_kw <- function(dt) {
  dt <- dt[
    !is.na(IMD_Decile) & !is.na(nearest_travel_time_min)
  ]
  if (nrow(dt) == 0L || uniqueN(dt$IMD_Decile) < 2L) {
    return(data.table(n = nrow(dt), eta_sq_h = NA_real_))
  }
  kt <- kruskal.test(nearest_travel_time_min ~ factor(IMD_Decile), data = dt)
  k <- uniqueN(dt$IMD_Decile)
  data.table(
    n = nrow(dt),
    H = as.numeric(kt$statistic),
    p_value = kt$p.value,
    eta_sq_h = as.numeric((kt$statistic - k + 1) / (nrow(dt) - k))
  )
}

kw_all <- long[, run_kw(.SD), by = .(facility_category, scenario)]
kw_direct <- long[
  IMD_Source == "direct_2011_lsoa_code",
  run_kw(.SD),
  by = .(facility_category, scenario)
]
kw <- merge(
  kw_all,
  kw_direct,
  by = c("facility_category", "scenario"),
  suffixes = c("_all", "_direct_only")
)
kw[, eta_sq_change_direct_minus_all := eta_sq_h_direct_only - eta_sq_h_all]
fwrite(kw, file.path(out_dir, "imd_bestfit_exclusion_kruskal.csv"))

# Reproduce Appendix C model structure with and without best-fit IMD cases.
lsoa_names <- st_read(project_path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
lsoa_names$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoa_names$LSOA21NM)
lsoa_names$london_ring <- ifelse(
  lsoa_names$borough_name %in% inner_london_boroughs,
  "Inner London",
  "Outer London"
)
wide_reg <- merge(
  wide,
  as.data.table(st_drop_geometry(lsoa_names))[, .(LSOA21CD, london_ring)],
  by = "LSOA21CD",
  all.x = TRUE
)
add_need_scores(wide_reg)
wide_reg[, older_access_score := weighted_index(wide_reg, service_basket_weights$older, "walk")]
wide_reg[, children_access_score := weighted_index(wide_reg, service_basket_weights$family, "walk")]
wide_reg[, older_mismatch_z := zscore(older_need_score) - zscore(older_access_score)]
wide_reg[, gp_time := fifelse(
  is.na(healthcare_gps_walk_15min),
  16,
  healthcare_gps_walk_15min
)]

fit_models <- function(dt, sample_name) {
  model_dt <- dt[, .(
    gp_time,
    older_access_score,
    children_access_score,
    older_mismatch_z,
    older_adults_pct,
    children_pct,
    IMD_Decile,
    london_ring
  )]
  model_dt <- model_dt[complete.cases(model_dt)]
  model_dt[, london_ring := factor(london_ring, levels = c("Inner London", "Outer London"))]
  models <- list(
    gp_time = lm(gp_time ~ older_adults_pct + children_pct + IMD_Decile + london_ring, data = model_dt),
    older_access = lm(older_access_score ~ older_adults_pct + children_pct + IMD_Decile + london_ring, data = model_dt),
    children_access = lm(children_access_score ~ older_adults_pct + children_pct + IMD_Decile + london_ring, data = model_dt),
    older_mismatch = lm(older_mismatch_z ~ older_adults_pct + children_pct + IMD_Decile + london_ring, data = model_dt)
  )
  rbindlist(lapply(names(models), function(nm) {
    m <- models[[nm]]
    co <- as.data.table(summary(m)$coefficients, keep.rownames = "term")
    setnames(co, c("term", "estimate", "std_error", "t_value", "p_value"))
    co[, `:=`(
      sample = sample_name,
      model = nm,
      r_squared = summary(m)$r.squared,
      n = nobs(m)
    )]
    co
  }), use.names = TRUE)
}

reg_all <- fit_models(wide_reg, "all_imd_matches")
reg_direct <- fit_models(
  wide_reg[IMD_Source == "direct_2011_lsoa_code"],
  "direct_matches_only"
)
reg <- rbindlist(list(reg_all, reg_direct), use.names = TRUE)
fwrite(reg, file.path(out_dir, "imd_bestfit_exclusion_regressions.csv"))

print(summary_dt)
print(kw[, .(
  max_abs_eta_change = max(abs(eta_sq_change_direct_minus_all), na.rm = TRUE),
  mean_abs_eta_change = mean(abs(eta_sq_change_direct_minus_all), na.rm = TRUE)
)])
print(reg[term == "IMD_Decile", .(
  sample, model, estimate, p_value, r_squared, n
)])
