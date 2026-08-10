# Tests whether need-access mismatch and service-basket accessibility differ
# by IMD decile separately within each household-sensitive group (RQ2).
# Reads: step 04 mismatch indices and step 03 composite accessibility indices.
# Writes: a 9-test Kruskal-Wallis + Benjamini-Hochberg FDR summary table.

library(data.table)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
examiner_dir <- file.path(analysis_dir, "examiner_checks")
dir.create(examiner_dir, recursive = TRUE, showWarnings = FALSE)

mismatch <- fread(file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv"))
access <- fread(file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv"))

# Kruskal-Wallis test of a continuous outcome across IMD deciles, with
# rank-based eta-squared (eta_sq_h), mirroring the formula and column
# conventions used for the 16-test facility-level family in
# scripts/03_service_baskets.R and scripts/additional_robustness_20260806.R.
kw_by_imd_decile <- function(dt, value_col, group_label, scenario_label) {
  sub <- dt[!is.na(IMD_Decile) & !is.na(get(value_col))]
  if (nrow(sub) == 0 || uniqueN(sub$IMD_Decile) < 2) {
    return(data.table(
      group = group_label, scenario = scenario_label,
      H = NA_real_, df = NA_real_, p_value = NA_real_,
      eta_sq_h = NA_real_, n = nrow(sub)
    ))
  }
  kt <- kruskal.test(sub[[value_col]] ~ factor(sub$IMD_Decile))
  k <- uniqueN(sub$IMD_Decile)
  n <- nrow(sub)
  eta <- max(0, (as.numeric(kt$statistic) - k + 1) / (n - k))
  data.table(
    group = group_label, scenario = scenario_label,
    H = as.numeric(kt$statistic),
    df = as.numeric(kt$parameter),
    p_value = as.numeric(kt$p.value),
    eta_sq_h = eta,
    n = n
  )
}

# --- Mismatch: one test per group (walk scenario only; the mismatch index
# has no transit-scenario counterpart in the current pipeline). ---
mismatch_tests <- rbind(
  kw_by_imd_decile(mismatch, "family_mismatch_index", "Children and Adolescents", NA_character_),
  kw_by_imd_decile(mismatch, "working_mismatch_index", "Caregiving Adults", NA_character_),
  kw_by_imd_decile(mismatch, "older_mismatch_index", "Older adults", NA_character_)
)
mismatch_tests[, analysis := "mismatch_by_imd_decile"]

# --- Accessibility: one test per group x scenario (3 groups x 2 scenarios). ---
access_tests <- rbind(
  kw_by_imd_decile(access, "family_walk_index", "Children and Adolescents", "Walk 15"),
  kw_by_imd_decile(access, "family_transit_index", "Children and Adolescents", "Walk + Transit 30"),
  kw_by_imd_decile(access, "working_walk_index", "Caregiving Adults", "Walk 15"),
  kw_by_imd_decile(access, "working_transit_index", "Caregiving Adults", "Walk + Transit 30"),
  kw_by_imd_decile(access, "older_walk_index", "Older adults", "Walk 15"),
  kw_by_imd_decile(access, "older_transit_index", "Older adults", "Walk + Transit 30")
)
access_tests[, analysis := "access_by_imd_decile"]

# Pool as one exploratory 9-test family (3 mismatch + 6 access x scenario),
# analogous in spirit to the existing 16-test facility-level family, but
# reported and FDR-corrected as its own family rather than merged with it.
group_level_tests <- rbind(mismatch_tests, access_tests, use.names = TRUE)
setcolorder(group_level_tests, c("analysis", "group", "scenario", "H", "df", "p_value", "eta_sq_h", "n"))
group_level_tests[, q_bh_9 := p.adjust(p_value, method = "BH")]
group_level_tests[, significant_bh := q_bh_9 < 0.05]

out_path <- file.path(examiner_dir, "rq2_group_level_kw_fdr.csv")
fwrite(group_level_tests, out_path)

message("RQ2 group-level Kruskal-Wallis + BH-FDR family (n = ", nrow(group_level_tests), " tests)")
print(group_level_tests[, .(analysis, group, scenario, H, p_value, q_bh_9, eta_sq_h, n, significant_bh)])
message("Written to: ", out_path)
