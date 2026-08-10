# Enriches R5r outputs with Census and IMD context and creates QA summaries.
# Reads: step 01 routing outputs plus Census, IMD and lookup data.
# Writes: enriched long/wide analysis tables and meeting QA figures.

library(data.table)
library(ggplot2)
library(readxl)
library(sf)

source(file.path("scripts", "model", "00_config.R"))

data_dir <- DATA_DIR
output_dir <- ANALYSIS_DIR
figure_dir <- MEETING_FIGURE_DIR
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

long <- fread(file.path(data_dir, "output", "r5r", "nearest_facility_times_full_long.csv"))
wide <- fread(file.path(data_dir, "output", "r5r", "nearest_facility_times_full_wide.csv"))

age <- fread(file.path(data_dir, "census", "TS007B_age_broad_bands_london_lsoa_2021.csv"))
households <- fread(file.path(data_dir, "census", "TS003_household_composition_london_lsoa_2021_enhanced.csv"))
imd <- as.data.table(read_excel(
  file.path(data_dir, "imd", "File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"),
  sheet = "IMD2019"
))
lookup <- fread(file.path(data_dir, "raw", "lookups", "LSOA11_LSOA21_LAD22_EW_bestfit_v2.csv"))

setnames(
  imd,
  old = c(
    "LSOA code (2011)",
    "LSOA name (2011)",
    "Index of Multiple Deprivation (IMD) Rank",
    "Index of Multiple Deprivation (IMD) Decile"
  ),
  new = c("LSOA11CD", "LSOA11NM", "IMD_Rank", "IMD_Decile_2011")
)

imd_bestfit <- lookup[, .(LSOA11CD, LSOA21CD)][
  imd[, .(LSOA11CD, IMD_Rank, IMD_Decile_2011)],
  on = "LSOA11CD"
][
  ,
  .(
    IMD_Rank_bestfit = mean(IMD_Rank, na.rm = TRUE),
    IMD_Decile_bestfit_mean = mean(IMD_Decile_2011, na.rm = TRUE),
    IMD_source_lsoa11_count = uniqueN(LSOA11CD)
  ),
  by = LSOA21CD
]

imd_direct <- imd[, .(
  LSOA21CD = LSOA11CD,
  IMD_Rank_direct = IMD_Rank,
  IMD_Decile_direct = IMD_Decile_2011
)]

age[, children_0_15_count :=
  `Aged 4 years and under` + `Aged 5 to 9 years` + `Aged 10 to 15 years`
]
age[, older_adults_65_plus_count :=
  `Aged 65 to 74 years` + `Aged 75 to 84 years` + `Aged 85 years and over`
]
age[, children_pct := children_0_15_count / Total * 100]
age[, older_adults_pct := older_adults_65_plus_count / Total * 100]

demographics <- age[, .(
  LSOA21CD,
  population_total = Total,
  children_0_15_count,
  children_pct,
  older_adults_65_plus_count,
  older_adults_pct
)]
demographics <- merge(demographics, households, by = "LSOA21CD", all.x = TRUE)
demographics <- merge(demographics, imd_direct, by = "LSOA21CD", all.x = TRUE)
demographics <- merge(demographics, imd_bestfit, by = "LSOA21CD", all.x = TRUE)

demographics[, IMD_Decile := fifelse(!is.na(IMD_Decile_direct), IMD_Decile_direct, round(IMD_Decile_bestfit_mean))]
demographics[, IMD_Decile := pmax(1, pmin(10, IMD_Decile))]
demographics[, IMD_Rank := fifelse(!is.na(IMD_Rank_direct), IMD_Rank_direct, IMD_Rank_bestfit)]
demographics[, IMD_Source := fifelse(
  !is.na(IMD_Decile_direct),
  "direct_2011_lsoa_code",
  fifelse(!is.na(IMD_Rank_bestfit), "lsoa11_to_lsoa21_bestfit", "missing_imd2019_for_lsoa21")
)]

metadata_cols <- c(
  "LSOA21CD",
  "population_total",
  "children_0_15_count",
  "children_pct",
  "older_adults_65_plus_count",
  "older_adults_pct",
  "household_total",
  "household_single_person_aged_66_over",
  "household_married_civil_couple_with_dependent_children",
  "household_cohabiting_couple_with_dependent_children",
  "household_couple_with_dependent_children",
  "household_lone_parent_with_dependent_children",
  "household_other_with_dependent_children",
  "household_dependent_children_total",
  "IMD_Rank",
  "IMD_Decile",
  "IMD_Source",
  "IMD_source_lsoa11_count"
)
demographics <- demographics[, ..metadata_cols]

long_enriched <- demographics[long, on = "LSOA21CD"]
wide_enriched <- demographics[wide, on = "LSOA21CD"]

fwrite(long_enriched, file.path(output_dir, "r5r_accessibility_enriched_long.csv"))
fwrite(wide_enriched, file.path(output_dir, "r5r_accessibility_enriched_wide.csv"))

summary_by_group <- long[
  ,
  .(
    n_lsoas = .N,
    reachable_lsoas = sum(!is.na(nearest_travel_time_min)),
    missing_lsoas = sum(is.na(nearest_travel_time_min)),
    missing_pct = round(mean(is.na(nearest_travel_time_min)) * 100, 1),
    zero_min_count = sum(nearest_travel_time_min == 0, na.rm = TRUE),
    min_time = as.numeric(min(nearest_travel_time_min, na.rm = TRUE)),
    median_time = as.numeric(median(nearest_travel_time_min, na.rm = TRUE)),
    mean_time = as.numeric(mean(nearest_travel_time_min, na.rm = TRUE)),
    max_time = as.numeric(max(nearest_travel_time_min, na.rm = TRUE))
  ),
  by = .(facility_category, scenario)
]
summary_by_group[is.infinite(min_time), `:=`(min_time = NA_real_, max_time = NA_real_)]
fwrite(summary_by_group, file.path(output_dir, "r5r_result_quality_summary.csv"))

imd_match_summary <- data.table(
  metric = c(
    "lsoa_rows",
    "direct_imd_matches",
    "bestfit_imd_matches",
    "remaining_missing_imd",
    "bestfit_assigned_lsoas"
  ),
  value = c(
    nrow(wide),
    sum(!is.na(demographics$IMD_Rank) & demographics$IMD_Source == "direct_2011_lsoa_code"),
    sum(!is.na(demographics$IMD_Rank)),
    sum(is.na(demographics$IMD_Rank)),
    sum(demographics$IMD_Source == "lsoa11_to_lsoa21_bestfit", na.rm = TRUE)
  )
)
fwrite(imd_match_summary, file.path(output_dir, "imd_match_summary.csv"))

boundaries <- st_read(file.path(data_dir, "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
map_data <- merge(boundaries, wide_enriched, by = "LSOA21CD", all.x = TRUE)

plot_access_map <- function(field, title, output_name) {
  p <- ggplot(map_data) +
    geom_sf(aes(fill = .data[[field]]), colour = NA) +
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      na.value = "grey88",
      name = "Minutes"
    ) +
    labs(title = title, subtitle = "Nearest facility travel time, WALK 15-minute scenario") +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      legend.position = "right"
    )
  ggsave(file.path(figure_dir, output_name), p, width = 8, height = 7, dpi = 300)
}

plot_access_map(
  "healthcare_gps_walk_15min",
  "Walking Accessibility to GPs within London LSOAs",
  "fig1_gp_walking_accessibility_map.png"
)

plot_access_map(
  "food_retail_supermarkets_walk_15min",
  "Walking Accessibility to Supermarkets within London LSOAs",
  "fig2_supermarket_walking_accessibility_map.png"
)

gp_imd <- long_enriched[
  facility_category == "healthcare_gps" &
    scenario == "walk_15min" &
    !is.na(IMD_Decile) &
    !is.na(nearest_travel_time_min)
]

boxplot_gp <- ggplot(gp_imd, aes(x = factor(IMD_Decile), y = nearest_travel_time_min)) +
  geom_boxplot(fill = "#8ecae6", colour = "#123047", outlier.alpha = 0.35, width = 0.65) +
  labs(
    title = "GP Travel Time by IMD Decile",
    x = "IMD Decile (1 = most deprived, 10 = least deprived)",
    y = "Nearest GP travel time (minutes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(
  file.path(figure_dir, "fig3_gp_travel_time_by_imd_decile.png"),
  boxplot_gp,
  width = 8,
  height = 5,
  dpi = 300
)

print(summary(long$nearest_travel_time_min))
print(table(long$facility_category))
print(table(long$scenario))
print(summary_by_group)
print(imd_match_summary)
