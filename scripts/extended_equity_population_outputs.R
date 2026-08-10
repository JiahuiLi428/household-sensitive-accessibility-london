library(data.table)
library(ggplot2)
library(sf)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "meeting")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

long <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_long.csv"))
wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
boundaries <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
map_data <- merge(boundaries, wide, by = "LSOA21CD", all.x = TRUE)

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

plot_imd_boxplot <- function(facility, title, output_name, fill) {
  dt <- long[
    facility_category == facility &
      scenario == "walk_15min" &
      !is.na(IMD_Decile) &
      !is.na(nearest_travel_time_min)
  ]
  p <- ggplot(dt, aes(x = factor(IMD_Decile), y = nearest_travel_time_min)) +
    geom_boxplot(fill = fill, colour = "#2f2f2f", outlier.alpha = 0.35, width = 0.65) +
    labs(
      title = title,
      x = "IMD Decile (1 = most deprived, 10 = least deprived)",
      y = "Nearest travel time (minutes)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 14))
  ggsave(file.path(figure_dir, output_name), p, width = 8, height = 5, dpi = 300)
}

plot_access_map(
  "education_schools_walk_15min",
  "Walking Accessibility to Schools within London LSOAs",
  "fig5_school_walking_accessibility_map.png"
)
plot_access_map(
  "healthcare_pharmacies_walk_15min",
  "Walking Accessibility to Pharmacies within London LSOAs",
  "fig6_pharmacy_walking_accessibility_map.png"
)
plot_access_map(
  "green_space_parks_walk_15min",
  "Walking Accessibility to Parks within London LSOAs",
  "fig7_park_walking_accessibility_map.png"
)

plot_imd_boxplot(
  "healthcare_pharmacies",
  "Pharmacy Travel Time by IMD Decile",
  "fig8_pharmacy_travel_time_by_imd_decile.png",
  "#90be6d"
)
plot_imd_boxplot(
  "green_space_parks",
  "Park Travel Time by IMD Decile",
  "fig9_park_travel_time_by_imd_decile.png",
  "#43aa8b"
)

run_kruskal <- function(facility) {
  dt <- long[
    facility_category == facility &
      scenario == "walk_15min" &
      !is.na(IMD_Decile) &
      !is.na(nearest_travel_time_min)
  ]
  test <- kruskal.test(nearest_travel_time_min ~ factor(IMD_Decile), data = dt)
  k <- uniqueN(dt$IMD_Decile)
  n <- nrow(dt)
  data.table(
    facility_category = facility,
    scenario = "walk_15min",
    n_observations = n,
    n_imd_deciles = k,
    statistic_H = round(as.numeric(test$statistic), 4),
    df = as.integer(test$parameter),
    p_value = signif(as.numeric(test$p.value), 4),
    eta_sq_h = round(as.numeric((test$statistic - k + 1) / (n - k)), 4)
  )
}

all_kruskal <- rbindlist(lapply(unique(long$facility_category), run_kruskal))
setorder(all_kruskal, p_value)
fwrite(all_kruskal, file.path(analysis_dir, "kruskal_tests_by_imd_walk15_all_facilities.csv"))

population_dt <- long[
  scenario == "walk_15min" &
    facility_category %in% c("healthcare_gps", "education_schools", "green_space_parks")
]

children_q <- quantile(wide$children_pct, probs = c(0.25, 0.75), na.rm = TRUE)
older_q <- quantile(wide$older_adults_pct, probs = c(0.25, 0.75), na.rm = TRUE)

population_dt[, children_group := fifelse(
  children_pct <= children_q[1],
  "Low children share (bottom quartile)",
  fifelse(children_pct >= children_q[2], "High children share (top quartile)", NA_character_)
)]
population_dt[, elderly_group := fifelse(
  older_adults_pct <= older_q[1],
  "Low older-adult share (bottom quartile)",
  fifelse(older_adults_pct >= older_q[2], "High older-adult share (top quartile)", NA_character_)
)]

summarise_population <- function(group_col, label) {
  out <- population_dt[
    !is.na(get(group_col)),
    .(
      n_lsoas = .N,
      reachable_pct = round(mean(!is.na(nearest_travel_time_min)) * 100, 1),
      mean_time_min = round(mean(nearest_travel_time_min, na.rm = TRUE), 2),
      median_time_min = round(median(nearest_travel_time_min, na.rm = TRUE), 2),
      q1_time_min = round(quantile(nearest_travel_time_min, 0.25, na.rm = TRUE), 2),
      q3_time_min = round(quantile(nearest_travel_time_min, 0.75, na.rm = TRUE), 2)
    ),
    by = .(population_group = get(group_col), facility_category)
  ]
  out[, population_dimension := label]
  setcolorder(out, c("population_dimension", "population_group", "facility_category"))
  out
}

population_summary <- rbindlist(list(
  summarise_population("children_group", "Children share"),
  summarise_population("elderly_group", "Older-adult share")
))
setorder(population_summary, population_dimension, facility_category, population_group)
fwrite(population_summary, file.path(analysis_dir, "population_sensitive_accessibility_summary.csv"))

wilcox_population <- function(group_col, label, facility) {
  dt <- population_dt[
    facility_category == facility &
      !is.na(get(group_col)) &
      !is.na(nearest_travel_time_min)
  ]
  dt[, test_group := get(group_col)]
  test <- wilcox.test(nearest_travel_time_min ~ test_group, data = dt, exact = FALSE)
  data.table(
    population_dimension = label,
    facility_category = facility,
    n_observations = nrow(dt),
    statistic_W = round(as.numeric(test$statistic), 2),
    p_value = signif(as.numeric(test$p.value), 4)
  )
}

population_tests <- rbindlist(list(
  wilcox_population("children_group", "Children share", "education_schools"),
  wilcox_population("children_group", "Children share", "green_space_parks"),
  wilcox_population("children_group", "Children share", "healthcare_gps"),
  wilcox_population("elderly_group", "Older-adult share", "healthcare_gps"),
  wilcox_population("elderly_group", "Older-adult share", "green_space_parks"),
  wilcox_population("elderly_group", "Older-adult share", "education_schools")
))
fwrite(population_tests, file.path(analysis_dir, "population_sensitive_wilcoxon_tests.csv"))

plot_population_boxplot <- function(group_col, facility, title, output_name, fill) {
  dt <- population_dt[
    facility_category == facility &
      !is.na(get(group_col)) &
      !is.na(nearest_travel_time_min)
  ]
  dt[, plot_group := get(group_col)]
  p <- ggplot(dt, aes(x = plot_group, y = nearest_travel_time_min)) +
    geom_boxplot(fill = fill, colour = "#2f2f2f", outlier.alpha = 0.35, width = 0.6) +
    labs(title = title, x = NULL, y = "Nearest travel time (minutes)") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 12, hjust = 1)
    )
  ggsave(file.path(figure_dir, output_name), p, width = 7.5, height = 5, dpi = 300)
}

plot_population_boxplot(
  "children_group",
  "education_schools",
  "School Travel Time by Children Share",
  "fig10_school_travel_time_by_children_share.png",
  "#f9c74f"
)
plot_population_boxplot(
  "elderly_group",
  "healthcare_gps",
  "GP Travel Time by Older-Adult Share",
  "fig11_gp_travel_time_by_older_adult_share.png",
  "#577590"
)

print(all_kruskal)
print(population_summary)
print(population_tests)
