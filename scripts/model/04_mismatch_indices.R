# Constructs continuous need-access mismatch indicators and related maps.
# Reads: steps 02-03 analysis outputs and London boundaries.
# Writes: mismatch indices, rankings, compliance summaries and figures.

library(data.table)
library(ggplot2)
library(sf)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
lsoas <- st_read(project_path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(project_path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)

lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
lsoas$london_ring <- ifelse(lsoas$borough_name %in% inner_london_boroughs, "Inner London", "Outer London")

boroughs <- make_borough_layer(lsoas)
thames <- st_transform(thames, st_crs(lsoas))

wide <- merge(
  wide,
  as.data.table(st_drop_geometry(lsoas))[, .(LSOA21CD, borough_name, london_ring)],
  by = "LSOA21CD",
  all.x = TRUE
)

add_facility_scores(wide)
add_need_scores(wide)
add_care_mobility_scores(wide)

wide[, family_access_score := weighted_index(wide, service_basket_weights$family, "walk")]
wide[, working_access_score := weighted_index(wide, service_basket_weights$working, "walk")]
wide[, older_access_score := weighted_index(wide, service_basket_weights$older, "walk")]
wide[, children_access_score := family_access_score]
wide[, general_access_score := rowMeans(
  as.matrix(wide[, .(
    school_walk_score,
    supermarket_walk_score,
    park_walk_score,
    gp_walk_score,
    hospital_walk_score,
    pharmacy_walk_score,
    bus_stop_walk_score,
    underground_walk_score
  )]),
  na.rm = TRUE
)]

wide[, older_mismatch_index := older_need_score - older_access_score]
wide[, family_mismatch_index := family_need_score - family_access_score]
wide[, working_mismatch_index := care_need_score - working_access_score]
wide[, children_mismatch_index := family_mismatch_index]

fwrite(
  wide[, .(
    LSOA21CD,
    LSOA21NM,
    borough_name,
    london_ring,
    IMD_Decile,
    older_adults_pct,
    older_single_intensity,
    older_need_score,
    older_access_score,
    older_mismatch_index,
    working_age_pct,
    care_need_score,
    working_access_score,
    working_mismatch_index,
    children_pct,
    child_household_intensity,
    family_need_score,
    family_access_score,
    family_mismatch_index,
    children_need_score,
    children_access_score,
    children_mismatch_index,
    general_access_score
  )],
  file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv")
)

borough_ranking <- wide[, .(
  n_lsoas = .N,
  family_access_mean = round(mean(family_access_score, na.rm = TRUE), 2),
  working_access_mean = round(mean(working_access_score, na.rm = TRUE), 2),
  older_access_mean = round(mean(older_access_score, na.rm = TRUE), 2),
  children_access_mean = round(mean(children_access_score, na.rm = TRUE), 2),
  general_access_mean = round(mean(general_access_score, na.rm = TRUE), 2),
  family_mismatch_mean = round(mean(family_mismatch_index, na.rm = TRUE), 2),
  working_mismatch_mean = round(mean(working_mismatch_index, na.rm = TRUE), 2),
  older_mismatch_mean = round(mean(older_mismatch_index, na.rm = TRUE), 2),
  children_mismatch_mean = round(mean(children_mismatch_index, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
), by = borough_name]
borough_ranking[, family_access_rank := frank(-family_access_mean, ties.method = "min")]
borough_ranking[, working_access_rank := frank(-working_access_mean, ties.method = "min")]
borough_ranking[, older_access_rank := frank(-older_access_mean, ties.method = "min")]
borough_ranking[, children_access_rank := frank(-children_access_mean, ties.method = "min")]
borough_ranking[, family_mismatch_rank := frank(-family_mismatch_mean, ties.method = "min")]
borough_ranking[, working_mismatch_rank := frank(-working_mismatch_mean, ties.method = "min")]
borough_ranking[, older_mismatch_rank := frank(-older_mismatch_mean, ties.method = "min")]
borough_ranking[, children_mismatch_rank := frank(-children_mismatch_mean, ties.method = "min")]
fwrite(borough_ranking[order(older_access_rank)], file.path(analysis_dir, "borough_service_basket_accessibility_ranking.csv"))

facility_cols <- walk15_facility_time_cols
facility_labels <- walk15_facility_labels
compliance <- rbindlist(lapply(facility_cols, function(col) {
  data.table(
    facility = facility_labels[[col]],
    n_lsoas = nrow(wide),
    reached_15min_n = sum(!is.na(wide[[col]])),
    reached_15min_pct = round(mean(!is.na(wide[[col]])) * 100, 1),
    mean_time_reached = round(mean(wide[[col]], na.rm = TRUE), 2),
    median_time_reached = round(median(wide[[col]], na.rm = TRUE), 2)
  )
}))
fwrite(compliance[order(-reached_15min_pct)], file.path(analysis_dir, "walk15_facility_compliance_summary.csv"))

equity <- fread(file.path(analysis_dir, "walk_vs_transit_imd_inequality_effect_sizes.csv"))
if ("facility_category" %in% names(equity)) {
  equity[, `:=`(
    statistic_H = H_walk_15min,
    p_value = p_value_walk_15min,
    eta_sq_h = eta_sq_h_walk_15min,
    n_observations = n_walk_15min
  )]
  equity[, facility := fifelse(
    facility_category == "education_schools", "Schools",
    fifelse(
      facility_category == "food_retail_supermarkets", "Supermarkets",
      fifelse(
        facility_category == "green_space_parks", "Parks",
        fifelse(
          facility_category == "healthcare_gps", "GPs",
          fifelse(
            facility_category == "healthcare_hospitals", "Hospitals",
            fifelse(
              facility_category == "healthcare_pharmacies", "Pharmacies",
              fifelse(
                facility_category == "transport_bus_stops", "Bus stops",
                "Underground/metro"
              )
            )
          )
        )
      )
    )
  )]
  equity_ranking <- equity[, .(
    facility,
    n_observations,
    statistic_H,
    p_value,
    eta_sq_h,
    inequality_rank = frank(-eta_sq_h, ties.method = "min")
  )][order(inequality_rank)]
  fwrite(equity_ranking, file.path(analysis_dir, "facility_specific_equity_ranking_walk15.csv"))
}

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)
diverging_palette <- c("#2166AC", "#67A9CF", "#F7F7F7", "#F4A582", "#B2182B")

map_theme <- LSOA_MAP_THEME

plot_mismatch_index_map <- function(field, title, subtitle, stem) {
  vals <- map_data[[field]]
  lim <- quantile(abs(vals), 0.98, na.rm = TRUE)
  p <- ggplot() +
    geom_sf(data = map_data, aes(fill = .data[[field]]), colour = NA) +
    geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.9) +
    geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.16) +
    scale_fill_gradientn(
      colours = diverging_palette,
      limits = c(-lim, lim),
      oob = scales::squish,
      name = "Mismatch\nindex\n(+ deficit)",
      na.value = "#ECEBE7"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Positive values indicate need exceeds accessibility (deficit); negative values indicate accessibility exceeds need (surplus)."
    ) +
    map_theme
  ggsave(file.path(figure_dir, paste0(stem, ".png")), p, width = 7.2, height = 6.7, dpi = 450)
  ggsave(file.path(figure_dir, paste0(stem, ".pdf")), p, width = 7.2, height = 6.7)
}

plot_mismatch_index_map(
  "older_mismatch_index",
  "Older People Need-Access Mismatch Index",
  "Need score combines older-people share and older single-person household intensity",
  "figure12_older_adult_need_access_mismatch_index"
)

plot_mismatch_index_map(
  "children_mismatch_index",
  "Children Need-Access Mismatch Index",
  "Need score combines children share and dependent-children household intensity",
  "figure13_children_need_access_mismatch_index"
)

print(borough_ranking[order(older_access_rank)][1:10])
print(compliance[order(-reached_15min_pct)])
if (exists("equity_ranking")) print(equity_ranking)
message("Saved supporting rankings and mismatch index outputs.")
