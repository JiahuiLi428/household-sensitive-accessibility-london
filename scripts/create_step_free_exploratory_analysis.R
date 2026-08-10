library(data.table)
library(sf)
library(ggplot2)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
step_dir <- file.path("data", "tfl_step_free", "detailed")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

lsoas <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(file.path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)
older_mismatch <- fread(file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv"))
care_mismatch <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))

stations <- fread(file.path(step_dir, "Stations.csv"))
station_points <- fread(file.path(step_dir, "StationPoints.csv"))
platforms <- fread(file.path(step_dir, "Platforms.csv"))

platform_summary <- platforms[IsCustomerFacing == TRUE, .(
  platform_count = .N,
  step_free_platform_count = sum(HasStepFreeRouteInformation == TRUE, na.rm = TRUE)
), by = StationUniqueId]
platform_summary[, step_free_coverage := fcase(
  platform_count > 0 & step_free_platform_count == platform_count, "Full recorded route coverage",
  step_free_platform_count > 0, "Partial recorded route coverage",
  default = "No recorded platform route info"
)]
platform_summary[, step_free_coverage := factor(
  step_free_coverage,
  levels = c("Full recorded route coverage", "Partial recorded route coverage", "No recorded platform route info")
)]

station_xy <- station_points[
  !is.na(Lat) & !is.na(Lon),
  .(lat = mean(Lat, na.rm = TRUE), lon = mean(Lon, na.rm = TRUE)),
  by = StationUniqueId
]

station_dt <- merge(
  stations[, .(StationUniqueId = UniqueId, station_name = Name, fare_zones = FareZones)],
  station_xy,
  by = "StationUniqueId",
  all.x = TRUE
)
station_dt <- merge(station_dt, platform_summary, by = "StationUniqueId", all.x = TRUE)
station_dt[is.na(step_free_coverage), step_free_coverage := "No recorded platform route info"]
station_dt[, step_free_coverage := factor(
  step_free_coverage,
  levels = c("Full recorded route coverage", "Partial recorded route coverage", "No recorded platform route info")
)]
station_dt <- station_dt[!is.na(lat) & !is.na(lon)]

station_sf <- st_as_sf(station_dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
station_sf <- st_transform(station_sf, st_crs(lsoas))

lsoa_dt <- merge(
  older_mismatch[, .(LSOA21CD, older_adults_pct, older_mismatch_index)],
  care_mismatch[, .(LSOA21CD, caregiver_household_intensity, care_mismatch_index)],
  by = "LSOA21CD",
  all = TRUE
)
lsoa_dt[, high_older_mismatch := older_mismatch_index >= quantile(older_mismatch_index, 0.75, na.rm = TRUE)]
lsoa_dt[, high_care_mismatch := care_mismatch_index >= quantile(care_mismatch_index, 0.75, na.rm = TRUE)]
lsoa_dt[, mismatch_flag := fcase(
  high_older_mismatch & high_care_mismatch, "High older + care mismatch",
  high_older_mismatch, "High older mismatch",
  high_care_mismatch, "High care mismatch",
  default = "Other LSOAs"
)]
lsoa_dt[, mismatch_flag := factor(
  mismatch_flag,
  levels = c("Other LSOAs", "High older mismatch", "High care mismatch", "High older + care mismatch")
)]

map_data <- merge(lsoas, lsoa_dt, by = "LSOA21CD", all.x = TRUE)
map_data$mismatch_flag <- factor(
  ifelse(is.na(map_data$mismatch_flag), "Other LSOAs", as.character(map_data$mismatch_flag)),
  levels = c("Other LSOAs", "High older mismatch", "High care mismatch", "High older + care mismatch")
)
thames <- st_transform(thames, st_crs(lsoas))

coverage_summary <- as.data.table(st_drop_geometry(station_sf))[, .(
  n_stations = .N,
  mean_platform_count = round(mean(platform_count, na.rm = TRUE), 2),
  mean_step_free_platform_count = round(mean(step_free_platform_count, na.rm = TRUE), 2)
), by = step_free_coverage]
fwrite(coverage_summary, file.path(analysis_dir, "step_free_station_coverage_summary.csv"))

lsoa_centroids <- st_point_on_surface(map_data)
lsoa_centroids_27700 <- st_transform(lsoa_centroids, 27700)
station_27700 <- st_transform(station_sf, 27700)
full_station_27700 <- station_27700[station_27700$step_free_coverage == "Full recorded route coverage", ]
london_buffer_27700 <- st_buffer(st_union(st_transform(lsoas, 27700)), 2500)
map_station_sf <- st_transform(station_27700[as.logical(st_intersects(station_27700, london_buffer_27700, sparse = FALSE)[, 1]), ], st_crs(lsoas))

nearest_any <- st_nearest_feature(lsoa_centroids_27700, station_27700)
nearest_full <- st_nearest_feature(lsoa_centroids_27700, full_station_27700)
nearest_any_dist_km <- as.numeric(st_distance(lsoa_centroids_27700, station_27700[nearest_any, ], by_element = TRUE)) / 1000
nearest_full_dist_km <- as.numeric(st_distance(lsoa_centroids_27700, full_station_27700[nearest_full, ], by_element = TRUE)) / 1000

distance_dt <- as.data.table(st_drop_geometry(lsoa_centroids))[, .(
  LSOA21CD,
  mismatch_flag,
  older_adults_pct,
  caregiver_household_intensity,
  older_mismatch_index,
  care_mismatch_index
)]
distance_dt[, nearest_tfl_station_km := round(nearest_any_dist_km, 3)]
distance_dt[, nearest_full_recorded_step_free_station_km := round(nearest_full_dist_km, 3)]

distance_summary <- distance_dt[, .(
  n_lsoas = .N,
  nearest_tfl_station_km = round(mean(nearest_tfl_station_km, na.rm = TRUE), 2),
  nearest_full_recorded_step_free_station_km = round(mean(nearest_full_recorded_step_free_station_km, na.rm = TRUE), 2),
  older_adults_pct = round(mean(older_adults_pct, na.rm = TRUE), 2),
  caregiver_household_intensity = round(mean(caregiver_household_intensity, na.rm = TRUE), 2)
), by = mismatch_flag]
fwrite(distance_dt, file.path(analysis_dir, "step_free_lsoa_nearest_station_distances.csv"))
fwrite(distance_summary, file.path(analysis_dir, "step_free_lsoa_distance_summary.csv"))

station_palette <- c(
  "Full recorded route coverage" = "#2F6F7E",
  "Partial recorded route coverage" = "#D5A05E",
  "No recorded platform route info" = "#8E4A4D"
)
mismatch_palette <- c(
  "Other LSOAs" = "#F4F3EF",
  "High older mismatch" = "#D8B08C",
  "High care mismatch" = "#E9B8B4",
  "High older + care mismatch" = "#B65A5C"
)

fig <- ggplot() +
  geom_sf(data = map_data, aes(fill = mismatch_flag), colour = NA, alpha = 0.78) +
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.95) +
  geom_sf(data = map_station_sf, aes(color = step_free_coverage), size = 1.05, alpha = 0.9) +
  scale_fill_manual(values = mismatch_palette, name = "Mismatch areas") +
  scale_color_manual(values = station_palette, name = "TfL station topology") +
  labs(
    title = "Step-Free Station Topology as Care Mobility Infrastructure",
    subtitle = "Exploratory overlay of recorded step-free station route coverage and high older/care mismatch LSOAs",
    caption = "Exploratory only. Station categories use TfL station topology platform route-information records; they do not replace R5 travel-time accessibility."
  ) +
  theme_void(base_size = 10.5) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 12.5, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 8.8, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    legend.position = "right",
    legend.title = element_text(size = 8.2, colour = "#333333"),
    legend.text = element_text(size = 7.6, colour = "#333333"),
    legend.key.size = unit(0.36, "cm"),
    plot.caption = element_text(size = 7.5, colour = "#666666", hjust = 0),
    plot.margin = margin(10, 10, 8, 10)
  )

ggsave(file.path(figure_dir, "figure21_step_free_care_mobility_exploratory.png"), fig, width = 7.2, height = 5.4, dpi = 450)
ggsave(file.path(figure_dir, "figure21_step_free_care_mobility_exploratory.pdf"), fig, width = 7.2, height = 5.4)

print(coverage_summary)
print(distance_summary)
message("Saved exploratory step-free station coverage outputs.")
