library(data.table)
library(ggplot2)
library(sf)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
lsoas <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(file.path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)

lsoas[, "borough_name"] <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
borough_geoms <- aggregate(st_geometry(lsoas), by = list(borough_name = lsoas$borough_name), FUN = st_union)
boroughs <- st_sf(
  borough_name = borough_geoms$borough_name,
  geometry = st_sfc(borough_geoms$x, crs = st_crs(lsoas))
)
boroughs <- st_make_valid(boroughs)
thames <- st_transform(thames, st_crs(lsoas))

encode_time <- function(x) {
  fifelse(is.na(x), 16, x)
}

wide[, older_basket_time := rowMeans(
  data.table(
    encode_time(healthcare_gps_walk_15min),
    encode_time(healthcare_pharmacies_walk_15min),
    encode_time(green_space_parks_walk_15min)
  ),
  na.rm = TRUE
)]

wide[, children_basket_time := rowMeans(
  data.table(
    encode_time(education_schools_walk_15min),
    encode_time(green_space_parks_walk_15min),
    encode_time(healthcare_gps_walk_15min)
  ),
  na.rm = TRUE
)]

classify_mismatch <- function(need, access) {
  need_cut <- median(need, na.rm = TRUE)
  access_cut <- median(access, na.rm = TRUE)
  fifelse(
    need >= need_cut & access >= access_cut,
    "High need / low access",
    fifelse(
      need >= need_cut & access < access_cut,
      "High need / high access",
      fifelse(
        need < need_cut & access >= access_cut,
        "Low need / low access",
        "Low need / high access"
      )
    )
  )
}

wide[, older_basket_mismatch := classify_mismatch(older_adults_pct, older_basket_time)]
wide[, children_basket_mismatch := classify_mismatch(children_pct, children_basket_time)]

wide[, older_need_high := older_adults_pct >= median(older_adults_pct, na.rm = TRUE)]
wide[, older_access_low := older_basket_time >= median(older_basket_time, na.rm = TRUE)]
wide[, children_need_high := children_pct >= median(children_pct, na.rm = TRUE)]
wide[, children_access_low := children_basket_time >= median(children_basket_time, na.rm = TRUE)]

wide[, older_basket_policy_class := fifelse(
  older_basket_mismatch == "High need / low access",
  "High need / low access",
  fifelse(
    older_basket_mismatch == "Low need / high access",
    "Low need / high access",
    "Other"
  )
)]

wide[, children_basket_policy_class := fifelse(
  children_basket_mismatch == "High need / low access",
  "High need / low access",
  fifelse(
    children_basket_mismatch == "Low need / high access",
    "Low need / high access",
    "Other"
  )
)]

older_summary <- wide[, .(
  n_lsoas = .N,
  mean_need_pct = round(mean(older_adults_pct, na.rm = TRUE), 2),
  mean_basket_time_min = round(mean(older_basket_time, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
), by = .(mismatch = older_basket_mismatch)]
older_summary[, population_group := "Older people"]

children_summary <- wide[, .(
  n_lsoas = .N,
  mean_need_pct = round(mean(children_pct, na.rm = TRUE), 2),
  mean_basket_time_min = round(mean(children_basket_time, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
), by = .(mismatch = children_basket_mismatch)]
children_summary[, population_group := "Children"]

basket_summary <- rbind(older_summary, children_summary)
basket_summary <- basket_summary[, .(
  population_group,
  mismatch,
  n_lsoas,
  mean_need_pct,
  mean_basket_time_min,
  mean_imd_decile
)]
fwrite(basket_summary, file.path(analysis_dir, "need_access_basket_mismatch_summary.csv"))

fwrite(
  wide[, .(
    LSOA21CD,
    LSOA21NM,
    IMD_Decile,
    older_adults_pct,
    older_basket_time,
    older_basket_mismatch,
    older_basket_policy_class,
    children_pct,
    children_basket_time,
    children_basket_mismatch,
    children_basket_policy_class
  )],
  file.path(analysis_dir, "need_access_basket_mismatch_lsoa.csv")
)

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)

map_palette <- c(
  "High need / low access" = "#C9827C",
  "Low need / high access" = "#5A86AB",
  "Other" = "#F1F1F1"
)

map_theme <- theme_void(base_size = 10.5) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 8, colour = "#666666", hjust = 0),
    legend.position = c(0.22, 0.18),
    legend.background = element_rect(fill = "white", colour = "#D7D3CA", linewidth = 0.25),
    legend.title = element_text(size = 8.5, colour = "#333333"),
    legend.text = element_text(size = 8, colour = "#333333"),
    plot.margin = margin(10, 10, 8, 10)
  )

plot_mismatch_map <- function(field, title, subtitle, caption, stem) {
  p <- ggplot() +
    geom_sf(data = map_data, aes(fill = .data[[field]]), colour = NA) +
    geom_sf(data = thames, colour = "#BFD9E7", linewidth = 0.38, alpha = 0.85) +
    geom_sf(data = boroughs, fill = NA, colour = "#B8B8B8", linewidth = 0.13, alpha = 0.65) +
    scale_fill_manual(
      values = map_palette,
      breaks = c(
        "High need / low access",
        "Low need / high access",
        "Other"
      ),
      na.value = "#F7F7F7",
      name = NULL
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    map_theme

  ggsave(file.path(figure_dir, paste0(stem, ".png")), p, width = 7.2, height = 6.7, dpi = 450)
  ggsave(file.path(figure_dir, paste0(stem, ".pdf")), p, width = 7.2, height = 6.7)
}

older_need_cut <- median(wide$older_adults_pct, na.rm = TRUE)
older_access_cut <- median(wide$older_basket_time, na.rm = TRUE)
children_need_cut <- median(wide$children_pct, na.rm = TRUE)
children_access_cut <- median(wide$children_basket_time, na.rm = TRUE)

plot_mismatch_map(
  "older_basket_policy_class",
  "Older People Need-Access Mismatch",
  "Need basket: GP services, pharmacies and parks under the WALK 15-minute scenario",
  paste0(
    "High need and low access are defined by London median thresholds: older people share = ",
    round(older_need_cut, 1),
    "%; basket time = ",
    round(older_access_cut, 1),
    " min.\nBorough boundaries and River Thames are shown for reference."
  ),
  "figure6_older_adult_need_access_mismatch_map"
)

plot_mismatch_map(
  "children_basket_policy_class",
  "Children Need-Access Mismatch",
  "Need basket: schools, parks and GP services under the WALK 15-minute scenario",
  paste0(
    "High need and low access are defined by London median thresholds: children share = ",
    round(children_need_cut, 1),
    "%; basket time = ",
    round(children_access_cut, 1),
    " min.\nBorough boundaries and River Thames are shown for reference."
  ),
  "figure7_children_need_access_mismatch_map"
)

print(basket_summary[order(population_group, mismatch)])
message("Saved basket mismatch outputs to: ", analysis_dir, " and ", figure_dir)
