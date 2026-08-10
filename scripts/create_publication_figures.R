library(data.table)
library(ggplot2)
library(sf)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
long <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_long.csv"))
lsoas <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(file.path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)

lsoas[, "borough_name"] <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
borough_geoms <- aggregate(st_geometry(lsoas), by = list(borough_name = lsoas$borough_name), FUN = st_union)
boroughs <- st_sf(
  borough_name = borough_geoms$borough_name,
  geometry = st_sfc(borough_geoms$x, crs = st_crs(lsoas))
)
boroughs <- st_make_valid(boroughs)

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)
thames <- st_transform(thames, st_crs(map_data))

# Muted, print-friendly sequential palette for travel-time burden.
# Low values are pale sage; high values move through ochre to deep rose.
map_palette <- c("#F7F7F2", "#DDE7D6", "#B7D3C1", "#E6C36A", "#D9825B", "#A23E48")

map_theme <- theme_void(base_size = 10.5) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 8, colour = "#666666", hjust = 0),
    legend.position = c(0.86, 0.22),
    legend.background = element_rect(fill = "white", colour = "#d9d9d9", linewidth = 0.25),
    legend.title = element_text(size = 8.5, colour = "#333333"),
    legend.text = element_text(size = 8, colour = "#333333"),
    plot.margin = margin(10, 10, 8, 10)
  )

save_publication <- function(plot, stem, width, height) {
  ggsave(file.path(figure_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 450)
  ggsave(file.path(figure_dir, paste0(stem, ".pdf")), plot, width = width, height = height)
}

plot_access_map <- function(field, title, stem) {
  p <- ggplot() +
    geom_sf(data = map_data, aes(fill = .data[[field]]), colour = NA) +
    geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.9) +
    geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.16) +
    scale_fill_gradientn(
      colours = map_palette,
      limits = c(0, 15),
      breaks = c(0, 5, 10, 15),
      labels = c("0", "5", "10", "15"),
      na.value = "#ECEBE7",
      name = "Nearest walking\ntime (min)"
    ) +
    labs(
      title = title,
      subtitle = "LSOA-level nearest facility travel time under the WALK 15-minute scenario",
      caption = "Grey LSOAs are not reached within 15 minutes; borough boundaries and River Thames are shown for reference."
    ) +
    map_theme
  save_publication(p, stem, 7.2, 6.7)
}

plot_imd_violin <- function(facility, title, stem) {
  dt <- long[
    facility_category == facility &
      scenario == "walk_15min" &
      !is.na(IMD_Decile) &
      !is.na(nearest_travel_time_min)
  ]
  dt[, IMD_Decile := factor(IMD_Decile, levels = 1:10)]

  p <- ggplot(dt, aes(x = IMD_Decile, y = nearest_travel_time_min, fill = IMD_Decile)) +
    geom_violin(width = 0.9, alpha = 0.88, colour = NA, trim = FALSE) +
    geom_boxplot(width = 0.18, fill = "white", colour = "#2a2a2a", linewidth = 0.38, outlier.shape = NA) +
    geom_jitter(width = 0.13, height = 0, alpha = 0.08, size = 0.45, colour = "#1f1f1f") +
    scale_fill_manual(
      values = c(
        "#E6DCCB", "#D7C5A6", "#C9AE83", "#B99766", "#A78353",
        "#8E7560", "#746D73", "#5A667E", "#405D85", "#2F4D73"
      ),
      guide = "none"
    ) +
    scale_y_continuous(breaks = c(0, 5, 10, 15), expand = expansion(mult = c(0.02, 0.04))) +
    coord_cartesian(ylim = c(0, 15)) +
    labs(
      title = title,
      subtitle = "Nearest facility travel time under the WALK 15-minute scenario",
      x = "IMD decile (1 = most deprived, 10 = least deprived)",
      y = "Nearest walking travel time (min)",
      caption = "Violin = distribution; box = median/IQR. LSOAs not reached within 15 minutes are excluded."
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.3),
      axis.title = element_text(colour = "#333333"),
      axis.text = element_text(colour = "#333333"),
      plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
      plot.subtitle = element_text(size = 9.5, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
      plot.caption = element_text(size = 8, colour = "#666666", hjust = 0),
      plot.margin = margin(10, 12, 8, 10)
    )
  save_publication(p, stem, 7.2, 4.8)
}

plot_access_map(
  "healthcare_gps_walk_15min",
  "Accessibility to GP Services",
  "figure1_gp_accessibility_publication"
)

plot_access_map(
  "food_retail_supermarkets_walk_15min",
  "Accessibility to Supermarkets",
  "figure2_supermarket_accessibility_publication"
)

plot_imd_violin(
  "healthcare_gps",
  "GP Accessibility by IMD Decile",
  "figure3_gp_imd_violin_publication"
)

plot_imd_violin(
  "food_retail_supermarkets",
  "Supermarket Accessibility by IMD Decile",
  "figure4_supermarket_imd_violin_publication"
)

message("Publication-style figures saved to: ", figure_dir)
