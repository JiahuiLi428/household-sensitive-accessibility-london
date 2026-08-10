# Builds caregiving-oriented accessibility, need and transit-gain indicators.
# Reads: step 02 enriched accessibility data and London boundaries.
# Writes: care-mobility LSOA tables, summaries and figures.

library(data.table)
library(ggplot2)
library(sf)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
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

add_care_mobility_scores(wide)
wide[, care_demand_group := fifelse(
  caregiver_household_intensity >= median(caregiver_household_intensity, na.rm = TRUE),
  "High care-demand household intensity",
  "Low care-demand household intensity"
)]

fwrite(
  wide[, .(
    LSOA21CD,
    LSOA21NM,
    borough_name,
    london_ring,
    population_total,
    household_total,
    IMD_Decile,
    children_pct,
    household_dependent_children_total,
    household_couple_with_dependent_children,
    household_lone_parent_with_dependent_children,
    caregiver_households_count,
    caregiver_household_intensity,
    lone_parent_intensity,
    care_need_score,
    care_walk_index,
    care_transit_index,
    care_transit_gain,
    care_mismatch_index,
    care_demand_group
  )],
  file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv")
)

summary_dt <- rbind(
  wide[, .(
    group = "All LSOAs",
    n_lsoas = .N,
    care_walk_index = round(mean(care_walk_index, na.rm = TRUE), 2),
    care_transit_index = round(mean(care_transit_index, na.rm = TRUE), 2),
    care_transit_gain = round(mean(care_transit_gain, na.rm = TRUE), 2),
    care_need_score = round(mean(care_need_score, na.rm = TRUE), 2),
    care_mismatch_index = round(mean(care_mismatch_index, na.rm = TRUE), 2),
    caregiver_household_intensity = round(mean(caregiver_household_intensity, na.rm = TRUE), 2),
    mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = care_demand_group,
    n_lsoas = .N,
    care_walk_index = round(mean(care_walk_index, na.rm = TRUE), 2),
    care_transit_index = round(mean(care_transit_index, na.rm = TRUE), 2),
    care_transit_gain = round(mean(care_transit_gain, na.rm = TRUE), 2),
    care_need_score = round(mean(care_need_score, na.rm = TRUE), 2),
    care_mismatch_index = round(mean(care_mismatch_index, na.rm = TRUE), 2),
    caregiver_household_intensity = round(mean(caregiver_household_intensity, na.rm = TRUE), 2),
    mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
  ), by = care_demand_group][, care_demand_group := NULL]
)
fwrite(summary_dt, file.path(analysis_dir, "care_mobility_accessibility_summary.csv"))

q20 <- quantile(wide$care_transit_gain, probs = 0.20, na.rm = TRUE)
q80 <- quantile(wide$care_transit_gain, probs = 0.80, na.rm = TRUE)
wide[, care_gain_group := fcase(
  care_transit_gain <= q20, "Bottom 20% care gain",
  care_transit_gain >= q80, "Top 20% care gain",
  default = "Middle 60%"
)]
wide[, inner_london := london_ring == "Inner London"]
wide[, most_deprived := IMD_Decile <= 3]

care_gain_summary <- wide[, .(
  n_lsoas = .N,
  care_transit_gain = round(mean(care_transit_gain, na.rm = TRUE), 2),
  care_walk_index = round(mean(care_walk_index, na.rm = TRUE), 2),
  care_transit_index = round(mean(care_transit_index, na.rm = TRUE), 2),
  care_mismatch_index = round(mean(care_mismatch_index, na.rm = TRUE), 2),
  caregiver_household_intensity = round(mean(caregiver_household_intensity, na.rm = TRUE), 2),
  lone_parent_intensity = round(mean(lone_parent_intensity, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2),
  most_deprived_pct = round(mean(most_deprived, na.rm = TRUE) * 100, 1),
  inner_london_pct = round(mean(inner_london, na.rm = TRUE) * 100, 1)
), by = care_gain_group]
setorder(care_gain_summary, care_gain_group)
fwrite(care_gain_summary, file.path(analysis_dir, "care_mobility_transit_gain_summary.csv"))

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)

borough_theme <- theme_void(base_size = 10.5) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.3, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 7.8, colour = "#666666", hjust = 0),
    legend.position = c(0.86, 0.22),
    legend.background = element_rect(fill = "white", colour = "#D7D3CA", linewidth = 0.25),
    legend.title = element_text(size = 8.3, colour = "#333333"),
    legend.text = element_text(size = 7.8, colour = "#333333"),
    plot.margin = margin(10, 10, 8, 10)
  )

access_palette <- c("#440154", "#31688E", "#35B779", "#FDE725")
care_access_fig <- ggplot() +
  geom_sf(data = map_data, aes(fill = care_walk_index), colour = NA) +
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.9) +
  geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.16) +
  scale_fill_gradientn(
    colours = access_palette,
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    na.value = "#ECEBE7",
    name = "Care mobility\nscore"
  ) +
  labs(
    title = "Caregiving Adult Service-Basket Accessibility",
    subtitle = "All eight daily facilities are included; public transport, schools, supermarkets and pharmacies receive higher weights",
    caption = "Scores range from 0 to 100. The basket reflects trip-chaining needs while retaining the full daily-service set."
  ) +
  borough_theme
ggsave(file.path(figure_dir, "figure19_care_mobility_service_basket_accessibility.png"), care_access_fig, width = 7.2, height = 6.7, dpi = 450)
ggsave(file.path(figure_dir, "figure19_care_mobility_service_basket_accessibility.pdf"), care_access_fig, width = 7.2, height = 6.7)

lim <- quantile(abs(map_data$care_mismatch_index), 0.98, na.rm = TRUE)
diverging_palette <- c("#2166AC", "#67A9CF", "#F7F7F7", "#F4A582", "#B2182B")
care_mismatch_fig <- ggplot() +
  geom_sf(data = map_data, aes(fill = care_mismatch_index), colour = NA) +
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.9) +
  geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.16) +
  scale_fill_gradientn(
    colours = diverging_palette,
    limits = c(-lim, lim),
    oob = scales::squish,
    na.value = "#ECEBE7",
    name = "Care\nmismatch\n(+ deficit)"
  ) +
  labs(
    title = "Care Mobility Need-Access Mismatch",
    subtitle = "Positive values indicate care demand exceeds accessibility; negative values indicate access surplus",
    caption = "Care demand combines dependent-children households, lone-parent intensity and children share."
  ) +
  borough_theme
ggsave(file.path(figure_dir, "figure20_care_mobility_need_access_mismatch.png"), care_mismatch_fig, width = 7.2, height = 6.7, dpi = 450)
ggsave(file.path(figure_dir, "figure20_care_mobility_need_access_mismatch.pdf"), care_mismatch_fig, width = 7.2, height = 6.7)

print(summary_dt)
print(care_gain_summary)
message("Saved care mobility accessibility outputs.")
