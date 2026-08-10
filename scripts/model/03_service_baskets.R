# Builds group-specific service baskets and deprivation-equity summaries.
# Reads: step 02 enriched accessibility tables and London boundaries.
# Writes: service-basket indices, statistical summaries and publication figures.

library(data.table)
library(ggplot2)
library(sf)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
long <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_long.csv"))
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

add_service_basket_indices(wide)

wide[, older_walk_index_class := cut(
  older_walk_index,
  breaks = c(-Inf, 20, 40, 60, 80, Inf),
  labels = c("Very low", "Low", "Moderate", "High", "Very high")
)]
wide[, children_walk_index_class := cut(
  children_walk_index,
  breaks = c(-Inf, 20, 40, 60, 80, Inf),
  labels = c("Very low", "Low", "Moderate", "High", "Very high")
)]

fwrite(
  wide[, .(
    LSOA21CD,
    LSOA21NM,
    borough_name,
    london_ring,
    IMD_Decile,
    older_adults_pct,
    children_pct,
    family_walk_index,
    family_transit_index,
    working_walk_index,
    working_transit_index,
    older_walk_index,
    older_transit_index,
    children_walk_index,
    children_transit_index
  )],
  file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv")
)

index_summary <- rbind(
  wide[, .(
    group = "Children and Adolescents",
    scenario = "Walk 15",
    mean_index = round(mean(family_walk_index, na.rm = TRUE), 2),
    median_index = round(median(family_walk_index, na.rm = TRUE), 2),
    q1_index = round(quantile(family_walk_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(family_walk_index, 0.75, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = "Children and Adolescents",
    scenario = "Walk + Transit 30",
    mean_index = round(mean(family_transit_index, na.rm = TRUE), 2),
    median_index = round(median(family_transit_index, na.rm = TRUE), 2),
    q1_index = round(quantile(family_transit_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(family_transit_index, 0.75, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = "Caregiving Adults",
    scenario = "Walk 15",
    mean_index = round(mean(working_walk_index, na.rm = TRUE), 2),
    median_index = round(median(working_walk_index, na.rm = TRUE), 2),
    q1_index = round(quantile(working_walk_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(working_walk_index, 0.75, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = "Caregiving Adults",
    scenario = "Walk + Transit 30",
    mean_index = round(mean(working_transit_index, na.rm = TRUE), 2),
    median_index = round(median(working_transit_index, na.rm = TRUE), 2),
    q1_index = round(quantile(working_transit_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(working_transit_index, 0.75, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = "Older adults",
    scenario = "Walk 15",
    mean_index = round(mean(older_walk_index, na.rm = TRUE), 2),
    median_index = round(median(older_walk_index, na.rm = TRUE), 2),
    q1_index = round(quantile(older_walk_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(older_walk_index, 0.75, na.rm = TRUE), 2)
  )],
  wide[, .(
    group = "Older adults",
    scenario = "Walk + Transit 30",
    mean_index = round(mean(older_transit_index, na.rm = TRUE), 2),
    median_index = round(median(older_transit_index, na.rm = TRUE), 2),
    q1_index = round(quantile(older_transit_index, 0.25, na.rm = TRUE), 2),
    q3_index = round(quantile(older_transit_index, 0.75, na.rm = TRUE), 2)
  )]
)
fwrite(index_summary, file.path(analysis_dir, "composite_service_basket_index_summary.csv"))

kw_result <- function(dt) {
  dt <- dt[!is.na(IMD_Decile) & !is.na(nearest_travel_time_min)]
  if (nrow(dt) == 0 || uniqueN(dt$IMD_Decile) < 2) {
    return(data.table(H = NA_real_, df = NA_real_, p_value = NA_real_, eta_sq_h = NA_real_, n = nrow(dt)))
  }
  kt <- kruskal.test(nearest_travel_time_min ~ factor(IMD_Decile), data = dt)
  k <- uniqueN(dt$IMD_Decile)
  n <- nrow(dt)
  eta <- max(0, (as.numeric(kt$statistic) - k + 1) / (n - k))
  data.table(
    H = round(as.numeric(kt$statistic), 4),
    df = as.numeric(kt$parameter),
    p_value = signif(as.numeric(kt$p.value), 4),
    eta_sq_h = round(eta, 4),
    n = n
  )
}

kw_scenario <- long[, kw_result(.SD), by = .(facility_category, scenario)]
kw_wide <- dcast(
  kw_scenario,
  facility_category ~ scenario,
  value.var = c("H", "p_value", "eta_sq_h", "n")
)
if ("eta_sq_h_walk_15min" %in% names(kw_wide) && "eta_sq_h_walk_transit_30min" %in% names(kw_wide)) {
  kw_wide[, eta_change_transit_minus_walk := round(eta_sq_h_walk_transit_30min - eta_sq_h_walk_15min, 4)]
  kw_wide[, inequality_change := fifelse(
    eta_change_transit_minus_walk < 0,
    "Reduced under transit",
    fifelse(eta_change_transit_minus_walk > 0, "Increased under transit", "No change")
  )]
}
fwrite(kw_wide, file.path(analysis_dir, "walk_vs_transit_imd_inequality_effect_sizes.csv"))

ring_long <- melt(
  wide,
  id.vars = c("LSOA21CD", "london_ring", "IMD_Decile"),
  measure.vars = c(
    "family_walk_index",
    "working_walk_index",
    "older_walk_index",
    "family_transit_index",
    "working_transit_index",
    "older_transit_index",
    "children_walk_index",
    "children_transit_index"
  ),
  variable.name = "index_type",
  value.name = "accessibility_index"
)
ring_summary <- ring_long[, .(
  n_lsoas = .N,
  mean_index = round(mean(accessibility_index, na.rm = TRUE), 2),
  median_index = round(median(accessibility_index, na.rm = TRUE), 2),
  q1_index = round(quantile(accessibility_index, 0.25, na.rm = TRUE), 2),
  q3_index = round(quantile(accessibility_index, 0.75, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
), by = .(london_ring, index_type)]
fwrite(ring_summary, file.path(analysis_dir, "inner_outer_composite_accessibility_summary.csv"))

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)

score_palette <- c("#440154", "#31688E", "#35B779", "#FDE725")

map_theme <- LSOA_MAP_THEME

plot_index_map <- function(field, title, subtitle, caption, stem) {
  p <- ggplot() +
    geom_sf(data = map_data, aes(fill = .data[[field]]), colour = NA) +
    geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.35, alpha = 0.9) +
    geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.16) +
    scale_fill_gradientn(
      colours = score_palette,
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      na.value = "#ECEBE7",
      name = "Accessibility\nscore"
    ) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    map_theme
  ggsave(file.path(figure_dir, paste0(stem, ".png")), p, width = 7.2, height = 6.7, dpi = 450)
  ggsave(file.path(figure_dir, paste0(stem, ".pdf")), p, width = 7.2, height = 6.7)
}

plot_index_map(
  "older_walk_index",
  "Older Adult Service-Basket Accessibility Index",
  "All eight everyday facilities are included; GP, pharmacy, park and bus-stop access receive higher weights",
  "Scores range from 0 to 100. Facility scores are set to 0 when not reached within 15 minutes on foot.",
  "figure8_older_adult_service_basket_accessibility_index"
)

plot_index_map(
  "family_walk_index",
  "Children and Adolescents Service-Basket Accessibility Index",
  "All eight everyday facilities are included; schools and parks receive higher weights",
  "Scores range from 0 to 100. Facility scores are set to 0 when not reached within 15 minutes on foot.",
  "figure9_children_service_basket_accessibility_index"
)

plot_kw <- kw_scenario[
  scenario %in% c("walk_15min", "walk_transit_30min") &
    !is.na(eta_sq_h)
]
plot_kw[, scenario_label := fifelse(scenario == "walk_15min", "Walk 15", "Walk + Transit 30")]
plot_kw[, facility_label := fifelse(
  facility_category == "food_retail_supermarkets",
  "Supermarkets",
  fifelse(
    facility_category == "green_space_parks",
    "Parks",
    fifelse(
      facility_category == "healthcare_gps",
      "GPs",
      fifelse(
        facility_category == "healthcare_pharmacies",
        "Pharmacies",
        fifelse(
          facility_category == "education_schools",
          "Schools",
          fifelse(
            facility_category == "transport_bus_stops",
            "Bus stops",
            fifelse(
              facility_category == "transport_underground_metro_stations",
              "Underground/metro",
              "Hospitals"
            )
          )
        )
      )
    )
  )
)]
order_dt <- plot_kw[scenario == "walk_15min"][order(eta_sq_h)]
plot_kw[, facility_label := factor(facility_label, levels = order_dt$facility_label)]

facility_lookup <- data.table(
  facility_category = c(
    "green_space_parks",
    "transport_bus_stops",
    "education_schools",
    "healthcare_pharmacies",
    "food_retail_supermarkets",
    "healthcare_gps"
  ),
  facility_label = c("Parks", "Bus stops", "Schools", "Pharmacies", "Supermarkets", "GPs"),
  walk_col = c(
    "park_walk_score",
    "bus_stop_walk_score",
    "school_walk_score",
    "pharmacy_walk_score",
    "supermarket_walk_score",
    "gp_walk_score"
  ),
  transit_col = c(
    "park_transit_score",
    "bus_stop_transit_score",
    "school_transit_score",
    "pharmacy_transit_score",
    "supermarket_transit_score",
    "gp_transit_score"
  )
)

access_summary <- rbindlist(lapply(seq_len(nrow(facility_lookup)), function(i) {
  row <- facility_lookup[i]
  data.table(
    facility_category = row$facility_category,
    facility_label = row$facility_label,
    scenario_label = c("Walk 15", "Walk + Transit 30"),
    value = c(
      mean(wide[[row$walk_col]], na.rm = TRUE),
      mean(wide[[row$transit_col]], na.rm = TRUE)
    ),
    metric = "A. Access increases"
  )
}))

eta_summary <- plot_kw[
  facility_category %in% facility_lookup$facility_category,
  .(facility_category, facility_label, scenario_label, value = eta_sq_h, metric = "B. Differences can remain")
]

paradox_dt <- rbindlist(list(access_summary, eta_summary), use.names = TRUE)
paradox_dt[, metric := factor(metric, levels = c("A. Access increases", "B. Differences can remain"))]
paradox_dt[, facility_label := factor(facility_label, levels = rev(facility_lookup$facility_label))]

kw_fig <- ggplot(paradox_dt, aes(x = value, y = facility_label, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c("Walk 15" = "#88AABD", "Walk + Transit 30" = "#C58FA0"), name = NULL) +
  labs(
    title = "Figure 9. Accessibility Paradox: Access Improves, Differences Remain",
    subtitle = "Public transport raises mean access, but IMD-related effect sizes do not automatically fall",
    x = NULL,
    y = NULL,
    caption = "Accessibility uses mean 0-100 facility scores. Larger eta-squared values indicate stronger IMD-related inequality."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#333333"),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    legend.position = "bottom",
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )
ggsave(file.path(figure_dir, "figure10_walk_vs_transit_imd_effect_size.png"), kw_fig, width = 7.2, height = 4.9, dpi = 450)
ggsave(file.path(figure_dir, "figure10_walk_vs_transit_imd_effect_size.pdf"), kw_fig, width = 7.2, height = 4.9)

ring_plot <- ring_long[index_type %in% c("family_walk_index", "working_walk_index", "older_walk_index")]
ring_plot[, index_label := fifelse(
  index_type == "family_walk_index",
  "Children and Adolescents basket",
  fifelse(index_type == "working_walk_index", "Caregiving basket", "Older-adult basket")
)]
ring_fig <- ggplot(ring_plot, aes(x = london_ring, y = accessibility_index, fill = london_ring)) +
  geom_violin(alpha = 0.85, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.18, fill = "white", color = "#2A2A2A", linewidth = 0.35, outlier.shape = NA) +
  facet_wrap(~ index_label, nrow = 1) +
  scale_fill_manual(values = c("Inner London" = "#5F8F88", "Outer London" = "#D8B08C"), guide = "none") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title = "Inner-Outer London Differences in Service-Basket Accessibility",
    subtitle = "Composite accessibility scores under the WALK 15-minute scenario",
    x = NULL,
    y = "Accessibility score (0-100)",
    caption = "All baskets include the same eight daily facility categories; only weights differ by household need."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E7E2D8", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#333333"),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )
ggsave(file.path(figure_dir, "figure11_inner_outer_service_basket_accessibility.png"), ring_fig, width = 7.6, height = 4.8, dpi = 450)
ggsave(file.path(figure_dir, "figure11_inner_outer_service_basket_accessibility.pdf"), ring_fig, width = 7.6, height = 4.8)

print(index_summary)
print(kw_wide)
print(ring_summary)
message("Saved advanced analytical outputs to: ", analysis_dir, " and ", figure_dir)
