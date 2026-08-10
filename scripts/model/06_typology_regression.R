# Builds the vulnerability typology and Appendix C explanatory regressions.
# Reads: earlier accessibility, mismatch and care-mobility outputs.
# Writes: typology tables, regression results and publication figures.

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

add_service_basket_indices(wide)
add_need_scores(wide)
add_care_mobility_scores(wide)

wide[, older_access_score := older_walk_index]
wide[, children_access_score := children_walk_index]
wide[, family_care_access_score := care_walk_index]
wide[, older_need_z := zscore(older_need_score)]
wide[, children_need_z := zscore(children_need_score)]
wide[, family_care_demand_z := zscore(care_need_score)]
wide[, older_access_z := zscore(older_access_score)]
wide[, children_access_z := zscore(children_access_score)]
wide[, family_care_access_z := zscore(family_care_access_score)]
wide[, older_mismatch_z := older_need_z - older_access_z]
wide[, children_mismatch_z := children_need_z - children_access_z]
wide[, family_care_mismatch_z := family_care_demand_z - family_care_access_z]

wide[, older_mismatch_class := cut(
  older_mismatch_z,
  breaks = c(-Inf, -1, 0, 1, Inf),
  labels = c("Low", "Moderate-low", "Moderate-high", "Very high")
)]
wide[, children_mismatch_class := cut(
  children_mismatch_z,
  breaks = c(-Inf, -1, 0, 1, Inf),
  labels = c("Low", "Moderate-low", "Moderate-high", "Very high")
)]

family_care_cut <- median(wide$family_care_demand_z, na.rm = TRUE)
imd_cut <- median(wide$IMD_Decile, na.rm = TRUE)
wide[, care_demand_profile := fifelse(
  family_care_demand_z >= family_care_cut,
  "High caregiving adult demand",
  "Low caregiving adult demand"
)]
wide[, deprivation_profile := fifelse(
  is.na(IMD_Decile),
  NA_character_,
  fifelse(IMD_Decile <= imd_cut, "More deprived", "Less deprived")
)]
wide[, vulnerability_typology := paste(care_demand_profile, deprivation_profile, sep = " + ")]
wide[is.na(deprivation_profile), vulnerability_typology := NA_character_]
typology_levels <- c(
  "Low caregiving adult demand + Less deprived",
  "Low caregiving adult demand + More deprived",
  "High caregiving adult demand + Less deprived",
  "High caregiving adult demand + More deprived"
)
wide[, vulnerability_typology := factor(vulnerability_typology, levels = typology_levels)]

typology_summary <- wide[!is.na(vulnerability_typology), .(
  n_lsoas = .N,
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2),
  mean_family_care_demand_z = round(mean(family_care_demand_z, na.rm = TRUE), 2),
  mean_dep_child_hh_intensity = round(mean(child_household_intensity, na.rm = TRUE), 2),
  mean_older_pct = round(mean(older_adults_pct, na.rm = TRUE), 2),
  mean_children_pct = round(mean(children_pct, na.rm = TRUE), 2),
  mean_family_care_access = round(mean(family_care_access_score, na.rm = TRUE), 2),
  mean_older_access = round(mean(older_access_score, na.rm = TRUE), 2),
  mean_children_access = round(mean(children_access_score, na.rm = TRUE), 2),
  mean_family_care_mismatch_z = round(mean(family_care_mismatch_z, na.rm = TRUE), 2),
  mean_older_mismatch_z = round(mean(older_mismatch_z, na.rm = TRUE), 2),
  mean_children_mismatch_z = round(mean(children_mismatch_z, na.rm = TRUE), 2)
), by = vulnerability_typology][order(as.integer(vulnerability_typology))]
fwrite(typology_summary, file.path(analysis_dir, "vulnerability_typology_summary.csv"))

fwrite(
  wide[, .(
    LSOA21CD,
    LSOA21NM,
    borough_name,
    london_ring,
    IMD_Decile,
    older_adults_pct,
    children_pct,
    child_household_intensity,
    family_care_demand_z,
    vulnerability_typology,
    family_care_access_score,
    family_care_mismatch_z,
    older_access_score,
    children_access_score,
    older_mismatch_z,
    older_mismatch_class,
    children_mismatch_z,
    children_mismatch_class
  )],
  file.path(analysis_dir, "vulnerability_typology_lsoa.csv")
)

model_dt <- wide[, .(
  LSOA21CD,
  gp_time = fifelse(
    is.na(healthcare_gps_walk_15min),
    GP_UNREACHED_IMPUTED_MIN,
    healthcare_gps_walk_15min
  ),
  older_access_score,
  children_access_score,
  older_mismatch_z,
  children_mismatch_z,
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

coef_table <- rbindlist(lapply(names(models), function(model_name) {
  m <- models[[model_name]]
  s <- summary(m)
  coefs <- as.data.table(s$coefficients, keep.rownames = "term")
  setnames(coefs, c("term", "estimate", "std_error", "t_value", "p_value"))
  coefs[, model := model_name]
  coefs[, r_squared := round(s$r.squared, 4)]
  coefs[, adj_r_squared := round(s$adj.r.squared, 4)]
  coefs[, n := nobs(m)]
  coefs
}), use.names = TRUE)
coef_table[, estimate := round(estimate, 4)]
coef_table[, std_error := round(std_error, 4)]
coef_table[, t_value := round(t_value, 3)]
coef_table[, p_value := signif(p_value, 4)]
coef_table <- coef_table[, .(model, term, estimate, std_error, t_value, p_value, r_squared, adj_r_squared, n)]
fwrite(coef_table, file.path(analysis_dir, "explanatory_regression_models.csv"))

model_summary <- unique(coef_table[, .(model, r_squared, adj_r_squared, n)])
fwrite(model_summary, file.path(analysis_dir, "explanatory_regression_model_summary.csv"))

map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)

typology_palette <- c(
  "High caregiving adult demand + More deprived" = "#A63603",
  "High caregiving adult demand + Less deprived" = "#F16913",
  "Low caregiving adult demand + More deprived" = "#FDD0A2",
  "Low caregiving adult demand + Less deprived" = "#F3F0EB"
)

map_theme <- LSOA_MAP_THEME +
  theme(legend.position = c(0.22, 0.18))

typology_map <- ggplot() +
  geom_sf(data = map_data, aes(fill = vulnerability_typology), colour = NA) +
  geom_sf(data = thames, colour = "#BFD9E7", linewidth = 0.38, alpha = 0.85) +
  geom_sf(data = boroughs, fill = NA, colour = "#B8B8B8", linewidth = 0.13, alpha = 0.65) +
  scale_fill_manual(values = typology_palette, name = NULL, na.value = "#F7F7F7") +
  labs(
    title = "Caregiving Adult Vulnerability Typology of London LSOAs",
    subtitle = "Orange-red tones identify care-related vulnerability rather than deprivation alone",
    caption = "High/low caregiving adult demand and more/less deprived are defined using London median thresholds."
  ) +
  map_theme
ggsave(file.path(figure_dir, "figure14_vulnerability_typology_map.png"), typology_map, width = 7.2, height = 6.7, dpi = 450)
ggsave(file.path(figure_dir, "figure14_vulnerability_typology_map.pdf"), typology_map, width = 7.2, height = 6.7)

typology_plot_dt <- melt(
  wide[!is.na(vulnerability_typology)],
  id.vars = "vulnerability_typology",
  measure.vars = c("family_care_mismatch_z", "older_mismatch_z", "children_mismatch_z"),
  variable.name = "mismatch_type",
  value.name = "mismatch_z"
)
typology_plot_dt[, mismatch_label := fifelse(
  mismatch_type == "family_care_mismatch_z", "Caregiving adult mismatch",
  fifelse(mismatch_type == "older_mismatch_z", "Older-people mismatch", "Children mismatch")
)]
typology_plot_dt[, vulnerability_typology := factor(
  vulnerability_typology,
  levels = c(
    "Low caregiving adult demand + Less deprived",
    "Low caregiving adult demand + More deprived",
    "High caregiving adult demand + Less deprived",
    "High caregiving adult demand + More deprived"
  )
)]
typology_plot_dt[, typology_short := factor(
  fifelse(
    vulnerability_typology == "Low caregiving adult demand + Less deprived", "Low care\nless dep.",
    fifelse(
      vulnerability_typology == "Low caregiving adult demand + More deprived", "Low care\nmore dep.",
      fifelse(
        vulnerability_typology == "High caregiving adult demand + Less deprived", "High care\nless dep.",
        "High care\nmore dep."
      )
    )
  ),
  levels = c("Low care\nless dep.", "Low care\nmore dep.", "High care\nless dep.", "High care\nmore dep.")
)]

typology_box <- ggplot(typology_plot_dt, aes(y = typology_short, x = mismatch_z, fill = vulnerability_typology)) +
  geom_vline(xintercept = 0, color = "#6D6A63", linewidth = 0.35) +
  geom_violin(alpha = 0.82, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.18, fill = "white", color = "#2A2A2A", linewidth = 0.35, outlier.shape = NA) +
  facet_wrap(~ mismatch_label, nrow = 1) +
  scale_fill_manual(values = typology_palette, guide = "none") +
  labs(
    title = "Need-Access Mismatch by Vulnerability Typology",
    subtitle = "The vulnerability paradox: high need-access mismatch is not simply the same as high deprivation",
    x = "Mismatch index (z-need minus z-access)",
    y = NULL,
    caption = "Typology uses London median thresholds for caregiving adult demand and IMD decile."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E7E2D8", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", color = "#333333"),
    axis.text.x = element_text(color = "#333333"),
    axis.text.y = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 14, 10)
  )
ggsave(file.path(figure_dir, "figure15_typology_mismatch_distribution.png"), typology_box, width = 7.2, height = 5.4, dpi = 450)
ggsave(file.path(figure_dir, "figure15_typology_mismatch_distribution.pdf"), typology_box, width = 7.2, height = 5.4)

reg_plot <- coef_table[
  term != "(Intercept)" &
    model %in% c("gp_time", "older_access", "children_access", "older_mismatch")
]
reg_plot[, term_label := fifelse(
  term == "older_adults_pct", "Older adult %",
  fifelse(
    term == "children_pct", "Children %",
    fifelse(
      term == "IMD_Decile", "IMD decile",
      "Outer London"
    )
  )
)]
reg_plot[, model_label := fifelse(
  model == "gp_time", "GP time",
  fifelse(
    model == "older_access", "Older access score",
    fifelse(model == "children_access", "Children access score", "Older mismatch")
  )
)]
reg_plot[, significant := p_value < 0.05]

reg_fig <- ggplot(reg_plot, aes(x = estimate, y = term_label, color = significant)) +
  geom_vline(xintercept = 0, color = "#6D6A63", linewidth = 0.35) +
  geom_point(size = 2.7) +
  facet_wrap(~ model_label, scales = "free_x", nrow = 2) +
  scale_color_manual(values = c("TRUE" = "#B65A5C", "FALSE" = "#BDB7AC"), guide = "none") +
  labs(
    title = "Regression Coefficients for Accessibility and Mismatch Models",
    subtitle = "Models include older-adult share, children share, IMD decile and Inner/Outer London",
    x = "Coefficient estimate",
    y = NULL,
    caption = "Red points indicate p < 0.05. Coefficients are not standardised and should be interpreted within each model."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E7E2D8", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#333333"),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )
ggsave(file.path(figure_dir, "figure16_regression_coefficients.png"), reg_fig, width = 7.2, height = 5.5, dpi = 450)
ggsave(file.path(figure_dir, "figure16_regression_coefficients.pdf"), reg_fig, width = 7.2, height = 5.5)

print(typology_summary)
print(model_summary)
message("Saved explanatory typology and regression outputs.")
