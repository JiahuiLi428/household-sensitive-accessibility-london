# Compares LSOAs in the top and bottom public-transport gain quintiles.
# Reads: service-basket, typology and care-mobility outputs.
# Writes: winners/losers tables and the transit-gain profile figure.

library(data.table)
library(ggplot2)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

indices <- fread(file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv"))
typology <- fread(file.path(analysis_dir, "vulnerability_typology_lsoa.csv"))
care_path <- file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv")
if (file.exists(care_path)) {
  care <- fread(care_path)
  indices <- merge(
    indices,
    care[, .(
      LSOA21CD,
      caregiver_household_intensity,
      care_walk_index,
      care_transit_index,
      care_transit_gain,
      care_mismatch_index
    )],
    by = "LSOA21CD",
    all.x = TRUE
  )
}

indices[, older_transit_gain := older_transit_index - older_walk_index]
indices[, children_transit_gain := children_transit_index - children_walk_index]
walk_cols <- intersect(c("older_walk_index", "children_walk_index", "care_walk_index"), names(indices))
transit_cols <- intersect(c("older_transit_index", "children_transit_index", "care_transit_index"), names(indices))
gain_cols <- intersect(c("older_transit_gain", "children_transit_gain", "care_transit_gain"), names(indices))
indices[, walk_service_basket_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = walk_cols]
indices[, transit_service_basket_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = transit_cols]
indices[, transit_gain_mean := transit_service_basket_mean - walk_service_basket_mean]

q20 <- quantile(indices$transit_gain_mean, probs = 0.20, na.rm = TRUE)
q80 <- quantile(indices$transit_gain_mean, probs = 0.80, na.rm = TRUE)

indices[, transit_gain_group := fcase(
  transit_gain_mean <= q20, "Bottom 20% gain",
  transit_gain_mean >= q80, "Top 20% gain",
  default = "Middle 60%"
)]
indices[, transit_gain_group := factor(
  transit_gain_group,
  levels = c("Bottom 20% gain", "Middle 60%", "Top 20% gain")
)]
indices[, inner_london := london_ring == "Inner London"]
indices[, most_deprived := IMD_Decile <= 3]
indices[, least_deprived := IMD_Decile >= 8]

typology_small <- typology[, .(LSOA21CD, vulnerability_typology, older_mismatch_z, children_mismatch_z)]
indices <- merge(indices, typology_small, by = "LSOA21CD", all.x = TRUE)

fwrite(indices, file.path(analysis_dir, "transit_gain_winners_losers_lsoa.csv"))

summary_dt <- indices[, .(
  n_lsoas = .N,
  mean_transit_gain = round(mean(transit_gain_mean, na.rm = TRUE), 2),
  median_transit_gain = round(median(transit_gain_mean, na.rm = TRUE), 2),
  mean_walk_access = round(mean(walk_service_basket_mean, na.rm = TRUE), 2),
  mean_transit_access = round(mean(transit_service_basket_mean, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2),
  most_deprived_pct = round(mean(most_deprived, na.rm = TRUE) * 100, 1),
  least_deprived_pct = round(mean(least_deprived, na.rm = TRUE) * 100, 1),
  inner_london_pct = round(mean(inner_london, na.rm = TRUE) * 100, 1),
  older_adults_pct = round(mean(older_adults_pct, na.rm = TRUE), 2),
  children_pct = round(mean(children_pct, na.rm = TRUE), 2),
  caregiver_household_intensity = if ("caregiver_household_intensity" %in% names(.SD)) round(mean(caregiver_household_intensity, na.rm = TRUE), 2) else NA_real_,
  older_gain = round(mean(older_transit_gain, na.rm = TRUE), 2),
  children_gain = round(mean(children_transit_gain, na.rm = TRUE), 2),
  care_gain = if ("care_transit_gain" %in% names(.SD)) round(mean(care_transit_gain, na.rm = TRUE), 2) else NA_real_,
  older_mismatch_z = round(mean(older_mismatch_z, na.rm = TRUE), 2),
  children_mismatch_z = round(mean(children_mismatch_z, na.rm = TRUE), 2),
  care_mismatch_index = if ("care_mismatch_index" %in% names(.SD)) round(mean(care_mismatch_index, na.rm = TRUE), 2) else NA_real_
), by = transit_gain_group]

setorder(summary_dt, transit_gain_group)
fwrite(summary_dt, file.path(analysis_dir, "transit_gain_winners_losers_summary.csv"))

typology_summary <- indices[, .(
  n_lsoas = .N,
  top_gain_pct = round(mean(transit_gain_group == "Top 20% gain", na.rm = TRUE) * 100, 1),
  bottom_gain_pct = round(mean(transit_gain_group == "Bottom 20% gain", na.rm = TRUE) * 100, 1),
  mean_transit_gain = round(mean(transit_gain_mean, na.rm = TRUE), 2),
  mean_walk_access = round(mean(walk_service_basket_mean, na.rm = TRUE), 2),
  mean_transit_access = round(mean(transit_service_basket_mean, na.rm = TRUE), 2)
), by = vulnerability_typology]
setorder(typology_summary, -top_gain_pct)
fwrite(typology_summary, file.path(analysis_dir, "transit_gain_by_vulnerability_typology.csv"))

plot_summary <- copy(summary_dt[transit_gain_group %in% c("Bottom 20% gain", "Top 20% gain")])
profile_dt <- melt(
  plot_summary,
  id.vars = "transit_gain_group",
  measure.vars = c(
    "mean_transit_gain",
    "mean_walk_access",
    "mean_imd_decile",
    "inner_london_pct",
    "care_mismatch_index"
  ),
  variable.name = "metric",
  value.name = "value"
)
metric_labels <- c(
  mean_transit_gain = "Transit gain\n(score points)",
  mean_walk_access = "Walking baseline\n(score)",
  mean_imd_decile = "Mean IMD\ndecile",
  inner_london_pct = "Inner London\nshare (%)",
  care_mismatch_index = "Care mobility\nmismatch"
)
profile_dt[, metric_label := factor(metric_labels[metric], levels = metric_labels)]
profile_dt[, transit_gain_group := factor(
  transit_gain_group,
  levels = c("Bottom 20% gain", "Top 20% gain")
)]

gain_palette <- c(
  "Bottom 20% gain" = "#8DA9B7",
  "Top 20% gain" = "#C58FA0"
)

profile_fig <- ggplot(profile_dt, aes(x = transit_gain_group, y = value, fill = transit_gain_group)) +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = sprintf("%.1f", value)),
    vjust = -0.25,
    size = 2.55,
    color = "#333333"
  ) +
  facet_wrap(~metric_label, scales = "free_y", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = gain_palette, name = NULL) +
  labs(
    title = "High-Gain and Low-Gain Areas of Public Transport Accessibility",
    subtitle = "Top and bottom gain quintiles compared across five policy-relevant indicators",
    x = NULL,
    y = NULL,
    caption = "Top/bottom quintiles use mean gain in older-adult, children-and-adolescents and caregiving-adult service-basket scores."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.2, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    strip.text = element_text(face = "bold", size = 8.5, color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.35),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    plot.caption = element_text(size = 7.7, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )

ggsave(file.path(figure_dir, "figure18_transit_gain_winners_losers_profile.png"), profile_fig, width = 7.2, height = 6.0, dpi = 450)
ggsave(file.path(figure_dir, "figure18_transit_gain_winners_losers_profile.pdf"), profile_fig, width = 7.2, height = 6.0)

print(summary_dt)
print(typology_summary)
message("Saved transit gain winners/losers analysis outputs.")
