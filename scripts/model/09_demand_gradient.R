# Summarises accessibility and mismatch across caregiving-demand quintiles.
# Reads: step 06 vulnerability typology output.
# Writes: quintile summary table and vulnerability-gradient figure.

library(data.table)
library(ggplot2)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(file.path(analysis_dir, "vulnerability_typology_lsoa.csv"))
dt <- dt[complete.cases(family_care_demand_z, family_care_mismatch_z)]

dt[, family_care_demand_quintile := cut(
  family_care_demand_z,
  breaks = quantile(family_care_demand_z, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = paste0("Q", 1:5)
)]
dt[, family_care_demand_quintile := factor(
  family_care_demand_quintile,
  levels = paste0("Q", 1:5),
  labels = c(
    "Q1\nlowest need",
    "Q2",
    "Q3",
    "Q4",
    "Q5\nhighest need"
  )
)]

gradient_summary <- dt[, .(
  n_lsoas = .N,
  mean_family_care_demand_z = round(mean(family_care_demand_z, na.rm = TRUE), 2),
  mean_family_care_access = round(mean(family_care_access_score, na.rm = TRUE), 2),
  mean_family_care_mismatch_z = round(mean(family_care_mismatch_z, na.rm = TRUE), 2),
  median_family_care_mismatch_z = round(median(family_care_mismatch_z, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2),
  mean_dep_child_hh_intensity = round(mean(child_household_intensity, na.rm = TRUE), 2)
), by = family_care_demand_quintile][order(family_care_demand_quintile)]

fwrite(gradient_summary, file.path(analysis_dir, "vulnerability_gradient_family_care_quintiles.csv"))

gradient_plot <- melt(
  gradient_summary,
  id.vars = "family_care_demand_quintile",
  measure.vars = c("mean_family_care_mismatch_z", "mean_family_care_access"),
  variable.name = "metric",
  value.name = "value"
)
gradient_plot[, metric := factor(metric, levels = c(
  "mean_family_care_mismatch_z",
  "mean_family_care_access"
), labels = c(
  "A. Need-access mismatch rises",
  "B. Walking accessibility falls"
))]
gradient_plot[, line_group := fifelse(metric == "A. Need-access mismatch rises", "Mismatch", "Accessibility")]
gradient_plot[, label_y := fifelse(
  metric == "A. Need-access mismatch rises",
  value + 0.12,
  value + fifelse(family_care_demand_quintile %in% c("Q4", "Q5\nhighest need"), -2.6, 2.6)
)]

gradient_fig <- ggplot(gradient_plot, aes(x = family_care_demand_quintile, y = value, group = line_group)) +
  geom_hline(
    data = gradient_plot[metric == "A. Need-access mismatch rises"],
    aes(yintercept = 0),
    inherit.aes = FALSE,
    color = "#8A8780",
    linewidth = 0.35
  ) +
  geom_line(aes(color = line_group), linewidth = 1.05) +
  geom_point(aes(color = line_group), fill = "white", shape = 21, stroke = 1.05, size = 3) +
  geom_text(
    aes(y = label_y, label = sprintf("%.2f", value), color = line_group),
    size = 2.7,
    show.legend = FALSE
  ) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("Mismatch" = "#C17F96", "Accessibility" = "#6F99B2"), guide = "none") +
  labs(
    title = "Vulnerability Gradient: Care Demand Proxy and Need-Access Mismatch",
    subtitle = "The vulnerability paradox: higher care-related vulnerability is not reducible to the poorest IMD contexts",
    x = "Caregiving-oriented demand proxy quintile",
    y = NULL,
    caption = "Q5 has the highest caregiving-oriented demand proxy. Mismatch rises while walking accessibility falls across demand quintiles."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#333333"),
    axis.text.x = element_text(color = "#333333"),
    axis.text.y = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 14, 10)
  )

ggsave(file.path(figure_dir, "figure17_vulnerability_gradient_family_care.png"), gradient_fig, width = 7.2, height = 4.8, dpi = 450)
ggsave(file.path(figure_dir, "figure17_vulnerability_gradient_family_care.pdf"), gradient_fig, width = 7.2, height = 4.8)

print(gradient_summary)
message("Saved vulnerability gradient outputs.")
