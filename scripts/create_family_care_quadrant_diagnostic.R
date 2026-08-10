library(data.table)
library(ggplot2)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))

need_cut <- median(care$care_need_score, na.rm = TRUE)
access_cut <- median(care$care_walk_index, na.rm = TRUE)

care[, quadrant := fifelse(
  care_need_score >= need_cut & care_walk_index < access_cut,
  "High need / low access",
  fifelse(
    care_need_score >= need_cut & care_walk_index >= access_cut,
    "High need / high access",
    fifelse(
      care_need_score < need_cut & care_walk_index < access_cut,
      "Low need / low access",
      "Low need / high access"
    )
  )
)]
care[, quadrant := factor(quadrant, levels = c(
  "High need / low access",
  "High need / high access",
  "Low need / low access",
  "Low need / high access"
))]

quad_summary <- care[, .(
  n_lsoas = .N,
  mean_care_need_score = round(mean(care_need_score, na.rm = TRUE), 2),
  mean_care_walk_index = round(mean(care_walk_index, na.rm = TRUE), 2),
  mean_care_access_burden = round(mean(100 - care_walk_index, na.rm = TRUE), 2),
  mean_care_mismatch_index = round(mean(care_mismatch_index, na.rm = TRUE), 2),
  mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2)
), by = quadrant][order(quadrant)]
fwrite(quad_summary, file.path(analysis_dir, "family_care_quadrant_diagnostic_summary.csv"))

quad_palette <- c(
  "High need / low access" = "#B2182B",
  "High need / high access" = "#F4A340",
  "Low need / low access" = "#9ECAE1",
  "Low need / high access" = "#2166AC"
)

fig <- ggplot(care, aes(x = care_walk_index, y = care_need_score)) +
  annotate("rect", xmin = -Inf, xmax = access_cut, ymin = need_cut, ymax = Inf, fill = "#FAD7C8", alpha = 0.90) +
  annotate("rect", xmin = access_cut, xmax = Inf, ymin = need_cut, ymax = Inf, fill = "#FFE7BD", alpha = 0.70) +
  annotate("rect", xmin = -Inf, xmax = access_cut, ymin = -Inf, ymax = need_cut, fill = "#D8EAF7", alpha = 0.70) +
  annotate("rect", xmin = access_cut, xmax = Inf, ymin = -Inf, ymax = need_cut, fill = "#D7E8F7", alpha = 0.82) +
  geom_point(aes(color = quadrant), alpha = 0.42, size = 1.05, stroke = 0) +
  geom_vline(xintercept = access_cut, linetype = "dashed", linewidth = 0.5, color = "#4D4D4D") +
  geom_hline(yintercept = need_cut, linetype = "dashed", linewidth = 0.5, color = "#4D4D4D") +
  annotate("text", x = 17, y = 91, label = "High Need\nLow Access", fontface = "bold", size = 3.5, color = "#7F1D1D", lineheight = 0.95) +
  annotate("text", x = 82, y = 91, label = "High Need\nHigh Access", fontface = "bold", size = 3.5, color = "#7C2D12", lineheight = 0.95) +
  annotate("text", x = 17, y = 10, label = "Low Need\nLow Access", fontface = "bold", size = 3.5, color = "#1E3A5F", lineheight = 0.95) +
  annotate("text", x = 82, y = 10, label = "Low Need\nHigh Access", fontface = "bold", size = 3.5, color = "#173B74", lineheight = 0.95) +
  scale_color_manual(values = quad_palette, name = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title = "Figure 8. Caregiving Adult Need-Access Quadrants",
    subtitle = "The upper-left quadrant identifies LSOAs where care demand exceeds the local walking service environment",
    x = "Caregiving adult accessibility score",
    y = "Caregiving adult need score",
    caption = paste0(
      "Dashed lines show London medians: care need = ",
      round(need_cut, 1),
      "; accessibility = ",
      round(access_cut, 1),
      "."
    )
  ) +
  theme_minimal(base_size = 10.8) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.4, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.3),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )

ggsave(file.path(figure_dir, "figure8_family_care_quadrant_diagnostic.png"), fig, width = 7.2, height = 5.2, dpi = 450)
ggsave(file.path(figure_dir, "figure8_family_care_quadrant_diagnostic.pdf"), fig, width = 7.2, height = 5.2)

print(quad_summary)
message("Saved caregiving adult quadrant diagnostic figure.")
