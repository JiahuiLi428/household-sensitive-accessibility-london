library(data.table)
library(ggplot2)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

ranking <- fread(file.path(analysis_dir, "borough_service_basket_accessibility_ranking.csv"))
target_boroughs <- c("Camden", "Tower Hamlets", "Richmond upon Thames", "Bromley")

case_summary <- ranking[borough_name %in% target_boroughs]
case_summary[, case_label := fifelse(
  borough_name == "Camden",
  "Inner London high-access reference",
  fifelse(
    borough_name == "Tower Hamlets",
    "Deprived but highly accessible",
    fifelse(
      borough_name == "Richmond upon Thames",
      "Affluent but lower accessibility",
      "Outer London high-mismatch case"
    )
  )
)]
case_summary[, borough_short := fifelse(
  borough_name == "Richmond upon Thames",
  "Richmond",
  borough_name
)]

fwrite(case_summary, file.path(analysis_dir, "four_faces_london_borough_case_studies.csv"))

plot_dt <- melt(
  case_summary,
  id.vars = c("borough_short", "case_label"),
  measure.vars = c("older_access_mean", "children_access_mean", "older_mismatch_mean", "children_mismatch_mean"),
  variable.name = "metric",
  value.name = "value"
)
plot_dt[, metric_label := fifelse(
  metric == "older_access_mean",
  "Older access",
  fifelse(
    metric == "children_access_mean",
    "Children access",
    fifelse(metric == "older_mismatch_mean", "Older mismatch", "Children mismatch")
  )
)]
plot_dt[, metric_group := fifelse(grepl("access", metric), "Accessibility score", "Mismatch index")]
plot_dt[, population_group := fifelse(grepl("^older", metric), "Older-adult basket", "Children basket")]
plot_dt[, borough_short := factor(borough_short, levels = c("Camden", "Tower Hamlets", "Richmond", "Bromley"))]
axis_labels <- case_summary[, .(
  borough_short,
  axis_label = sprintf("%s\nIMD %.1f", borough_short, mean_imd_decile)
)]
axis_labels[, borough_short := factor(borough_short, levels = c("Camden", "Tower Hamlets", "Richmond", "Bromley"))]
axis_label_lookup <- setNames(axis_labels$axis_label, axis_labels$borough_short)

population_palette <- c(
  "Older-adult basket" = "#3F7C8A",
  "Children basket" = "#B65A5C"
)

case_fig <- ggplot(plot_dt, aes(x = borough_short, y = value, fill = population_group)) +
  geom_hline(
    data = data.table(metric_group = "Mismatch index"),
    aes(yintercept = 0),
    inherit.aes = FALSE,
    color = "#6D6A63",
    linewidth = 0.4
  ) +
  geom_col(position = position_dodge(width = 0.74), width = 0.62, alpha = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f", value)),
    position = position_dodge(width = 0.74),
    vjust = ifelse(plot_dt$value >= 0, -0.35, 1.2),
    size = 2.3,
    color = "#333333"
  ) +
  facet_wrap(~metric_group, nrow = 1, scales = "free_y") +
  scale_x_discrete(labels = axis_label_lookup) +
  scale_fill_manual(values = population_palette, name = NULL) +
  labs(
    title = "Four Faces of London's Accessibility Paradox",
    subtitle = "Affluent boroughs can show lower access, while deprived high-density boroughs can perform well",
    x = NULL,
    y = NULL,
    caption = "Labels report mean IMD decile. Positive mismatch = need exceeds access."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, color = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.5, color = "#4d4d4d", margin = margin(t = 2, b = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 8.5, color = "#333333"),
    legend.key.size = unit(0.32, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E7E2D8", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#333333", size = 9.5),
    axis.text.x = element_text(color = "#333333", size = 8.2),
    axis.text.y = element_text(color = "#333333"),
    plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
    plot.margin = margin(10, 12, 8, 10)
  )

ggsave(file.path(figure_dir, "figure17_four_faces_london_accessibility_paradox.png"), case_fig, width = 7.2, height = 4.5, dpi = 450)
ggsave(file.path(figure_dir, "figure17_four_faces_london_accessibility_paradox.pdf"), case_fig, width = 7.2, height = 4.5)

print(case_summary[, .(
  borough_name,
  case_label,
  mean_imd_decile,
  older_access_mean,
  children_access_mean,
  older_mismatch_mean,
  children_mismatch_mean
)])
message("Saved Four Faces case-study outputs.")
