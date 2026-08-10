library(data.table)
library(ggplot2)

root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
data_path <- file.path(root_dir, "data", "output", "analysis", "r5r_accessibility_enriched_wide.csv")
out_dir <- file.path(root_dir, "figures", "publication")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(data_path)

dt[, gp_time_plot := healthcare_gps_walk_15min]
dt[is.na(gp_time_plot), gp_time_plot := 16]
dt[, gp_reached_15min := !is.na(healthcare_gps_walk_15min)]

need_cut <- median(dt$older_adults_pct, na.rm = TRUE)
access_cut <- median(dt$gp_time_plot, na.rm = TRUE)

dt[, mismatch_quadrant := fifelse(
  older_adults_pct >= need_cut & gp_time_plot >= access_cut,
  "High need / low access",
  fifelse(
    older_adults_pct >= need_cut & gp_time_plot < access_cut,
    "High need / high access",
    fifelse(
      older_adults_pct < need_cut & gp_time_plot >= access_cut,
      "Low need / low access",
      "Low need / high access"
    )
  )
)]

quad_summary <- dt[, .(
  n_lsoas = .N,
  mean_older_adults_pct = round(mean(older_adults_pct, na.rm = TRUE), 2),
  mean_gp_time_min = round(mean(gp_time_plot, na.rm = TRUE), 2),
  not_reached_pct = round(mean(!gp_reached_15min) * 100, 1)
), by = mismatch_quadrant][order(mismatch_quadrant)]

fwrite(
  quad_summary,
  file.path(root_dir, "data", "output", "analysis", "older_adult_gp_need_access_mismatch_summary.csv")
)

label_dt <- data.table(
  older_adults_pct = c(
    quantile(dt$older_adults_pct, 0.78, na.rm = TRUE),
    quantile(dt$older_adults_pct, 0.78, na.rm = TRUE),
    quantile(dt$older_adults_pct, 0.10, na.rm = TRUE),
    quantile(dt$older_adults_pct, 0.10, na.rm = TRUE)
  ),
  gp_time_plot = c(14.7, 2.0, 14.7, 2.0),
  label = c(
    "High need\nLow access",
    "High need\nHigh access",
    "Low need\nLow access",
    "Low need\nHigh access"
  )
)

palette <- c(
  "High need / low access" = "#B2182B",
  "High need / high access" = "#67A9CF",
  "Low need / low access" = "#F4A582",
  "Low need / high access" = "#2166AC"
)

fig <- ggplot(dt, aes(x = older_adults_pct, y = gp_time_plot)) +
  annotate(
    "rect",
    xmin = need_cut,
    xmax = Inf,
    ymin = access_cut,
    ymax = Inf,
    fill = "#F6D7D7",
    alpha = 0.55
  ) +
  geom_point(aes(color = mismatch_quadrant), alpha = 0.42, size = 1.25, stroke = 0) +
  geom_vline(xintercept = need_cut, linetype = "dashed", linewidth = 0.45, color = "#5F5A54") +
  geom_hline(yintercept = access_cut, linetype = "dashed", linewidth = 0.45, color = "#5F5A54") +
  geom_text(
    data = label_dt,
    aes(x = older_adults_pct, y = gp_time_plot, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    fontface = "bold",
    color = "#333333",
    lineheight = 0.95
  ) +
  scale_color_manual(values = palette, name = NULL) +
  scale_y_continuous(
    limits = c(0, 16.4),
    breaks = c(0, 5, 10, 15, 16),
    labels = c("0", "5", "10", "15", ">15")
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Older People Need-Access Mismatch for GP Services",
    subtitle = "LSOA-level older people share and nearest walking travel time to GP services",
    x = "Older adults aged 65+ (% of LSOA population)",
    y = "Nearest GP walking time (min)",
    caption = paste0(
      "Dashed lines show London medians: older adult share = ",
      round(need_cut, 1),
      "%; GP time = ",
      round(access_cut, 1),
      " min.\nRed indicates need exceeding access; blue indicates access exceeding need. Values >15 indicate no GP reached within the 15-minute walking threshold."
    )
  ) +
  theme_minimal(base_family = "sans", base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333"),
    plot.subtitle = element_text(size = 10.5, color = "#666666", margin = margin(b = 8)),
    axis.title = element_text(color = "#333333"),
    axis.text = element_text(color = "#4D4D4D"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
    legend.position = "bottom",
    legend.key.width = unit(0.8, "cm"),
    plot.caption = element_text(color = "#777777", size = 8, hjust = 0, margin = margin(t = 8)),
    plot.margin = margin(12, 16, 10, 12)
  )

png_path <- file.path(out_dir, "figure5_older_adult_gp_need_access_mismatch.png")
pdf_path <- file.path(out_dir, "figure5_older_adult_gp_need_access_mismatch.pdf")

ggsave(png_path, fig, width = 7.2, height = 5.55, dpi = 320)
ggsave(pdf_path, fig, width = 7.2, height = 5.55)

print(quad_summary)
message("Saved: ", png_path)
