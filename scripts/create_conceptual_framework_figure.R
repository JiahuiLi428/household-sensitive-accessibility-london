library(ggplot2)

figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

box_df <- function(section, x, y, label, fill, width = 2.55, height = 0.42) {
  data.frame(
    section = section,
    x = x,
    y = y,
    label = label,
    fill = fill,
    width = width,
    height = height
  )
}

nodes <- rbind(
  box_df("traditional", 3, 6.18, "Generic Resident", "#EAF2F8", 2.25),
  box_df("traditional", 3, 5.54, "Accessibility", "#D7E8F7", 2.10),
  box_df("traditional", 3, 4.90, "Traditional 15-minute City", "#EAF2F8", 2.85),
  box_df("dissertation", 1.35, 3.62, "Children", "#FFF3E8", 1.55),
  box_df("dissertation", 3.00, 3.62, "Caregiving Adults", "#FFF3E8", 2.10),
  box_df("dissertation", 4.65, 3.62, "Older Adults", "#FFF3E8", 1.75),
  box_df("dissertation", 3.00, 2.70, "Weighted Service Basket", "#FDE4D6", 2.85),
  box_df("dissertation", 3.00, 2.03, "Need-Access Mismatch", "#F9D6D5", 2.80),
  box_df("dissertation", 1.78, 1.20, "Accessibility Paradox\nmore access != more equity", "#E8F0FA", 2.35, 0.58),
  box_df("dissertation", 4.22, 1.20, "Vulnerability Paradox\nmore poverty != more vulnerability", "#FAD7C8", 2.55, 0.58),
  box_df("dissertation", 3.00, 0.28, "Care-sensitive 15-minute City", "#DCEFDA", 3.25, 0.50)
)

arrows <- rbind(
  data.frame(x = 3, xend = 3, y = 5.96, yend = 5.76),
  data.frame(x = 3, xend = 3, y = 5.32, yend = 5.12),
  data.frame(x = c(1.35, 3, 4.65), xend = c(3, 3, 3), y = 3.38, yend = 2.94),
  data.frame(x = 3, xend = 3, y = 2.48, yend = 2.25),
  data.frame(x = c(3, 3), xend = c(1.78, 4.22), y = c(1.79, 1.79), yend = c(1.51, 1.51)),
  data.frame(x = c(1.78, 4.22), xend = c(2.63, 3.37), y = c(0.88, 0.88), yend = c(0.54, 0.54))
)

framework_fig <- ggplot() +
  annotate(
    "rect",
    xmin = 0.35,
    xmax = 5.65,
    ymin = 4.54,
    ymax = 6.70,
    fill = "#F8FAFC",
    colour = "#D9E2EC",
    linewidth = 0.35
  ) +
  annotate(
    "rect",
    xmin = 0.35,
    xmax = 5.65,
    ymin = -0.10,
    ymax = 4.20,
    fill = "#FFFCF8",
    colour = "#EADFD2",
    linewidth = 0.35
  ) +
  annotate(
    "segment",
    x = 0.35,
    xend = 5.65,
    y = 4.37,
    yend = 4.37,
    colour = "#7A7F86",
    linewidth = 0.45
  ) +
  geom_segment(
    data = arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
    colour = "#6B6F76",
    linewidth = 0.42
  ) +
  geom_label(
    data = nodes,
    aes(x = x, y = y, label = label, fill = fill),
    colour = "#1F2933",
    size = 3.05,
    fontface = "bold",
    lineheight = 0.92,
    label.r = unit(0.08, "lines"),
    label.padding = unit(0.28, "lines"),
    linewidth = 0.32
  ) +
  annotate(
    "text",
    x = 0.55,
    y = 6.50,
    label = "Traditional Approach",
    hjust = 0,
    fontface = "bold",
    size = 3.4,
    colour = "#1F2933"
  ) +
  annotate(
    "text",
    x = 0.55,
    y = 4.02,
    label = "This Dissertation",
    hjust = 0,
    fontface = "bold",
    size = 3.4,
    colour = "#1F2933"
  ) +
  annotate(
    "text",
    x = 3,
    y = 7.05,
    label = "Conceptual Framework: From Generic Accessibility to Care-sensitive Equity",
    fontface = "bold",
    size = 3.7,
    colour = "#1F2933"
  ) +
  annotate(
    "text",
    x = 3,
    y = -0.48,
    label = "Core claim: London may be accessible for a generic resident, but not for children, caregiving adults and older adults.",
    size = 2.8,
    colour = "#4B5563"
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0.15, 5.85), ylim = c(-0.65, 7.18), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 18, 12)
  )

ggsave(file.path(figure_dir, "figure0_conceptual_accessibility_paradox_framework.png"), framework_fig, width = 7.2, height = 7.2, dpi = 450)
ggsave(file.path(figure_dir, "figure0_conceptual_accessibility_paradox_framework.pdf"), framework_fig, width = 7.2, height = 7.2)

message("Saved conceptual framework figure.")
