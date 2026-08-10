library(data.table)
library(ggplot2)
library(sf)
library(grid)
library(ragg)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

lsoas <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(file.path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)
care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))
typology <- fread(file.path(analysis_dir, "vulnerability_typology_lsoa.csv"))
lisa <- fread(file.path(analysis_dir, "family_care_mismatch_lisa_lsoa.csv"))

lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
boroughs <- aggregate(lsoas["borough_name"], by = list(lsoas$borough_name), FUN = length)
boroughs$borough_name <- boroughs$Group.1
boroughs$Group.1 <- NULL
boroughs <- st_make_valid(boroughs)
thames <- st_transform(thames, st_crs(lsoas))

map_data <- merge(lsoas, care[, .(
  LSOA21CD,
  care_walk_index,
  care_mismatch_index,
  care_need_score,
  care_demand_group
)], by = "LSOA21CD", all.x = TRUE)
map_data <- merge(map_data, typology[, .(
  LSOA21CD,
  vulnerability_typology
)], by = "LSOA21CD", all.x = TRUE)
map_data <- merge(map_data, lisa[, .(
  LSOA21CD,
  lisa_cluster
)], by = "LSOA21CD", all.x = TRUE)

map_data$vulnerability_typology <- factor(
  map_data$vulnerability_typology,
  levels = c(
    "Low caregiving adult demand + Less deprived",
    "Low caregiving adult demand + More deprived",
    "High caregiving adult demand + Less deprived",
    "High caregiving adult demand + More deprived"
  )
)
map_data$lisa_cluster <- factor(
  map_data$lisa_cluster,
  levels = c(
    "High-high mismatch cluster",
    "Low-low surplus cluster",
    "High-low spatial outlier",
    "Low-high spatial outlier",
    "Not significant"
  )
)

base_map_theme <- theme_void(base_size = 9.1) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 10.5, colour = "#1f1f1f", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 7.6, colour = "#555555", margin = margin(b = 4)),
    legend.position = "bottom",
    legend.title = element_text(size = 7.3),
    legend.text = element_text(size = 6.6),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.45, "cm"),
    plot.margin = margin(4, 4, 4, 4)
  )

map_layers <- list(
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.26, alpha = 0.9),
  geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.11)
)

access_palette <- c("#2C3E82", "#8FA6C9", "#E7CFA0", "#B8862E")
diverging_palette <- c("#2C3E82", "#B8C4DC", "#F2E9DC", "#C98C8C", "#8C2F39")
typology_palette <- c(
  "Low caregiving adult demand + Less deprived" = "#F2E9DC",
  "Low caregiving adult demand + More deprived" = "#B8C4DC",
  "High caregiving adult demand + Less deprived" = "#C98C8C",
  "High caregiving adult demand + More deprived" = "#8C2F39"
)
lisa_palette <- c(
  "High-high mismatch cluster" = "#8C2F39",
  "Low-low surplus cluster" = "#2C3E82",
  "High-low spatial outlier" = "#C98C8C",
  "Low-high spatial outlier" = "#B8C4DC",
  "Not significant" = "#F2E9DC"
)

mismatch_lim <- quantile(abs(map_data$care_mismatch_index), 0.98, na.rm = TRUE)
access_bounds <- seq(0, 100, length.out = 21)
mismatch_bounds <- seq(-mismatch_lim, mismatch_lim, length.out = 21)
bbox <- st_bbox(map_data)
map_w <- as.numeric(bbox["xmax"] - bbox["xmin"])
map_h <- as.numeric(bbox["ymax"] - bbox["ymin"])
scale_len <- 10000
scale_x <- as.numeric(bbox["xmin"] + map_w * 0.08)
scale_y <- as.numeric(bbox["ymin"] + map_h * 0.08)
north_x <- as.numeric(bbox["xmax"] - map_w * 0.10)
north_y <- as.numeric(bbox["ymax"] - map_h * 0.12)

map_context <- list(
  annotate("segment", x = scale_x, xend = scale_x + scale_len, y = scale_y, yend = scale_y, linewidth = 0.45, colour = "#333333"),
  annotate("segment", x = scale_x, xend = scale_x, y = scale_y - map_h * 0.008, yend = scale_y + map_h * 0.008, linewidth = 0.45, colour = "#333333"),
  annotate("segment", x = scale_x + scale_len, xend = scale_x + scale_len, y = scale_y - map_h * 0.008, yend = scale_y + map_h * 0.008, linewidth = 0.45, colour = "#333333"),
  annotate("text", x = scale_x + scale_len / 2, y = scale_y - map_h * 0.035, label = "10 km", size = 2.25, colour = "#333333"),
  annotate("segment", x = north_x, xend = north_x, y = north_y - map_h * 0.04, yend = north_y + map_h * 0.035, arrow = arrow(length = unit(0.11, "cm"), type = "closed"), linewidth = 0.45, colour = "#333333"),
  annotate("text", x = north_x, y = north_y + map_h * 0.065, label = "N", size = 2.6, fontface = "bold", colour = "#333333")
)

p_access <- ggplot() +
  geom_sf(data = map_data, aes(fill = care_walk_index), colour = "white", linewidth = 0.012) +
  map_layers +
  map_context +
  scale_fill_stepsn(
    colours = access_palette,
    limits = c(0, 100),
    breaks = access_bounds,
    labels = c("0", rep("", 19), "100"),
    na.value = "#ECEBE7",
    name = "Access"
  ) +
  labs(
    title = "A. Accessibility baseline",
    subtitle = "All-facility weighted service-basket score"
  ) +
  base_map_theme

p_mismatch <- ggplot() +
  geom_sf(data = map_data, aes(fill = care_mismatch_index), colour = "white", linewidth = 0.012) +
  map_layers +
  map_context +
  scale_fill_stepsn(
    colours = diverging_palette,
    limits = c(-mismatch_lim, mismatch_lim),
    breaks = mismatch_bounds,
    labels = c(sprintf("%.0f", -mismatch_lim), rep("", 9), "0", rep("", 9), sprintf("%.0f", mismatch_lim)),
    oob = scales::squish,
    na.value = "#ECEBE7",
    name = "Mismatch\n(+ deficit)"
  ) +
  labs(
    title = "B. Need-access mismatch",
    subtitle = "Positive values indicate need exceeds access"
  ) +
  base_map_theme

p_typology <- ggplot() +
  geom_sf(data = map_data, aes(fill = vulnerability_typology), colour = "white", linewidth = 0.012) +
  map_layers +
  map_context +
  scale_fill_manual(
    values = typology_palette,
    na.value = "#ECEBE7",
    name = NULL,
    labels = c(
      "Low care + less deprived",
      "Low care + more deprived",
      "High care + less deprived",
      "High care + more deprived"
    )
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "C. Vulnerability typology",
    subtitle = "Care-related household demand proxy crossed with deprivation"
  ) +
  base_map_theme +
  theme(legend.text = element_text(size = 6.2))

p_lisa <- ggplot() +
  geom_sf(data = map_data, aes(fill = lisa_cluster), colour = "white", linewidth = 0.012) +
  map_layers +
  map_context +
  scale_fill_manual(
    values = lisa_palette,
    na.value = "#ECEBE7",
    name = NULL,
    labels = c(
      "High-high deficit",
      "Low-low surplus",
      "High-low outlier",
      "Low-high outlier",
      "Not significant"
    )
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "D. Spatial clusters",
    subtitle = "Local Moran's I clusters for caregiving adult mismatch"
  ) +
  base_map_theme +
  theme(legend.text = element_text(size = 6.2))

save_composite <- function(path, device = c("png", "pdf", "svg", "tiff")) {
  device <- match.arg(device)
  if (device == "png") {
    png(path, width = 8.7, height = 7.25, units = "in", res = 450)
  } else if (device == "pdf") {
    pdf(path, width = 8.7, height = 7.25)
  } else if (device == "svg") {
    svglite::svglite(path, width = 8.7, height = 7.25, system_fonts = list(sans = "Arial"))
  } else {
    ragg::agg_tiff(path, width = 8.7, height = 7.25, units = "in", res = 600,
                   compression = "lzw", background = "white")
  }
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  grid.text(
    "Figure 7A-D. Why Caregiving Adults Are Not Generic Adults",
    x = unit(0.04, "npc"),
    y = unit(0.966, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 15.0, fontface = "bold", col = "#1f1f1f")
  )
  grid.text(
    "A four-panel evidence set for RQ3: baseline access, need-access mismatch, care-related demand proxy and spatial clustering.",
    x = unit(0.04, "npc"),
    y = unit(0.923, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 9.0, col = "#555555")
  )

  plots <- list(
    p_access + labs(title = "A. Accessibility baseline", subtitle = "Reachable caregiving service basket") +
      theme(legend.position = "bottom", plot.margin = margin(0, 3, 0, 3)),
    p_mismatch + labs(title = "B. Need-access mismatch", subtitle = "Where care need exceeds modelled access") +
      theme(legend.position = "bottom", plot.margin = margin(0, 3, 0, 3)),
    p_typology + labs(title = "C. Vulnerability typology", subtitle = "Care-related demand proxy and deprivation context") +
      theme(legend.position = "bottom", plot.margin = margin(0, 3, 0, 3), legend.text = element_text(size = 5.4)),
    p_lisa + labs(title = "D. Spatial clusters", subtitle = "High-high mismatch clusters test spatial concentration") +
      theme(legend.position = "bottom", plot.margin = margin(0, 3, 0, 3), legend.text = element_text(size = 5.4))
  )

  panel_x <- c(0.270, 0.730, 0.270, 0.730)
  panel_y <- c(0.640, 0.640, 0.285, 0.285)
  panel_w <- 0.435
  panel_h <- 0.325

  for (i in seq_along(plots)) {
    print(
      plots[[i]],
      vp = viewport(
        x = panel_x[i],
        y = panel_y[i],
        width = panel_w,
        height = panel_h
      )
    )
  }

  grid.rect(
    x = unit(0.50, "npc"),
    y = unit(0.052, "npc"),
    width = unit(0.86, "npc"),
    height = unit(0.043, "npc"),
    gp = gpar(fill = "#FAFAF8", col = NA)
  )
  grid.text(
    "Interpretation: RQ3 is answered by showing where the caregiving-oriented demand proxy exceeds modelled access, and whether those deficits cluster spatially.",
    x = unit(0.50, "npc"),
    y = unit(0.055, "npc"),
    just = c("centre", "centre"),
    gp = gpar(fontsize = 7.4, col = "#333333", fontface = "italic")
  )
  dev.off()
}

save_composite(file.path(figure_dir, "figure7_family_care_diagnostics_abcd.png"), "png")
save_composite(file.path(figure_dir, "figure7_family_care_diagnostics_abcd.pdf"), "pdf")
save_composite(file.path(figure_dir, "figure7_family_care_diagnostics_abcd.svg"), "svg")
save_composite(file.path(figure_dir, "figure7_family_care_diagnostics_abcd.tiff"), "tiff")

message("Saved late-stage caregiving adult composite maps to: ", figure_dir)
