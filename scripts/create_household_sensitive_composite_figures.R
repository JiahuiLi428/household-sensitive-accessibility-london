library(data.table)
library(ggplot2)
library(sf)
library(grid)
library(ragg)

analysis_dir <- file.path("data", "output", "analysis")
figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

indices <- fread(file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv"))
mismatch <- fread(file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv"))
lsoas <- st_read(file.path("data", "boundaries", "london_lsoa_2021.gpkg"), quiet = TRUE)
thames <- st_read(file.path("data", "boundaries", "river_thames_osm.geojson"), quiet = TRUE)

lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
boroughs <- aggregate(lsoas["borough_name"], by = list(lsoas$borough_name), FUN = length)
boroughs$borough_name <- boroughs$Group.1
boroughs$Group.1 <- NULL
boroughs <- st_make_valid(boroughs)
thames <- st_transform(thames, st_crs(lsoas))

save_publication <- function(plot, stem, width = 8.2, height = 7.4) {
  ggsave(file.path(figure_dir, paste0(stem, ".png")), plot, width = width, height = height, dpi = 450)
  ggsave(file.path(figure_dir, paste0(stem, ".pdf")), plot, width = width, height = height)
  ggsave(file.path(figure_dir, paste0(stem, ".svg")), plot, width = width, height = height, device = svglite::svglite)
  ggsave(file.path(figure_dir, paste0(stem, ".tiff")), plot, width = width, height = height,
         dpi = 600, device = ragg::agg_tiff, compression = "lzw")
}

access_palette <- c("#2C3E82", "#8FA6C9", "#E7CFA0", "#B8862E")
gap_palette <- c("#5B3A6E", "#A084B0", "#DCC17A", "#B8862E")
mismatch_palette <- c("#2C3E82", "#B8C4DC", "#F2E9DC", "#C98C8C", "#8C2F39")
access_map_limit <- 75
group_palette <- c(
  "Children" = "#9ECAE1",
  "Caregiving" = "#4292C6",
  "Older adults" = "#08519C",
  "Caregiving Adults" = "#4292C6"
)

map_theme <- theme_void(base_size = 9.6) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.2, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 7.6, colour = "#666666", hjust = 0),
    strip.text = element_text(face = "bold", size = 9.2, colour = "#252525", hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(size = 8.2),
    legend.text = element_text(size = 7.6),
    plot.margin = margin(10, 10, 8, 10)
  )

chart_theme <- theme_minimal(base_size = 9.8) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E2E8F0", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 9.2, colour = "#4d4d4d", margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 7.6, colour = "#666666", hjust = 0),
    strip.text = element_text(face = "bold", size = 9.2, colour = "#252525", hjust = 0),
    axis.text = element_text(size = 7.8, colour = "#333333"),
    axis.title = element_text(size = 8.8, colour = "#333333"),
    legend.position = "none",
    plot.margin = margin(10, 10, 8, 10)
  )

map_data <- merge(lsoas, indices, by = "LSOA21CD", all.x = TRUE)
if (!"borough_name" %in% names(map_data)) {
  if ("borough_name.x" %in% names(map_data)) {
    map_data$borough_name <- map_data$borough_name.x
  } else if ("borough_name.y" %in% names(map_data)) {
    map_data$borough_name <- map_data$borough_name.y
  }
}
map_data$household_sensitivity_gap <- pmax(
  map_data$family_walk_index,
  map_data$working_walk_index,
  map_data$older_walk_index,
  na.rm = TRUE
) - pmin(
  map_data$family_walk_index,
  map_data$working_walk_index,
  map_data$older_walk_index,
  na.rm = TRUE
)

make_access_map <- function(col, title) {
  access_bounds <- seq(0, access_map_limit, length.out = 21)
  ggplot() +
    geom_sf(data = map_data, aes(fill = .data[[col]]), colour = "white", linewidth = 0.015) +
    geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.25, alpha = 0.9) +
    geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.12) +
    scale_fill_stepsn(
      colours = access_palette,
      limits = c(0, access_map_limit),
      breaks = access_bounds,
      labels = c("0", rep("", 19), "75"),
      oob = scales::squish,
      na.value = "#ECEBE7",
      name = "Score"
    ) +
    labs(title = title, subtitle = "Weighted service-basket score, WALK 15 scenario") +
    map_theme +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, colour = "#1f1f1f"),
      plot.subtitle = element_text(size = 7.3, colour = "#4d4d4d", margin = margin(t = 1, b = 4)),
      plot.margin = margin(4, 4, 4, 4)
    )
}

gap_lim <- as.numeric(quantile(map_data$household_sensitivity_gap, 0.985, na.rm = TRUE))
p_access_a <- make_access_map("family_walk_index", "A. Children & Adolescents")
p_access_b <- make_access_map("working_walk_index", "B. Caregiving Adults")
p_access_c <- make_access_map("older_walk_index", "C. Older adults")
p_access_d <- ggplot() +
  geom_sf(data = map_data, aes(fill = household_sensitivity_gap), colour = "white", linewidth = 0.015) +
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.25, alpha = 0.9) +
  geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.12) +
  scale_fill_stepsn(
    colours = gap_palette,
    limits = c(0, gap_lim),
    breaks = seq(0, gap_lim, length.out = 21),
    labels = c("0", rep("", 19), sprintf("%.1f", gap_lim)),
    oob = scales::squish,
    na.value = "#ECEBE7",
    name = "Gap"
  ) +
  labs(
    title = "D. Household sensitivity gap",
    subtitle = "Max-min score; not mismatch"
  ) +
  map_theme +
  theme(
    plot.title = element_text(face = "bold", size = 10.5, colour = "#1f1f1f"),
    plot.subtitle = element_text(size = 7.1, colour = "#4d4d4d", margin = margin(t = 1, b = 4)),
    plot.margin = margin(4, 4, 4, 4)
  )

borough_gap <- as.data.table(st_drop_geometry(map_data))[
  !is.na(household_sensitivity_gap),
  .(mean_gap = mean(household_sensitivity_gap), n_lsoas = .N),
  by = borough_name
][order(-mean_gap)][1:8]
borough_gap[, borough_label := gsub("Barking and Dagenham", "Barking & Dagenham", borough_name, fixed = TRUE)]
borough_gap[, borough_label := gsub("Tower Hamlets", "Tower\nHamlets", borough_label, fixed = TRUE)]
borough_gap[, borough_name := factor(borough_name, levels = rev(borough_name))]
p_access_bar <- ggplot(borough_gap, aes(x = mean_gap, y = factor(borough_label, levels = rev(borough_label)))) +
  geom_col(fill = "#9A4A55", width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", mean_gap)), hjust = 1.08, size = 2.3, colour = "white") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Top\nboroughs by\nhousehold\nsensitivity\ngap", x = "Mean gap", y = NULL) +
  chart_theme +
  theme(
    plot.title = element_text(face = "bold", size = 7.2, colour = "#1f1f1f", lineheight = 0.92),
    axis.text.y = element_text(size = 7.1),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 7.4),
    plot.margin = margin(4, 8, 4, 1)
  )

save_access_grid <- function(path, device = c("png", "pdf", "svg", "tiff")) {
  device <- match.arg(device)
  if (device == "png") {
    png(path, width = 8.13, height = 7.1, units = "in", res = 450)
  } else if (device == "pdf") {
    pdf(path, width = 8.13, height = 7.1)
  } else if (device == "svg") {
    svglite::svglite(path, width = 8.13, height = 7.1, system_fonts = list(sans = "Arial"))
  } else {
    ragg::agg_tiff(path, width = 8.13, height = 7.1, units = "in", res = 600,
                   compression = "lzw", background = "white")
  }
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  grid.text(
    "Figure 4A-D. Same city, different accessibility subjects",
    x = unit(0.04, "npc"),
    y = unit(0.972, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 15, fontface = "bold", col = "#1f1f1f")
  )
  grid.text(
    "A-C use a shared 0-75 score scale to show within-range contrasts; values above 75 are darkest.",
    x = unit(0.04, "npc"),
    y = unit(0.930, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 8.5, col = "#4d4d4d")
  )
  pushViewport(viewport(y = 0.49, height = 0.80, layout = grid.layout(
    2, 2,
    widths = unit(c(1, 1), "null"),
    heights = unit(c(1, 1), "null")
  )))
  print(p_access_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_access_b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_access_c, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2, layout = grid.layout(
    1, 2,
    widths = unit(c(1.18, 0.82), "null")
  )))
  print(p_access_d, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_access_bar, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  popViewport()
  popViewport()
  grid.text(
    "D is the within-LSOA max-min score across the three household-weighted accessibility surfaces, not a need-access mismatch indicator.",
    x = unit(0.04, "npc"),
    y = unit(0.025, "npc"),
    just = c("left", "bottom"),
    gp = gpar(fontsize = 7.2, col = "#666666")
  )
  dev.off()
}

save_access_grid(file.path(figure_dir, "figure4_household_accessibility_abcd.png"), "png")
save_access_grid(file.path(figure_dir, "figure4_household_accessibility_abcd.pdf"), "pdf")
save_access_grid(file.path(figure_dir, "figure4_household_accessibility_abcd.svg"), "svg")
save_access_grid(file.path(figure_dir, "figure4_household_accessibility_abcd.tiff"), "tiff")

dist_base <- indices[
  !is.na(IMD_Decile),
  .(LSOA21CD, IMD_Decile, family_walk_index, working_walk_index, older_walk_index)
]
dist_long <- melt(
  dist_base,
  id.vars = c("LSOA21CD", "IMD_Decile"),
  measure.vars = c("family_walk_index", "working_walk_index", "older_walk_index"),
  variable.name = "group",
  value.name = "accessibility_score"
)
dist_long[, group_label := factor(group, levels = c(
  "family_walk_index",
  "working_walk_index",
  "older_walk_index"
), labels = c("Children", "Caregiving", "Older adults"))]
dist_long[, imd_label := factor(IMD_Decile, levels = 1:10)]

dist_panel <- function(group_key, title, fill_colour) {
  ggplot(dist_long[group == group_key], aes(x = imd_label, y = accessibility_score)) +
    geom_violin(fill = fill_colour, alpha = 0.78, trim = FALSE, colour = NA) +
    geom_boxplot(width = 0.18, fill = "white", colour = "#2A2A2A", linewidth = 0.28, outlier.shape = NA) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    labs(title = title, x = NULL, y = "Accessibility score") +
    chart_theme +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, colour = "#1f1f1f"),
      legend.position = "none",
      plot.margin = margin(4, 6, 4, 6)
    )
}

equity_rank <- fread(file.path(analysis_dir, "facility_specific_equity_ranking_walk15.csv"))
equity_rank <- equity_rank[facility %in% c("Parks", "Bus stops", "Schools", "Pharmacies", "Supermarkets", "GPs")]
equity_rank[, facility := factor(facility, levels = rev(equity_rank[order(eta_sq_h)]$facility))]

p_dist_a <- dist_panel("family_walk_index", "A. Children by IMD", "#A9C7D8")
p_dist_b <- dist_panel("working_walk_index", "B. Caregiving by IMD", "#D7A9B7")
p_dist_c <- dist_panel("older_walk_index", "C. Older adults by IMD", "#A8BDA9")
p_dist_d <- ggplot(equity_rank, aes(x = eta_sq_h, y = facility)) +
  geom_col(fill = "#7E9FB2", width = 0.68) +
  geom_text(aes(label = sprintf("%.3f", eta_sq_h)), hjust = -0.08, size = 2.7, colour = "#333333") +
  scale_x_continuous(
    limits = c(0, max(equity_rank$eta_sq_h, na.rm = TRUE) * 1.22),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "D. Effect Size Ranking",
    x = "eta-squared",
    y = NULL
  ) +
  chart_theme +
  theme(
    plot.title = element_text(face = "bold", size = 10.5, colour = "#1f1f1f"),
    axis.text.y = element_text(size = 8.2),
    plot.margin = margin(4, 6, 4, 6)
  )

save_distribution_grid <- function(path, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(path, width = 8.4, height = 6.8, units = "in", res = 450)
  } else {
    pdf(path, width = 8.4, height = 6.8)
  }
  grid.newpage()
  grid.text(
    "Figure 5A-D. Accessibility, Deprivation and Effect Size",
    x = unit(0.04, "npc"),
    y = unit(0.965, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 15, fontface = "bold", col = "#1f1f1f")
  )
  grid.text(
    "A-C show accessibility distributions by IMD decile; D shows why IMD explains only part of the pattern",
    x = unit(0.04, "npc"),
    y = unit(0.925, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 9.2, col = "#4d4d4d")
  )
  pushViewport(viewport(y = 0.46, height = 0.78, layout = grid.layout(2, 2)))
  print(p_dist_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_dist_b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_dist_c, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_dist_d, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  popViewport()
  grid.text(
    "IMD decile 1 is most deprived and 10 is least deprived. Effect sizes are small, supporting the deprivation-vulnerability distinction.",
    x = unit(0.04, "npc"),
    y = unit(0.025, "npc"),
    just = c("left", "bottom"),
    gp = gpar(fontsize = 7.6, col = "#666666")
  )
  dev.off()
}

save_distribution_grid(file.path(figure_dir, "figure5_household_imd_distribution_abcd.png"), "png")
save_distribution_grid(file.path(figure_dir, "figure5_household_imd_distribution_abcd.pdf"), "pdf")

mismatch_data <- merge(
  map_data,
  mismatch[, .(
    LSOA21CD,
    family_mismatch_index,
    working_mismatch_index,
    older_mismatch_index
  )],
  by = "LSOA21CD",
  all.x = TRUE
)
mismatch_data$household_mean_mismatch <- rowMeans(data.frame(
  mismatch_data$family_mismatch_index,
  mismatch_data$working_mismatch_index,
  mismatch_data$older_mismatch_index
), na.rm = TRUE)

mismatch_long <- melt(
  as.data.table(st_drop_geometry(mismatch_data)),
  id.vars = "LSOA21CD",
  measure.vars = c(
    "family_mismatch_index",
    "working_mismatch_index",
    "older_mismatch_index",
    "household_mean_mismatch"
  ),
  variable.name = "panel",
  value.name = "mismatch_score"
)
mismatch_long[, panel := factor(panel, levels = c(
  "family_mismatch_index",
  "working_mismatch_index",
  "older_mismatch_index",
  "household_mean_mismatch"
), labels = c(
  "A. Children mismatch",
  "B. Caregiving mismatch",
  "C. Older-adult mismatch",
  "D. Average household mismatch"
))]
mismatch_map <- merge(lsoas[, "LSOA21CD"], mismatch_long, by = "LSOA21CD", all.x = TRUE)
mismatch_lim <- quantile(abs(mismatch_long$mismatch_score), 0.98, na.rm = TRUE)
mismatch_bounds <- seq(-mismatch_lim, mismatch_lim, length.out = 21)

fig_mismatch <- ggplot() +
  geom_sf(data = mismatch_map, aes(fill = mismatch_score), colour = "white", linewidth = 0.012) +
  geom_sf(data = thames, colour = "#A9C9D8", linewidth = 0.25, alpha = 0.9) +
  geom_sf(data = boroughs, fill = NA, colour = "#8B8B84", linewidth = 0.12) +
  facet_wrap(~ panel, ncol = 2) +
  scale_fill_stepsn(
    colours = mismatch_palette,
    limits = c(-mismatch_lim, mismatch_lim),
    breaks = mismatch_bounds,
    labels = c(sprintf("%.0f", -mismatch_lim), rep("", 9), "0", rep("", 9), sprintf("%.0f", mismatch_lim)),
    oob = scales::squish,
    na.value = "#ECEBE7",
    name = "Mismatch"
  ) +
  labs(
    title = "Figure 6A-D. Need-Access Mismatch and Household Sensitivity",
    subtitle = "Positive mismatch reveals where need exceeds weighted accessibility",
    caption = "Red indicates need exceeding accessibility; blue indicates access surplus. Panel D shows the signed average of the three household mismatch scores."
  ) +
  map_theme
save_publication(fig_mismatch, "figure6_household_mismatch_abcd")

message("Saved household-sensitive ABCD figures to: ", figure_dir)
