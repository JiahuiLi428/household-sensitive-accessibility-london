library(data.table)
library(ggplot2)
library(sf)
library(grid)
library(dplyr)

analysis_dir <- file.path("data", "output", "analysis")
boundary_dir <- file.path("data", "boundaries")
poi_dir <- file.path("data", "poi")
out_dir <- file.path("figures", "publication_nature_revised")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_classic(base_size = 7.2, base_family = "Arial"))

palette <- list(
  ink = "#252525",
  muted = "#666666",
  boundary = "#9A9A94",
  thames = "#9EC5D3",
  land = "#F7F7F4",
  pale = "#EFEFEB",
  blue = "#6F8FAA",
  blue_dark = "#526F89",
  orange = "#C8A36B",
  red = "#B87380",
  green = "#809A7D",
  purple = "#8878A0"
)

# Map colour contract adapted from the supplied cartographic reference:
# cool blue for lower values, a quiet near-white midpoint, and muted
# rose/yellow/green endpoints.  The low chroma keeps boundaries and labels
# legible at journal size while preserving metric direction.
map_palettes <- list(
  accessibility_seq = c("#F7FCF0", "#C7E9C0", "#7FCDBB", "#2C7FB8", "#253494"),
  blue_rose = c("#6F9FBC", "#A9C7D8", "#DCE8EC", "#F5ECEE", "#E5BDC9", "#C985A2"),
  blue_yellow = c("#778CB8", "#AEBAD2", "#DEE2EA", "#F4EFD9", "#E8D58F", "#D6B84E"),
  purple_green = c("#8974AD", "#B3A5C9", "#E1DCE8", "#EFF1DF", "#C8D38F", "#91A85B"),
  blue_white_rose = c("#6794B2", "#A9C8D8", "#F8F6F2", "#E8C0CC", "#C57494")
)

theme_map <- function() {
  theme_void(base_size = 7.2, base_family = "Arial") +
    theme(
      panel.border = element_rect(fill = NA, colour = "#222222", linewidth = 0.38),
      panel.grid.major = element_line(colour = "#D6D6D2", linewidth = 0.18, linetype = 3),
      axis.text = element_text(size = 5.8, colour = "#222222"),
      axis.ticks = element_line(colour = "#222222", linewidth = 0.28),
      axis.ticks.length = unit(0.08, "cm"),
      plot.title = element_text(face = "bold", size = 7.8, colour = palette$ink, margin = margin(b = 1.8)),
      plot.subtitle = element_text(size = 6.4, colour = palette$muted, margin = margin(b = 2.8), lineheight = 1.05),
      legend.position = "bottom",
      legend.title = element_text(size = 6.5, face = "bold"),
      legend.text = element_text(size = 6.1),
      legend.key.height = unit(0.23, "cm"),
      legend.key.width = unit(0.52, "cm"),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(fill = "white", colour = "#A7A7A1", linewidth = 0.22),
      legend.box.margin = margin(1.2, 2.2, 1.2, 2.2),
      legend.spacing.x = unit(0.12, "cm"),
      plot.caption = element_text(size = 5.7, colour = palette$muted, hjust = 0, lineheight = 1.05,
                                  margin = margin(t = 2.2)),
      plot.margin = margin(2.4, 2.4, 2.4, 2.4)
    )
}

map_furniture <- function(data, scale_km = 20, arrow_height_fraction = 0.115, pad_fraction = 0.03) {
  d <- st_transform(data, 27700)
  bb_raw <- st_bbox(d)
  raw_dx <- as.numeric(bb_raw["xmax"] - bb_raw["xmin"])
  raw_dy <- as.numeric(bb_raw["ymax"] - bb_raw["ymin"])
  bb <- bb_raw
  bb[c("xmin", "xmax")] <- bb_raw[c("xmin", "xmax")] + c(-1, 1) * pad_fraction * raw_dx
  bb[c("ymin", "ymax")] <- bb_raw[c("ymin", "ymax")] + c(-1, 1) * pad_fraction * raw_dy
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  # Add a small cartographic breathing margin so the London outline does not
  # visually collide with the panel frame at final A4 size.
  bb[c("xmin", "xmax")] <- bb[c("xmin", "xmax")] + c(-1, 1) * 0.03 * dx
  bb[c("ymin", "ymax")] <- bb[c("ymin", "ymax")] + c(-1, 1) * 0.03 * dy
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  bar_m <- scale_km * 1000
  sx <- as.numeric(bb["xmin"] + 0.055 * dx)
  sy <- as.numeric(bb["ymin"] + 0.055 * dy)
  nx <- as.numeric(bb["xmax"] - 0.065 * dx)
  ny <- as.numeric(bb["ymax"] - 0.17 * dy)
  nh <- arrow_height_fraction * dy
  aw <- 0.027 * dx
  arrow_left <- data.frame(
    x = c(nx, nx - aw, nx, nx),
    y = c(ny + nh, ny - 0.32 * nh, ny, ny + nh)
  )
  arrow_right <- data.frame(
    x = c(nx, nx + aw, nx, nx),
    y = c(ny + nh, ny - 0.32 * nh, ny, ny + nh)
  )
  list(
    annotate("segment", x = sx, xend = sx + bar_m / 2, y = sy, yend = sy,
             linewidth = 1.05, colour = "#111111"),
    annotate("segment", x = sx + bar_m / 2, xend = sx + bar_m, y = sy, yend = sy,
             linewidth = 1.05, colour = "white"),
    annotate("segment", x = sx, xend = sx + bar_m, y = sy, yend = sy,
             linewidth = 0.18, colour = "#111111"),
    annotate("text", x = sx, y = sy + 0.025 * dy, label = "0", size = 1.85, hjust = 0),
    annotate("text", x = sx + bar_m, y = sy + 0.025 * dy,
             label = paste0(scale_km, " km"), size = 1.85, hjust = 1),
    geom_polygon(data = arrow_left, aes(x = x, y = y), inherit.aes = FALSE,
                 fill = "#111111", colour = "#111111", linewidth = 0.28),
    geom_polygon(data = arrow_right, aes(x = x, y = y), inherit.aes = FALSE,
                 fill = "white", colour = "#111111", linewidth = 0.28),
    annotate("text", x = nx, y = ny + nh + 0.032 * dy, label = "N",
             size = 2.35, fontface = "bold"),
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      crs = 27700, datum = 4326, expand = FALSE
    )
  )
}

case_map_legend <- function(bb) {
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  x0 <- as.numeric(bb["xmax"] - 0.255 * dx)
  x1 <- as.numeric(bb["xmax"] - 0.018 * dx)
  y0 <- as.numeric(bb["ymin"] + 0.055 * dy)
  y1 <- as.numeric(bb["ymin"] + 0.245 * dy)
  sx0 <- x0 + 0.022 * dx
  sx1 <- x0 + 0.068 * dx
  tx <- x0 + 0.082 * dx
  ys <- y1 - c(0.042, 0.082, 0.122, 0.162) * dy
  list(
    annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
             fill = scales::alpha("white", 0.92), colour = "#7F7F79", linewidth = 0.28),
    annotate("text", x = x0 + 0.018 * dx, y = y1 - 0.014 * dy,
             label = "Legend", hjust = 0, vjust = 1, size = 1.8, fontface = "bold"),
    annotate("rect", xmin = sx0, xmax = sx1, ymin = ys[1] - 0.010 * dy, ymax = ys[1] + 0.010 * dy,
             fill = scales::alpha("#E7C5CE", 0.55), colour = "#A84F69", linewidth = 0.32),
    annotate("text", x = tx, y = ys[1], label = "Selected case area", hjust = 0, size = 1.55),
    annotate("rect", xmin = sx0, xmax = sx1, ymin = ys[2] - 0.010 * dy, ymax = ys[2] + 0.010 * dy,
             fill = "#94A6B1", colour = "#687D89", linewidth = 0.20),
    annotate("text", x = tx, y = ys[2], label = "Building footprint", hjust = 0, size = 1.55),
    annotate("segment", x = sx0, xend = sx1, y = ys[3], yend = ys[3], colour = "#A3A39D", linewidth = 0.48),
    annotate("text", x = tx, y = ys[3], label = "Road network", hjust = 0, size = 1.55),
    annotate("segment", x = sx0, xend = sx1, y = ys[4], yend = ys[4], colour = "#8FC4D5", linewidth = 1.0),
    annotate("text", x = tx, y = ys[4], label = "River Thames", hjust = 0, size = 1.55)
  )
}

theme_chart <- function() {
  theme_classic(base_size = 6.5, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = palette$ink),
      axis.ticks = element_line(linewidth = 0.3, colour = palette$ink),
      axis.text = element_text(size = 5.7, colour = palette$ink),
      axis.title = element_text(size = 6.1, colour = palette$ink),
      plot.title = element_text(face = "bold", size = 7, colour = palette$ink, margin = margin(b = 2)),
      legend.position = "none",
      panel.grid.major.x = element_line(colour = "#E5E5E0", linewidth = 0.25),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

save_nature <- function(draw_fun, stem, width_mm = 183, height_mm = 150, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  grDevices::svg(file.path(out_dir, paste0(stem, ".svg")), width = width_in, height = height_in,
                 family = "Arial", pointsize = 6.5)
  draw_fun()
  dev.off()

  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = width_in, height = height_in,
                       family = "Arial", pointsize = 6.5)
  draw_fun()
  dev.off()

  ragg::agg_png(file.path(out_dir, paste0(stem, ".png")), width = width_in, height = height_in,
                units = "in", res = dpi, background = "white")
  draw_fun()
  dev.off()

  ragg::agg_tiff(file.path(out_dir, paste0(stem, ".tiff")), width = width_in, height = height_in,
                 units = "in", res = dpi, background = "white", compression = "lzw")
  draw_fun()
  dev.off()
}

load_base <- function() {
  lsoas <- st_read(file.path(boundary_dir, "london_lsoa_2021.gpkg"), quiet = TRUE)
  thames <- st_read(file.path(boundary_dir, "river_thames_osm.geojson"), quiet = TRUE)
  lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
  borough_geoms <- aggregate(st_geometry(lsoas), by = list(borough_name = lsoas$borough_name), FUN = st_union)
  boroughs <- st_sf(
    borough_name = borough_geoms$borough_name,
    geometry = st_sfc(borough_geoms$x, crs = st_crs(lsoas))
  )
  list(
    lsoas = st_make_valid(lsoas),
    boroughs = st_make_valid(boroughs),
    thames = st_transform(thames, st_crs(lsoas))
  )
}

add_london_context <- function(thames, boroughs) {
  list(
    geom_sf(data = thames, colour = palette$thames, linewidth = 0.22, alpha = 0.95),
    geom_sf(data = boroughs, fill = NA, colour = palette$boundary, linewidth = 0.10)
  )
}

make_fig4 <- function(base) {
  indices <- fread(file.path(analysis_dir, "composite_service_basket_accessibility_indices.csv"))
  map_data <- merge(base$lsoas, indices, by = "LSOA21CD", all.x = TRUE)
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

  make_access_map <- function(col, title, subtitle) {
    ggplot() +
      geom_sf(data = map_data, aes(fill = .data[[col]]), colour = "white", linewidth = 0.010) +
      add_london_context(base$thames, base$boroughs) +
      map_furniture(base$lsoas, 20) +
      scale_fill_stepsn(
        colours = map_palettes$accessibility_seq,
        limits = c(0, 100), breaks = seq(0, 100, 25), name = "Accessibility score (0-100)",
        na.value = palette$pale
      ) +
      labs(title = title, subtitle = subtitle,
           caption = "Modelled WALK <=15 min | London LSOAs | British National Grid (EPSG:27700)") +
      theme_map()
  }

  gap_lim <- as.numeric(quantile(map_data$household_sensitivity_gap, 0.985, na.rm = TRUE))
  p_a <- make_access_map("family_walk_index", "a  Children & Adolescents", "Weighted service-basket score, WALK 15 scenario")
  p_b <- make_access_map("working_walk_index", "b  Caregiving adults", "Weighted service-basket score, WALK 15 scenario")
  p_c <- make_access_map("older_walk_index", "c  Older adults", "Weighted service-basket score, WALK 15 scenario")
  p_d_map <- ggplot() +
    geom_sf(data = map_data, aes(fill = household_sensitivity_gap), colour = "white", linewidth = 0.010) +
    add_london_context(base$thames, base$boroughs) +
    map_furniture(base$lsoas, 20) +
    scale_fill_stepsn(
      colours = map_palettes$blue_yellow,
      limits = c(0, gap_lim), breaks = pretty(c(0, gap_lim), n = 4),
      oob = scales::squish, name = "Score-point gap", na.value = palette$pale
    ) +
    labs(title = "d  Household sensitivity gap", subtitle = "Maximum minus minimum household accessibility score",
         caption = "Score-point range; this is not need-access mismatch | WALK <=15 min | EPSG:27700") +
    theme_map()

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    pushViewport(viewport(layout = grid.layout(
      2, 2,
      widths = unit(c(1, 1), "null"),
      heights = unit(c(1, 1), "null")
    )))
    print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(p_c, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(p_d_map, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    popViewport()
  }
}

make_fig6 <- function(base) {
  mismatch <- fread(file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv"))
  map_data <- merge(
    base$lsoas,
    mismatch[, .(
      LSOA21CD,
      family_mismatch_index,
      working_mismatch_index,
      older_mismatch_index
    )],
    by = "LSOA21CD", all.x = TRUE
  )
  map_data$household_mean_mismatch <- rowMeans(data.frame(
    map_data$family_mismatch_index,
    map_data$working_mismatch_index,
    map_data$older_mismatch_index
  ), na.rm = TRUE)

  lim <- as.numeric(quantile(abs(c(
    map_data$family_mismatch_index,
    map_data$working_mismatch_index,
    map_data$older_mismatch_index,
    map_data$household_mean_mismatch
  )), 0.98, na.rm = TRUE))

  make_mismatch_map <- function(col, title, subtitle) {
    ggplot() +
      geom_sf(data = map_data, aes(fill = .data[[col]]), colour = "white", linewidth = 0.010) +
      add_london_context(base$thames, base$boroughs) +
      map_furniture(base$lsoas, 20) +
      scale_fill_gradientn(
        colours = map_palettes$blue_white_rose,
        limits = c(-lim, lim),
        breaks = c(-lim, -lim / 2, 0, lim / 2, lim),
        labels = scales::label_number(accuracy = 1),
        oob = scales::squish,
        na.value = palette$pale,
        name = "Mismatch (score points)"
      ) +
      labs(title = title, subtitle = subtitle,
           caption = "Positive = need > access; negative = access surplus | WALK <=15 min | EPSG:27700") +
      theme_map()
  }

  plots <- list(
    make_mismatch_map("family_mismatch_index", "a  Children", "Need score - accessibility score"),
    make_mismatch_map("working_mismatch_index", "b  Caregivers", "Need score - accessibility score"),
    make_mismatch_map("older_mismatch_index", "c  Older adults", "Need score - accessibility score"),
    make_mismatch_map("household_mean_mismatch", "d  Household mean", "Signed mean of three role-specific differences")
  )

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(plots[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(plots[[2]], vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(plots[[3]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(plots[[4]], vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    popViewport()
  }
}

make_fig7 <- function(base) {
  care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))
  typology <- fread(file.path(analysis_dir, "vulnerability_typology_lsoa.csv"))
  lisa <- fread(file.path(analysis_dir, "family_care_mismatch_lisa_lsoa.csv"))
  map_data <- merge(base$lsoas, care[, .(
    LSOA21CD, care_walk_index, care_mismatch_index, care_need_score
  )], by = "LSOA21CD", all.x = TRUE)
  map_data <- merge(map_data, typology[, .(LSOA21CD, vulnerability_typology)], by = "LSOA21CD", all.x = TRUE)
  map_data <- merge(map_data, lisa[, .(LSOA21CD, lisa_cluster)], by = "LSOA21CD", all.x = TRUE)

  p_access <- ggplot() +
    geom_sf(data = map_data, aes(fill = care_walk_index), colour = "white", linewidth = 0.010) +
    add_london_context(base$thames, base$boroughs) +
    map_furniture(base$lsoas, 20) +
    scale_fill_stepsn(
      colours = map_palettes$accessibility_seq,
      limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), name = "Accessibility score (0-100)",
      na.value = palette$pale
    ) +
    labs(title = "a  Accessibility baseline", subtitle = "Caregiving service-basket score; WALK <=15 min",
         caption = "Modelled score (0-100) | London LSOAs | EPSG:27700") +
    theme_map()

  lim <- quantile(abs(map_data$care_mismatch_index), 0.98, na.rm = TRUE)
  p_mismatch <- ggplot() +
    geom_sf(data = map_data, aes(fill = care_mismatch_index), colour = "white", linewidth = 0.010) +
    add_london_context(base$thames, base$boroughs) +
    map_furniture(base$lsoas, 20) +
    scale_fill_gradientn(
      colours = map_palettes$blue_white_rose,
      limits = c(-lim, lim), oob = scales::squish, name = "Need - access (score points)",
      na.value = palette$pale
    ) +
    labs(title = "b  Need-access mismatch", subtitle = "Need score - accessibility score; positive values indicate deficit",
         caption = "Positive = need exceeds modelled accessibility | WALK <=15 min | EPSG:27700") +
    theme_map()

  typology_palette <- c(
    "Low caregiving adult demand + Less deprived" = "#EEECE5",
    "Low caregiving adult demand + More deprived" = "#8FA5B8",
    "High caregiving adult demand + Less deprived" = "#BDC8A3",
    "High caregiving adult demand + More deprived" = "#9586A8"
  )
  p_typology <- ggplot() +
    geom_sf(data = map_data, aes(fill = vulnerability_typology), colour = "white", linewidth = 0.010) +
    add_london_context(base$thames, base$boroughs) +
    map_furniture(base$lsoas, 20) +
    scale_fill_manual(
      values = typology_palette, breaks = names(typology_palette),
      na.value = palette$pale, name = NULL,
      labels = c(
        "Low caregiving adult demand + Less deprived"  = "Low care + less deprived",
        "Low caregiving adult demand + More deprived"  = "Low care + more deprived",
        "High caregiving adult demand + Less deprived" = "High care + less deprived",
        "High caregiving adult demand + More deprived" = "High care + more deprived"
      )
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = "c  Care-demand and deprivation context", subtitle = "IMD: higher decile = less deprived",
         caption = "Median-split care demand and deprivation typology | EPSG:27700") +
    theme_map()

  lisa_palette <- c(
    "High-high mismatch cluster" = palette$red,
    "Low-low surplus cluster" = palette$blue,
    "High-low spatial outlier" = "#C9AD74",
    "Low-high spatial outlier" = "#AFC8D3",
    "Not significant" = "#ECECE8"
  )
  p_lisa <- ggplot() +
    geom_sf(data = map_data, aes(fill = lisa_cluster), colour = "white", linewidth = 0.010) +
    add_london_context(base$thames, base$boroughs) +
    map_furniture(base$lsoas, 20) +
    scale_fill_manual(
      values = lisa_palette, breaks = names(lisa_palette),
      na.value = palette$pale, name = NULL,
      labels = c(
        "High-high mismatch cluster" = "High-high deficit",
        "Low-low surplus cluster"    = "Low-low surplus",
        "High-low spatial outlier"   = "High-low outlier",
        "Low-high spatial outlier"   = "Low-high outlier",
        "Not significant"            = "Not significant"
      )
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = "d  Spatial clusters", subtitle = "Local Moran's I; analytical p < 0.05, unadjusted",
         caption = "High-high = neighbouring high positive mismatch values | Queen contiguity | EPSG:27700") +
    theme_map()

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(p_access, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_mismatch, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(p_typology, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(p_lisa, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    popViewport()
  }
}

make_fig8 <- function() {
  care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))
  need_cut <- median(care$care_need_score, na.rm = TRUE)
  access_cut <- median(care$care_walk_index, na.rm = TRUE)
  care[, quadrant := fifelse(
    care_need_score >= need_cut & care_walk_index < access_cut, "High need / low access",
    fifelse(
      care_need_score >= need_cut & care_walk_index >= access_cut, "High need / high access",
      fifelse(care_need_score < need_cut & care_walk_index < access_cut, "Low need / low access", "Low need / high access")
    )
  )]
  quad_summary <- care[, .(
    n = .N,
    mean_need = mean(care_need_score, na.rm = TRUE),
    mean_access = mean(care_walk_index, na.rm = TRUE),
    mean_mismatch = mean(care_mismatch_index, na.rm = TRUE)
  ), by = quadrant]
  label_for <- function(q) {
    r <- quad_summary[quadrant == q]
    sprintf("n = %s\nneed %.1f\naccess %.1f\nmismatch %.1f", format(r$n, big.mark = ","), r$mean_need, r$mean_access, r$mean_mismatch)
  }
  quad_palette <- c(
    "High need / low access" = palette$red,
    "High need / high access" = palette$orange,
    "Low need / low access" = "#7BAFD4",
    "Low need / high access" = palette$blue
  )

  p <- ggplot(care, aes(x = care_walk_index, y = care_need_score)) +
    annotate("rect", xmin = -Inf, xmax = access_cut, ymin = need_cut, ymax = Inf, fill = "#F7D4CD", alpha = 0.78) +
    annotate("rect", xmin = access_cut, xmax = Inf, ymin = need_cut, ymax = Inf, fill = "#F7E4BD", alpha = 0.62) +
    annotate("rect", xmin = -Inf, xmax = access_cut, ymin = -Inf, ymax = need_cut, fill = "#D9EAF4", alpha = 0.62) +
    annotate("rect", xmin = access_cut, xmax = Inf, ymin = -Inf, ymax = need_cut, fill = "#D7E5F2", alpha = 0.70) +
    geom_point(aes(color = quadrant), alpha = 0.35, size = 0.65, stroke = 0) +
    geom_vline(xintercept = access_cut, linetype = "dashed", linewidth = 0.35, color = palette$muted) +
    geom_hline(yintercept = need_cut, linetype = "dashed", linewidth = 0.35, color = palette$muted) +
    annotate("text", x = 15, y = 92, label = paste("High need / low access\n", label_for("High need / low access")),
             fontface = "bold", size = 2.1, color = "#7D1D25", lineheight = 0.95, hjust = 0.5) +
    annotate("text", x = 82, y = 92, label = paste("High need / high access\n", label_for("High need / high access")),
             size = 2.0, color = "#7A4A15", lineheight = 0.95, hjust = 0.5) +
    annotate("text", x = 15, y = 11, label = paste("Low need / low access\n", label_for("Low need / low access")),
             size = 2.0, color = "#1D4F75", lineheight = 0.95, hjust = 0.5) +
    annotate("text", x = 82, y = 11, label = paste("Low need / high access\n", label_for("Low need / high access")),
             size = 2.0, color = "#173B74", lineheight = 0.95, hjust = 0.5) +
    scale_color_manual(values = quad_palette, name = NULL) +
    scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    labs(x = "Caregiving adult accessibility score", y = "Caregiving adult need score") +
    theme_classic(base_size = 6.5, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35),
      axis.ticks = element_line(linewidth = 0.30),
      axis.text = element_text(size = 5.7, colour = palette$ink),
      axis.title = element_text(size = 6.5, colour = palette$ink),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(4, 5, 3, 4)
    )

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    print(p, vp = viewport(width = 0.98, height = 0.96))
  }
}

load_pois <- function(crs) {
  specs <- data.table(
    facility = c("School", "Supermarket", "GP", "Pharmacy", "Bus stop", "Station"),
    path = file.path(poi_dir, c(
      "education_schools.geojson",
      "food_retail_supermarkets.geojson",
      "healthcare_gps.geojson",
      "healthcare_pharmacies.geojson",
      "transport_bus_stops.geojson",
      "transport_underground_metro_stations.geojson"
    )),
    colour = c(palette$orange, palette$green, palette$red, palette$purple, "#5A5A5A", palette$blue)
  )
  out <- lapply(seq_len(nrow(specs)), function(i) {
    g <- st_read(specs$path[i], quiet = TRUE)
    g <- st_transform(g, crs)
    g$facility <- specs$facility[i]
    g$colour <- specs$colour[i]
    g[, c("facility", "colour", "geometry")]
  })
  do.call(rbind, out)
}

fetch_osm_roads <- function(case_area) {
  pbf <- file.path("data", "raw", "osm", "greater-london-latest.osm.pbf")
  if (!file.exists(pbf)) {
    return(st_sf(geometry = st_sfc(crs = st_crs(case_area))))
  }
  keep_highway <- c(
    "pedestrian", "residential", "living_street",
    "tertiary", "secondary", "primary", "unclassified"
  )
  roads <- tryCatch(
    st_read(pbf, layer = "lines", quiet = TRUE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(roads) || nrow(roads) == 0) {
    return(st_sf(geometry = st_sfc(crs = st_crs(case_area))))
  }
  roads <- roads[!is.na(roads$highway) & roads$highway %in% keep_highway, ]
  roads <- st_transform(roads, st_crs(case_area))
  roads <- suppressWarnings(st_intersection(st_make_valid(roads), st_buffer(case_area, 2400)))
  roads[, "geometry"]
}

make_fig12 <- function(base) {
  negative_case <- c("E01000095", "E01000094", "E01034478", "E01000096", "E01000090", "E01034476", "E01000014")
  positive_case <- c("E01003566", "E01003525", "E01003531", "E01003607")
  lsoas <- st_transform(base$lsoas, 27700)
  boroughs <- st_transform(base$boroughs, 27700)
  thames <- st_transform(base$thames, 27700)
  neg <- lsoas[lsoas$LSOA21CD %in% negative_case, ]
  pos <- lsoas[lsoas$LSOA21CD %in% positive_case, ]
  neg_area <- st_sf(geometry = st_sfc(st_union(st_geometry(neg)), crs = st_crs(lsoas)))
  pos_area <- st_sf(geometry = st_sfc(st_union(st_geometry(pos)), crs = st_crs(lsoas)))
  barking_borough <- boroughs[grepl("Barking", boroughs$borough_name), ]
  countries <- st_transform(st_read(
    file.path(boundary_dir, "Countries_December_2022_Boundaries_UK_BGC.gpkg"),
    quiet = TRUE
  ), 27700)
  country_bb <- st_bbox(countries)
  country_cx <- mean(c(country_bb["xmin"], country_bb["xmax"]))
  country_dy <- as.numeric(country_bb["ymax"] - country_bb["ymin"])
  country_half_width <- 0.55 * country_dy
  country_bb["xmin"] <- country_cx - country_half_width
  country_bb["xmax"] <- country_cx + country_half_width
  country_frame <- st_sf(geometry = st_as_sfc(country_bb))
  london_outline <- st_sf(geometry = st_sfc(st_union(st_geometry(lsoas)), crs = 27700))
  london_point <- st_point_on_surface(london_outline)
  origin <- st_centroid(neg_area)
  pois <- load_pois(27700)
  local_window <- st_buffer(neg_area, 2400)
  local_pois <- pois[lengths(st_within(pois, local_window)) > 0, ]
  service_pois <- local_pois[local_pois$facility %in% c("School", "Supermarket", "GP", "Pharmacy", "Station"), ]
  service_pois$service_group <- dplyr::case_when(
    service_pois$facility == "School" ~ "School",
    service_pois$facility == "Supermarket" ~ "Food retail",
    service_pois$facility %in% c("GP", "Pharmacy") ~ "Health / pharmacy",
    service_pois$facility == "Station" ~ "Transport entry",
    TRUE ~ NA_character_
  )
  service_pois$service_group <- factor(
    service_pois$service_group,
    levels = c("School", "Food retail", "Health / pharmacy", "Transport entry")
  )
  roads <- fetch_osm_roads(neg_area)
  building_path <- file.path("data", "derived", "figure12_panel_c_osm_buildings.gpkg")
  buildings <- if (file.exists(building_path)) {
    b <- st_transform(st_read(building_path, quiet = TRUE), 27700)
    suppressWarnings(st_intersection(st_make_valid(b), st_buffer(neg_area, 2400)))
  } else {
    st_sf(geometry = st_sfc(crs = 27700))
  }

  adult_buffer <- st_buffer(origin, 900)
  caregiver_buffer <- st_buffer(origin, 810)
  older_buffer <- st_buffer(origin, 675)
  win <- st_bbox(st_buffer(neg_area, 2200))

  p_a <- ggplot() +
    geom_sf(data = countries, fill = "#D9DEDB", colour = "#A7AFAB", linewidth = 0.24) +
    geom_sf(data = london_point, shape = 21, size = 2.2, stroke = 0.55,
            fill = "#C985A2", colour = "#8F3658") +
    map_furniture(country_frame, 200) +
    labs(title = "a  United Kingdom", subtitle = "Greater London highlighted") +
    theme_map() + theme(legend.position = "none")

  east_win <- st_bbox(st_buffer(neg_area, 10500))
  east_window <- st_sf(geometry = st_as_sfc(east_win))
  p_b <- ggplot() +
    geom_sf(data = lsoas, fill = "#E5E7E3", colour = "#C9CECA", linewidth = 0.018) +
    geom_sf(data = thames, colour = "#78B4C8", linewidth = 0.62, alpha = 0.94) +
    geom_sf(data = boroughs, fill = NA, colour = "#949B96", linewidth = 0.20) +
    geom_sf(data = barking_borough, fill = "#D9ADB8", colour = palette$red, alpha = 0.52, linewidth = 0.34) +
    geom_sf(data = pos_area, fill = "#91A9BB", colour = palette$blue, alpha = 0.48, linewidth = 0.28) +
    geom_sf(data = neg_area, fill = "#C9879B", colour = "#9B455F", alpha = 0.78, linewidth = 0.48) +
    map_furniture(east_window, 5) +
    labs(title = "b  East London context", subtitle = "Barking borough and Newham counter-case") +
    theme_map() + theme(legend.position = "none")

  ring_df <- rbind(
    st_sf(ring = "Adult benchmark: 3.6 km/h", geometry = st_geometry(adult_buffer)),
    st_sf(ring = "Caregiver / child: 3.24 km/h", geometry = st_geometry(caregiver_buffer)),
    st_sf(ring = "Older adult: 2.70 km/h", geometry = st_geometry(older_buffer))
  )
  ring_df$ring <- factor(
    ring_df$ring,
    levels = c("Adult benchmark: 3.6 km/h", "Caregiver / child: 3.24 km/h", "Older adult: 2.70 km/h")
  )
  ring_lines <- st_sf(
    ring = ring_df$ring,
    geometry = st_boundary(st_geometry(ring_df)),
    crs = st_crs(lsoas)
  )
  ring_cols <- c(
    "Adult benchmark: 3.6 km/h" = "#4D7FA7",
    "Caregiver / child: 3.24 km/h" = "#C77F35",
    "Older adult: 2.70 km/h" = "#B64C59"
  )
  service_cols <- c(
    "School" = "#D4953F",
    "Food retail" = "#5D8C70",
    "Health / pharmacy" = "#7A66A5",
    "Transport entry" = "#386C9E"
  )
  service_shapes <- c(
    "School" = 16,
    "Food retail" = 15,
    "Health / pharmacy" = 17,
    "Transport entry" = 18
  )

  local_frame <- st_sf(geometry = st_as_sfc(win))
  p_c <- ggplot() +
    geom_sf(data = lsoas, fill = "#F1F2EF", colour = "#D2D5D0", linewidth = 0.060) +
    geom_sf(data = buildings, aes(fill = "Building footprint"), colour = "#687D89",
            linewidth = 0.035, alpha = 0.92, show.legend = TRUE) +
    geom_sf(data = roads, aes(colour = "Road network"), linewidth = 0.17,
            alpha = 0.88, show.legend = TRUE) +
    geom_sf(data = thames, aes(colour = "River Thames"), linewidth = 1.0,
            alpha = 0.92, show.legend = TRUE) +
    geom_sf(data = neg, aes(colour = "Selected case area"), fill = NA,
            linewidth = 0.66, show.legend = TRUE) +
    map_furniture(local_frame, 1, arrow_height_fraction = 0.105) +
    case_map_legend(win) +
    scale_fill_manual(values = c("Building footprint" = "#94A6B1"), name = NULL) +
    scale_colour_manual(
      values = c(
        "Selected case area" = "#A84F69",
        "Road network" = "#A3A39D",
        "River Thames" = "#8FC4D5"
      ),
      breaks = c("Selected case area", "Building footprint", "Road network", "River Thames"),
      name = NULL
    ) +
    guides(fill = "none", colour = "none") +
    labs(title = "c  Local service environment",
         subtitle = "Building footprints, road network and selected case boundary") +
    theme_map() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 7.4)
    )

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    pushViewport(viewport(layout = grid.layout(
      2, 2,
      widths = unit(c(0.72, 1.68), "null"),
      heights = unit(c(0.86, 0.86), "null")
    )))
    print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(p_c, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2))
    popViewport()
    grid.lines(
      x = unit(c(0.29, 0.37), "npc"), y = unit(c(0.70, 0.62), "npc"),
      gp = gpar(col = "#8D8D86", lwd = 0.55, lty = 2)
    )
    grid.lines(
      x = unit(c(0.29, 0.37), "npc"), y = unit(c(0.31, 0.41), "npc"),
      gp = gpar(col = "#8D8D86", lwd = 0.55, lty = 2)
    )
  }
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  base <- load_base()
  if ("fig12" %in% args) {
    save_nature(make_fig12(base), "figure12_case_study_accessibility_tripchain_ABC_nature", 183, 145)
    message("Saved revised Figure 12 to: ", out_dir)
    return(invisible(NULL))
  }
  if ("fig4" %in% args) {
    save_nature(make_fig4(base), "figure4_household_accessibility_abcd_nature", 183, 190)
    message("Saved revised Figure 4 to: ", out_dir)
    return(invisible(NULL))
  }
  if ("fig7" %in% args) {
    save_nature(make_fig7(base), "figure7_family_care_diagnostics_abcd_nature", 183, 190)
    message("Saved revised Figure 7 to: ", out_dir)
    return(invisible(NULL))
  }
  if ("fig6" %in% args) {
    save_nature(make_fig6(base), "figure6_household_mismatch_abcd_nature", 183, 190)
    message("Saved revised Figure 6 to: ", out_dir)
    return(invisible(NULL))
  }
  save_nature(make_fig4(base), "figure4_household_accessibility_abcd_nature", 183, 190)
  save_nature(make_fig6(base), "figure6_household_mismatch_abcd_nature", 183, 190)
  save_nature(make_fig7(base), "figure7_family_care_diagnostics_abcd_nature", 183, 190)
  save_nature(make_fig8(), "figure8_family_care_quadrant_diagnostic_nature", 160, 120)
  save_nature(make_fig12(base), "figure12_case_study_accessibility_tripchain_ABC_nature", 183, 145)
  message("Saved Nature-style revised figures to: ", out_dir)
}

main()
