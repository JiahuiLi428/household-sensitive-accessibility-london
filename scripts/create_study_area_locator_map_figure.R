library(sf)
library(ggplot2)
library(grid)
library(ragg)

boundary_dir <- file.path("data", "boundaries")
out_dir <- file.path("figures", "publication_nature_revised")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pal <- list(
  bg = "#F5F5F5",
  land = "#EFEFEF",
  thames = "#D2E8F0",
  boundary = "#CFCFCF",
  community = "#174A73",
  case = "#B2182B",
  comparison = "#4C78A8",
  border = "#333333",
  text = "#222222"
)

negative_case <- c("E01000095", "E01000094", "E01034478", "E01000096", "E01000090", "E01034476", "E01000014")
positive_case <- c("E01003566", "E01003525", "E01003531", "E01003607")

theme_locator_map <- function() {
  theme_void(base_size = 6.3, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = pal$bg, colour = NA),
      panel.border = element_rect(fill = NA, colour = pal$border, linewidth = 0.38),
      plot.margin = margin(0, 0, 0, 0)
    )
}

panel_label <- function(label, bbox, colour = pal$text) {
  xmin <- as.numeric(bbox["xmin"]); xmax <- as.numeric(bbox["xmax"])
  ymin <- as.numeric(bbox["ymin"]); ymax <- as.numeric(bbox["ymax"])
  annotate(
    "text",
    x = xmin + 0.035 * (xmax - xmin),
    y = ymax - 0.055 * (ymax - ymin),
    hjust = 0,
    label = label,
    size = 2.2,
    fontface = "bold",
    colour = colour
  )
}

scale_bar <- function(x, y, length_m, label) {
  list(
    annotate("segment", x = x, xend = x + length_m / 2, y = y, yend = y,
             linewidth = 0.95, colour = "#222222"),
    annotate("segment", x = x + length_m / 2, xend = x + length_m, y = y, yend = y,
             linewidth = 0.95, colour = "#BDBDBD"),
    annotate("text", x = x, y = y + length_m * 0.09, label = "0", size = 1.65, colour = pal$text),
    annotate("text", x = x + length_m, y = y + length_m * 0.09, label = label, size = 1.65, colour = pal$text)
  )
}

north_arrow <- function(x, y, length_m) {
  list(
    annotate("segment", x = x, xend = x, y = y, yend = y + length_m,
             arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.45, colour = pal$text),
    annotate("text", x = x, y = y + length_m * 1.18, label = "N", size = 2.0, fontface = "bold", colour = pal$text)
  )
}

legend_box <- function(xmin, xmax, ymin, ymax) {
  x0 <- xmin + 0.745 * (xmax - xmin)
  x1 <- xmax - 0.025 * (xmax - xmin)
  y0 <- ymin + 0.055 * (ymax - ymin)
  y1 <- ymin + 0.255 * (ymax - ymin)
  tx <- x0 + 145
  list(
    annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
             fill = scales::alpha("white", 0.88), colour = "#C7C7C7", linewidth = 0.24),
    annotate("rect", xmin = x0 + 58, xmax = x0 + 118, ymin = y1 - 165, ymax = y1 - 105,
             fill = scales::alpha("#F4A3A3", 0.16), colour = pal$case, linewidth = 0.40),
    annotate("text", x = tx, y = y1 - 135, hjust = 0, label = "Selected case area", size = 1.7, colour = pal$text),
    annotate("rect", xmin = x0 + 58, xmax = x0 + 118, ymin = y1 - 305, ymax = y1 - 245,
             fill = "#E5EAED", colour = "#AAB4BA", linewidth = 0.20),
    annotate("text", x = tx, y = y1 - 275, hjust = 0, label = "LSOA / neighbourhood", size = 1.7, colour = pal$text),
    annotate("segment", x = x0 + 58, xend = x0 + 118, y = y1 - 415, yend = y1 - 415,
             colour = pal$thames, linewidth = 1.15),
    annotate("text", x = tx, y = y1 - 415, hjust = 0, label = "River Thames", size = 1.7, colour = pal$text)
  )
}

read_layers <- function() {
  lsoas <- st_make_valid(st_read(file.path(boundary_dir, "london_lsoa_2021.gpkg"), quiet = TRUE))
  thames <- st_transform(st_read(file.path(boundary_dir, "river_thames_osm.geojson"), quiet = TRUE), st_crs(lsoas))
  lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
  borough_geom <- aggregate(st_geometry(lsoas), by = list(borough_name = lsoas$borough_name), FUN = st_union)
  boroughs <- st_sf(borough_name = borough_geom$borough_name, geometry = st_sfc(borough_geom$x, crs = st_crs(lsoas)))
  list(lsoas = st_transform(lsoas, 27700), boroughs = st_transform(st_make_valid(boroughs), 27700), thames = st_transform(thames, 27700))
}

make_study_area_locator <- function() {
  base <- read_layers()
  lsoas <- base$lsoas
  boroughs <- base$boroughs
  thames <- base$thames

  london_area <- st_sf(geometry = st_sfc(st_union(st_geometry(lsoas)), crs = st_crs(lsoas)))
  neg <- lsoas[lsoas$LSOA21CD %in% negative_case, ]
  pos <- lsoas[lsoas$LSOA21CD %in% positive_case, ]
  neg_area <- st_sf(geometry = st_sfc(st_union(st_geometry(neg)), crs = st_crs(lsoas)))
  pos_area <- st_sf(geometry = st_sfc(st_union(st_geometry(pos)), crs = st_crs(lsoas)))

  a_bbox <- st_bbox(london_area)
  east_union <- st_union(st_geometry(st_buffer(neg_area, 4200)), st_geometry(st_buffer(pos_area, 4200)))
  b_bbox <- st_bbox(st_sf(geometry = st_sfc(st_union(east_union), crs = st_crs(lsoas))))
  c_bbox <- st_bbox(st_buffer(neg_area, 1200))

  local_lsoas <- lsoas[lengths(st_intersects(lsoas, st_as_sfc(c_bbox))) > 0, ]

  p_a <- ggplot() +
    geom_sf(data = london_area, fill = pal$land, colour = NA) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 0.42, alpha = 0.95) +
    geom_sf(data = neg_area, fill = scales::alpha("#F4A3A3", 0.20), colour = "white", linewidth = 1.0) +
    geom_sf(data = neg_area, fill = scales::alpha("#F4A3A3", 0.24), colour = pal$case, linewidth = 0.55) +
    coord_sf(xlim = c(a_bbox["xmin"], a_bbox["xmax"]), ylim = c(a_bbox["ymin"], a_bbox["ymax"]), expand = FALSE) +
    panel_label("(a) Greater London", a_bbox) +
    scale_bar(a_bbox["xmin"] + 5000, a_bbox["ymin"] + 4500, 20000, "20 km") +
    theme_locator_map()

  p_b <- ggplot() +
    geom_sf(data = lsoas, fill = pal$land, colour = "white", linewidth = 0.015) +
    geom_sf(data = boroughs, fill = NA, colour = pal$boundary, linewidth = 0.22) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 0.80, alpha = 0.95) +
    geom_sf(data = pos_area, fill = scales::alpha(pal$comparison, 0.14), colour = pal$comparison, linewidth = 0.60) +
    geom_sf(data = neg_area, fill = scales::alpha("#F4A3A3", 0.20), colour = pal$case, linewidth = 0.65) +
    coord_sf(xlim = c(b_bbox["xmin"], b_bbox["xmax"]), ylim = c(b_bbox["ymin"], b_bbox["ymax"]), expand = FALSE) +
    panel_label("(b) East London", b_bbox) +
    scale_bar(b_bbox["xmin"] + 650, b_bbox["ymin"] + 650, 3000, "3 km") +
    theme_locator_map()

  p_c <- ggplot() +
    geom_sf(data = lsoas, fill = pal$land, colour = NA) +
    geom_sf(data = local_lsoas, fill = "#E5EAED", colour = "#AAB4BA", linewidth = 0.16) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 1.25, alpha = 0.98) +
    geom_sf(data = neg_area, fill = NA, colour = "white", linewidth = 1.25) +
    geom_sf(data = neg_area, fill = scales::alpha("#E7A9B8", 0.58), colour = pal$case, linewidth = 0.78) +
    coord_sf(xlim = c(c_bbox["xmin"], c_bbox["xmax"]), ylim = c(c_bbox["ymin"], c_bbox["ymax"]), expand = FALSE) +
    panel_label("(c) Barking Riverside / Thames View case area", c_bbox, colour = pal$text) +
    scale_bar(c_bbox["xmin"] + 230, c_bbox["ymin"] + 220, 1000, "1 km") +
    north_arrow(c_bbox["xmax"] - 260, c_bbox["ymax"] - 650, 420) +
    legend_box(c_bbox["xmin"], c_bbox["xmax"], c_bbox["ymin"], c_bbox["ymax"]) +
    theme_locator_map()

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    print(p_a, vp = viewport(x = unit(0.151, "npc"), y = unit(0.770, "npc"),
                             width = unit(0.302, "npc"), height = unit(0.355, "npc")))
    print(p_b, vp = viewport(x = unit(0.151, "npc"), y = unit(0.268, "npc"),
                             width = unit(0.302, "npc"), height = unit(0.355, "npc")))
    print(p_c, vp = viewport(x = unit(0.652, "npc"), y = unit(0.500, "npc"),
                             width = unit(0.688, "npc"), height = unit(0.900, "npc")))
    grid.lines(unit(c(0.302, 0.328), "npc"), unit(c(0.765, 0.690), "npc"),
               gp = gpar(col = "#555555", lwd = 0.6, lty = 2))
    grid.lines(unit(c(0.302, 0.328), "npc"), unit(c(0.300, 0.430), "npc"),
               gp = gpar(col = "#555555", lwd = 0.6, lty = 2))
  }
}

save_study_area_locator <- function(draw_fun, stem, width_mm = 180, height_mm = 120, dpi = 600) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  grDevices::svg(file.path(out_dir, paste0(stem, ".svg")), width = w, height = h,
                 family = "Arial", pointsize = 6.3)
  draw_fun()
  dev.off()
  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = w, height = h,
                       family = "Arial", pointsize = 6.3)
  draw_fun()
  dev.off()
  ragg::agg_png(file.path(out_dir, paste0(stem, ".png")), width = w, height = h,
                units = "in", res = dpi, background = "white")
  draw_fun()
  dev.off()
}

figure <- make_study_area_locator()
save_study_area_locator(figure, "figure12_study_area_locator_map")
message("Saved study area locator map to: ", out_dir)
