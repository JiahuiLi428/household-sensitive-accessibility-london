library(sf)
library(ggplot2)
library(grid)
library(ragg)

boundary_dir <- file.path("data", "boundaries")
out_dir <- file.path("figures", "publication_nature_revised")
cache_dir <- file.path("data", "derived")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

negative_case <- c("E01000095", "E01000094", "E01034478", "E01000096", "E01000090", "E01034476", "E01000014")
panel_c_case <- setdiff(negative_case, "E01000014")

pal <- list(
  bg = "#F7F7F7",
  land = "#EFEFEF",
  lsoa = "#D0D0D0",
  road = "#DCDCDC",
  thames = "#CFE8F3",
  case = "#B2182B",
  case_fill = "#F4A3A3",
  border = "#333333",
  text = "#222222"
)

theme_panel_c <- function() {
  theme_void(base_size = 6.5, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = pal$bg, colour = NA),
      panel.border = element_rect(fill = NA, colour = pal$border, linewidth = 0.42),
      plot.margin = margin(0, 0, 0, 0)
    )
}

panel_label <- function(label, bbox) {
  xmin <- as.numeric(bbox["xmin"]); xmax <- as.numeric(bbox["xmax"])
  ymin <- as.numeric(bbox["ymin"]); ymax <- as.numeric(bbox["ymax"])
  annotate(
    "text",
    x = xmin + 0.028 * (xmax - xmin),
    y = ymax - 0.045 * (ymax - ymin),
    hjust = 0,
    label = label,
    size = 1.65,
    fontface = "bold",
    colour = pal$text
  )
}

scale_bar <- function(x, y, length_m = 1000) {
  list(
    annotate("segment", x = x, xend = x + length_m / 2, y = y, yend = y,
             linewidth = 0.68, colour = pal$text),
    annotate("segment", x = x + length_m / 2, xend = x + length_m, y = y, yend = y,
             linewidth = 0.68, colour = "#BDBDBD"),
    annotate("text", x = x, y = y + 95, label = "0", size = 1.35, colour = pal$text),
    annotate("text", x = x + length_m, y = y + 95, label = "1 km", size = 1.35, colour = pal$text)
  )
}

north_arrow <- function(x, y) {
  list(
    annotate("segment", x = x, xend = x, y = y, yend = y + 270,
             arrow = arrow(length = unit(0.085, "cm")), linewidth = 0.32, colour = pal$text),
    annotate("text", x = x, y = y + 355, label = "N", size = 1.55, fontface = "bold", colour = pal$text)
  )
}

legend_box <- function(xmin, xmax, ymin, ymax) {
  x0 <- xmin + 0.755 * (xmax - xmin)
  x1 <- xmax - 0.025 * (xmax - xmin)
  y0 <- ymin + 0.065 * (ymax - ymin)
  y1 <- ymin + 0.185 * (ymax - ymin)
  tx <- x0 + 120
  list(
    annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
             fill = scales::alpha("white", 0.88), colour = "#C7C7C7", linewidth = 0.22),
    annotate("rect", xmin = x0 + 45, xmax = x0 + 95, ymin = y1 - 130, ymax = y1 - 82,
             fill = scales::alpha(pal$case_fill, 0.09), colour = pal$case, linewidth = 0.38),
    annotate("text", x = tx, y = y1 - 106, hjust = 0, label = "Selected case area", size = 1.45, colour = pal$text),
    annotate("segment", x = x0 + 45, xend = x0 + 95, y = y1 - 210, yend = y1 - 210,
             colour = pal$lsoa, linewidth = 0.38),
    annotate("text", x = tx, y = y1 - 210, hjust = 0, label = "LSOA boundary", size = 1.45, colour = pal$text),
    annotate("segment", x = x0 + 45, xend = x0 + 95, y = y1 - 310, yend = y1 - 310,
             colour = pal$thames, linewidth = 1.15),
    annotate("text", x = tx, y = y1 - 310, hjust = 0, label = "River Thames", size = 1.45, colour = pal$text)
  )
}

fetch_roads <- function(bbox_27700, crs_target) {
  cache_file <- file.path(cache_dir, "figure12_panel_c_osm_roads.geojson")
  if (file.exists(cache_file)) {
    roads <- st_read(cache_file, quiet = TRUE)
    roads <- st_transform(roads, crs_target)
    roads <- roads[as.numeric(st_length(roads)) > 260, ]
    return(roads)
  }
  st_sf(geometry = st_sfc(crs = crs_target))
}

make_panel_c <- function() {
  lsoas <- st_make_valid(st_read(file.path(boundary_dir, "london_lsoa_2021.gpkg"), quiet = TRUE))
  thames <- st_transform(st_read(file.path(boundary_dir, "river_thames_osm.geojson"), quiet = TRUE), st_crs(lsoas))
  lsoas <- st_transform(lsoas, 27700)
  thames <- st_transform(thames, 27700)

  neg <- lsoas[lsoas$LSOA21CD %in% panel_c_case, ]
  neg_area <- st_sf(geometry = st_sfc(st_union(st_geometry(neg)), crs = st_crs(lsoas)))
  panel_bbox <- st_bbox(st_buffer(neg_area, 1900))
  panel_sfc <- st_as_sfc(panel_bbox)
  local_lsoas <- lsoas[lengths(st_intersects(lsoas, panel_sfc)) > 0, ]
  roads <- fetch_roads(panel_bbox, st_crs(lsoas))
  if (nrow(roads) > 0) {
    roads <- suppressWarnings(st_intersection(st_make_valid(roads), panel_sfc))
  }

  xmin <- as.numeric(panel_bbox["xmin"]); xmax <- as.numeric(panel_bbox["xmax"])
  ymin <- as.numeric(panel_bbox["ymin"]); ymax <- as.numeric(panel_bbox["ymax"])

  ggplot() +
    geom_sf(data = local_lsoas, fill = pal$land, colour = scales::alpha(pal$lsoa, 0.72), linewidth = 0.075) +
    geom_sf(data = roads, colour = "#CFCFCF", linewidth = 0.13, alpha = 0.58) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 1.45, alpha = 0.98) +
    geom_sf(data = neg_area, fill = NA, colour = "white", linewidth = 0.90) +
    geom_sf(data = neg_area, fill = NA, colour = pal$case, linewidth = 0.58) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    panel_label("(c) Barking Riverside / Thames View", panel_bbox) +
    scale_bar(xmin + 260, ymin + 240, 1000) +
    north_arrow(xmax - 260, ymax - 520) +
    theme_panel_c()
}

save_panel_c <- function(plot, stem, width_mm = 120, height_mm = 90, dpi = 600) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  grDevices::svg(file.path(out_dir, paste0(stem, ".svg")), width = w, height = h,
                 family = "Arial", pointsize = 6.5)
  print(plot)
  dev.off()
  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = w, height = h,
                       family = "Arial", pointsize = 6.5)
  print(plot)
  dev.off()
  ragg::agg_png(file.path(out_dir, paste0(stem, ".png")), width = w, height = h,
                units = "in", res = dpi, background = "white")
  print(plot)
  dev.off()
}

panel_c <- make_panel_c()
save_panel_c(panel_c, "figure12_panel_c_clean_locator")
message("Saved clean Panel C locator map to: ", out_dir)
