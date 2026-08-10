library(sf)
library(ggplot2)
library(grid)
library(ragg)

boundary_dir <- file.path("data", "boundaries")
derived_dir <- file.path("data", "derived")
out_dir <- file.path("figures", "publication")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

case_codes <- c("E01000095", "E01000094", "E01034478", "E01000096", "E01000090", "E01034476", "E01000014")

pal <- list(
  sea = "#F8FAFB", land = "#F2E9DC", context = "#D6D6D2",
  border = "#4D4D4D", london = "#B8862E", case = "#8C2F39",
  road = "#C6C6C2", building = "#C98C8C", river = "#B8C4DC",
  text = "#242424", connector = "#4D4D4D"
)

theme_locator <- function() {
  theme_void(base_size = 6.5, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = pal$sea, colour = NA),
      panel.border = element_rect(fill = NA, colour = pal$border, linewidth = 0.38),
      plot.title = element_text(size = 7.1, face = "bold", colour = pal$text, margin = margin(2, 2, 2, 3)),
      plot.margin = margin(0, 0, 0, 0)
    )
}

scale_bar <- function(x, y, length_m, label) {
  list(
    annotate("segment", x = x, xend = x + length_m / 2, y = y, yend = y, linewidth = 0.7, colour = pal$text),
    annotate("segment", x = x + length_m / 2, xend = x + length_m, y = y, yend = y, linewidth = 0.7, colour = "#BDBDBD"),
    annotate("text", x = x, y = y + length_m * 0.07, label = "0", size = 1.45),
    annotate("text", x = x + length_m, y = y + length_m * 0.07, label = label, size = 1.45)
  )
}

north_arrow <- function(x, y, length_m) {
  list(
    annotate("segment", x = x, xend = x, y = y, yend = y + length_m,
             arrow = arrow(length = unit(0.09, "cm"), type = "closed"), linewidth = 0.38),
    annotate("text", x = x, y = y + length_m * 1.22, label = "N", size = 1.7, fontface = "bold")
  )
}

lsoas <- st_transform(st_make_valid(st_read(file.path(boundary_dir, "london_lsoa_2021.gpkg"), quiet = TRUE)), 27700)
thames <- st_transform(st_read(file.path(boundary_dir, "river_thames_osm.geojson"), quiet = TRUE), 27700)
lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
boroughs <- aggregate(lsoas["borough_name"], by = list(lsoas$borough_name), FUN = length)
boroughs$borough_name <- boroughs$Group.1
boroughs$Group.1 <- NULL
boroughs <- st_make_valid(boroughs)
london <- st_sf(geometry = st_sfc(st_union(st_geometry(boroughs)), crs = st_crs(lsoas)))
case_lsoas <- lsoas[lsoas$LSOA21CD %in% case_codes, ]
case_union <- st_sf(geometry = st_sfc(st_union(st_geometry(case_lsoas)), crs = st_crs(lsoas)))

countries <- st_transform(st_read(
  file.path(boundary_dir, "Countries_December_2022_Boundaries_UK_BGC.gpkg"),
  quiet = TRUE
), 27700)
country_bbox <- st_bbox(countries)
london_centroid <- st_coordinates(st_centroid(london))[1, ]

p_country <- ggplot() +
  geom_sf(data = countries, fill = pal$land, colour = "white", linewidth = 0.16) +
  annotate("point", x = london_centroid[1], y = london_centroid[2], shape = 21,
           size = 3.0, stroke = 0.9, fill = pal$london, colour = pal$case) +
  annotate("text", x = london_centroid[1] - 90000, y = london_centroid[2] + 30000,
           label = "Greater London", hjust = 1, size = 1.75, fontface = "bold", colour = pal$text) +
  coord_sf(xlim = c(country_bbox["xmin"], country_bbox["xmax"]),
           ylim = c(country_bbox["ymin"], country_bbox["ymax"]), expand = FALSE) +
  labs(title = "a  United Kingdom") + theme_locator()

london_bbox <- st_bbox(st_buffer(london, 7000))
case_bbox <- st_bbox(st_buffer(case_union, 1800))
bd_lsoas <- lsoas[lsoas$borough_name == "Barking and Dagenham", ]
bd <- st_sf(geometry = st_sfc(st_union(st_geometry(bd_lsoas)), crs = st_crs(lsoas)))
bd_xy <- st_coordinates(st_point_on_surface(bd))[1, ]
p_london <- ggplot() +
  geom_sf(data = boroughs, fill = pal$context, colour = "white", linewidth = 0.22) +
  geom_sf(data = london, fill = NA, colour = pal$border, linewidth = 0.38) +
  geom_sf(data = bd,
          fill = pal$london, colour = pal$case, linewidth = 0.55) +
  geom_sf(data = thames, colour = pal$river, linewidth = 0.75) +
  annotate("rect", xmin = case_bbox["xmin"], xmax = case_bbox["xmax"],
           ymin = case_bbox["ymin"], ymax = case_bbox["ymax"],
           fill = NA, colour = pal$case, linewidth = 0.7) +
  annotate("text", x = bd_xy[1], y = bd_xy[2], label = "Barking &\nDagenham",
           hjust = 0.5, size = 1.45, fontface = "bold", colour = pal$text) +
  coord_sf(xlim = c(london_bbox["xmin"], london_bbox["xmax"]),
           ylim = c(london_bbox["ymin"], london_bbox["ymax"]), expand = FALSE) +
  labs(title = "b  Greater London") + theme_locator()

buildings <- st_transform(st_read(file.path(derived_dir, "figure12_panel_c_osm_buildings.gpkg"), quiet = TRUE), 27700)
roads <- st_transform(st_read(file.path(derived_dir, "figure12_panel_c_osm_roads.geojson"), quiet = TRUE), 27700)
case_window <- st_as_sfc(case_bbox)
buildings <- suppressWarnings(st_intersection(buildings, case_window))
roads <- suppressWarnings(st_intersection(st_make_valid(roads), case_window))
local_lsoas <- lsoas[lengths(st_intersects(lsoas, case_window)) > 0, ]
label_pts <- st_point_on_surface(case_lsoas)
label_xy <- cbind(st_drop_geometry(label_pts), st_coordinates(label_pts))
label_xy$label <- sub("Barking and Dagenham ", "", label_xy$LSOA21NM, fixed = TRUE)

p_case <- ggplot() +
  geom_sf(data = local_lsoas, fill = "#F7F7F5", colour = "#D5D5D0", linewidth = 0.10) +
  geom_sf(data = buildings, fill = pal$building, colour = NA, alpha = 0.48) +
  geom_sf(data = roads, colour = pal$road, linewidth = 0.15, alpha = 0.85) +
  geom_sf(data = thames, colour = pal$river, linewidth = 1.15) +
  geom_sf(data = case_lsoas, fill = NA, colour = pal$case, linewidth = 0.56) +
  geom_text(data = label_xy, aes(X, Y, label = label), size = 1.65, fontface = "bold", colour = pal$text) +
  scale_bar(case_bbox["xmin"] + 250, case_bbox["ymin"] + 240, 1000, "1 km") +
  north_arrow(case_bbox["xmax"] - 300, case_bbox["ymax"] - 620, 350) +
  coord_sf(xlim = c(case_bbox["xmin"], case_bbox["xmax"]),
           ylim = c(case_bbox["ymin"], case_bbox["ymax"]), expand = FALSE) +
  labs(title = "c  Barking Riverside / Thames View / Goresbrook case cluster") + theme_locator()

draw_nested <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  print(p_country, vp = viewport(x = 0.165, y = 0.745, width = 0.31, height = 0.43))
  print(p_london, vp = viewport(x = 0.165, y = 0.265, width = 0.31, height = 0.43))
  print(p_case, vp = viewport(x = 0.675, y = 0.505, width = 0.64, height = 0.91))
  grid.lines(unit(c(0.305, 0.355), "npc"), unit(c(0.69, 0.62), "npc"),
             gp = gpar(col = pal$connector, lwd = 0.7, lty = 2))
  grid.lines(unit(c(0.305, 0.355), "npc"), unit(c(0.30, 0.42), "npc"),
             gp = gpar(col = pal$connector, lwd = 0.7, lty = 2))
}

save_nested <- function(stem, width_mm = 183, height_mm = 125, dpi = 600) {
  w <- width_mm / 25.4; h <- height_mm / 25.4
  svglite::svglite(file.path(out_dir, paste0(stem, ".svg")), width = w, height = h,
                   system_fonts = list(sans = "Arial"))
  draw_nested(); dev.off()
  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = w, height = h, family = "Arial", pointsize = 6.5)
  draw_nested(); dev.off()
  ragg::agg_png(file.path(out_dir, paste0(stem, ".png")), width = w, height = h, units = "in", res = dpi, background = "white")
  draw_nested(); dev.off()
  ragg::agg_tiff(file.path(out_dir, paste0(stem, ".tiff")), width = w, height = h, units = "in", res = dpi,
                 compression = "lzw", background = "white")
  draw_nested(); dev.off()
}

save_nested("figure12_nested_locator_map")
message("Saved Figure 12 nested locator map to: ", out_dir)
