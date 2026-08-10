library(sf)
library(ggplot2)
library(grid)
library(ragg)

boundary_dir <- file.path("data", "boundaries")
poi_dir <- file.path("data", "poi")
out_dir <- file.path("figures", "publication_nature_revised")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pal <- list(
  background = "#FFFFFF",
  london = "#F0F0F0",
  local_bg = "#F7F7F7",
  boundary = "#CFCFCF",
  road = "#E2E2E2",
  thames = "#BFDDE8",
  case = "#B84A55",
  case_fill = "#F4A3A3",
  comparison = "#6E93B5",
  school = "#D39B43",
  food = "#5E8B6A",
  health = "#7A6AB3",
  transport = "#3D6FA8",
  adult = "#2C7FB8",
  caregiver = "#E08214",
  older = "#C44E52",
  text = "#333333",
  muted = "#6E6E6E"
)

theme_case_map <- function() {
  theme_void(base_size = 6.4, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$background, colour = NA),
      plot.title = element_text(face = "bold", size = 7.3, colour = pal$text, margin = margin(b = 1.2)),
      plot.subtitle = element_text(size = 5.8, colour = pal$muted, margin = margin(b = 2.2)),
      plot.margin = margin(2.2, 2.2, 2.2, 2.2)
    )
}

read_base_layers <- function() {
  lsoas <- st_make_valid(st_read(file.path(boundary_dir, "london_lsoa_2021.gpkg"), quiet = TRUE))
  thames <- st_transform(st_read(file.path(boundary_dir, "river_thames_osm.geojson"), quiet = TRUE), st_crs(lsoas))
  lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
  borough_geom <- aggregate(st_geometry(lsoas), by = list(borough_name = lsoas$borough_name), FUN = st_union)
  boroughs <- st_sf(
    borough_name = borough_geom$borough_name,
    geometry = st_sfc(borough_geom$x, crs = st_crs(lsoas))
  )
  list(lsoas = lsoas, boroughs = st_make_valid(boroughs), thames = thames)
}

read_pois <- function(crs) {
  specs <- data.frame(
    group = c("School / childcare", "Food retail", "Health / pharmacy", "Health / pharmacy", "Transport node"),
    path = file.path(poi_dir, c(
      "education_schools.geojson",
      "food_retail_supermarkets.geojson",
      "healthcare_gps.geojson",
      "healthcare_pharmacies.geojson",
      "transport_underground_metro_stations.geojson"
    )),
    stringsAsFactors = FALSE
  )
  layers <- lapply(seq_len(nrow(specs)), function(i) {
    x <- st_read(specs$path[i], quiet = TRUE)
    x <- st_transform(x, crs)
    x$poi_group <- specs$group[i]
    x[, c("poi_group", "geometry")]
  })
  out <- do.call(rbind, layers)
  out$poi_group <- factor(out$poi_group, levels = c(
    "School / childcare", "Food retail", "Health / pharmacy", "Transport node"
  ))
  out
}

read_roads <- function(case_area, crs) {
  pbf <- file.path("data", "raw", "osm", "greater-london-latest.osm.pbf")
  if (!file.exists(pbf)) return(st_sf(geometry = st_sfc(crs = crs)))
  keep_highway <- c("primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "pedestrian")
  roads <- tryCatch(
    st_read(pbf, layer = "lines", quiet = TRUE, stringsAsFactors = FALSE, options = "INTERLEAVED_READING=YES"),
    error = function(e) NULL
  )
  if (is.null(roads) || nrow(roads) == 0 || !"highway" %in% names(roads)) {
    return(st_sf(geometry = st_sfc(crs = crs)))
  }
  roads <- roads[!is.na(roads$highway) & roads$highway %in% keep_highway, ]
  roads <- st_transform(roads, crs)
  roads <- suppressWarnings(st_intersection(st_make_valid(roads), st_buffer(case_area, 2300)))
  roads[, "geometry"]
}

add_scale_bar <- function(xmin, ymin, length_m = 1000) {
  list(
    annotate("segment", x = xmin, xend = xmin + length_m / 2, y = ymin, yend = ymin,
             linewidth = 1.0, colour = pal$text),
    annotate("segment", x = xmin + length_m / 2, xend = xmin + length_m, y = ymin, yend = ymin,
             linewidth = 1.0, colour = "#BDBDBD"),
    annotate("text", x = xmin, y = ymin + 115, label = "0", size = 1.75, colour = pal$text),
    annotate("text", x = xmin + length_m, y = ymin + 115, label = "1 km", size = 1.75, colour = pal$text)
  )
}

add_north_arrow <- function(x, y) {
  list(
    annotate("segment", x = x, xend = x, y = y - 260, yend = y + 250,
             arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.42, colour = pal$text),
    annotate("text", x = x, y = y + 390, label = "N", size = 2.1, fontface = "bold", colour = pal$text)
  )
}

add_custom_legend <- function(xmin, xmax, ymin, ymax) {
  x0 <- xmin + 0.735 * (xmax - xmin)
  x1 <- xmax - 135
  y1 <- ymin + 0.390 * (ymax - ymin)
  tx <- x0 + 185
  y_area <- y1 - 70
  y_sel <- y_area - 155
  y_facilities <- y_sel - 200
  y_school <- y_facilities - 150
  y_food <- y_school - 135
  y_health <- y_food - 135
  y_transport <- y_health - 135
  y_walk <- y_transport - 185
  y_adult <- y_walk - 240
  y_caregiver <- y_adult - 135
  y_older <- y_caregiver - 135
  y0 <- y_older - 115
  list(
    annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
             fill = "white", colour = "#D2D2D2", linewidth = 0.25, alpha = 0.90),
    annotate("text", x = x0 + 70, y = y_area, hjust = 0, label = "Area",
             size = 1.95, fontface = "bold", colour = pal$text),
    annotate("rect", xmin = x0 + 70, xmax = x0 + 145, ymin = y_sel - 35, ymax = y_sel + 35,
             fill = scales::alpha(pal$case_fill, 0.15), colour = pal$case, linewidth = 0.35),
    annotate("text", x = tx, y = y_sel, hjust = 0, label = "Selected case area", size = 1.75, colour = pal$text),
    annotate("text", x = x0 + 70, y = y_facilities, hjust = 0, label = "Daily facilities",
             size = 1.95, fontface = "bold", colour = pal$text),
    annotate("point", x = x0 + 105, y = y_school, shape = 16, size = 1.65, colour = pal$school),
    annotate("text", x = tx, y = y_school, hjust = 0, label = "School / childcare", size = 1.75, colour = pal$text),
    annotate("point", x = x0 + 105, y = y_food, shape = 15, size = 1.55, colour = pal$food),
    annotate("text", x = tx, y = y_food, hjust = 0, label = "Food retail", size = 1.75, colour = pal$text),
    annotate("point", x = x0 + 105, y = y_health, shape = 17, size = 1.65, colour = pal$health),
    annotate("text", x = tx, y = y_health, hjust = 0, label = "Health / pharmacy", size = 1.75, colour = pal$text),
    annotate("point", x = x0 + 105, y = y_transport, shape = 18, size = 1.65, colour = pal$transport),
    annotate("text", x = tx, y = y_transport, hjust = 0, label = "Transport node", size = 1.75, colour = pal$text),
    annotate("text", x = x0 + 70, y = y_walk, hjust = 0, label = "Illustrative 15-min\nwalking distance",
             size = 1.95, fontface = "bold", colour = pal$text),
    annotate("segment", x = x0 + 65, xend = x0 + 145, y = y_adult, yend = y_adult,
             colour = pal$adult, linewidth = 0.62),
    annotate("text", x = tx, y = y_adult, hjust = 0, label = "Adult: 900 m", size = 1.75, colour = pal$text),
    annotate("segment", x = x0 + 65, xend = x0 + 145, y = y_caregiver, yend = y_caregiver,
             colour = pal$caregiver, linewidth = 0.62),
    annotate("text", x = tx, y = y_caregiver, hjust = 0, label = "Caregiver / child: 810 m", size = 1.75, colour = pal$text),
    annotate("segment", x = x0 + 65, xend = x0 + 145, y = y_older, yend = y_older,
             colour = pal$older, linewidth = 0.62),
    annotate("text", x = tx, y = y_older, hjust = 0, label = "Older adult: 675 m", size = 1.75, colour = pal$text)
  )
}

make_case_context_figure <- function() {
  negative_case <- c("E01000095", "E01000094", "E01034478", "E01000096", "E01000090", "E01034476", "E01000014")
  positive_case <- c("E01003566", "E01003525", "E01003531", "E01003607")

  base <- read_base_layers()
  lsoas <- st_transform(base$lsoas, 27700)
  boroughs <- st_transform(base$boroughs, 27700)
  thames <- st_transform(base$thames, 27700)
  london_area <- st_sf(geometry = st_sfc(st_union(st_geometry(lsoas)), crs = st_crs(lsoas)))
  neg <- lsoas[lsoas$LSOA21CD %in% negative_case, ]
  pos <- lsoas[lsoas$LSOA21CD %in% positive_case, ]
  neg_area <- st_sf(geometry = st_sfc(st_union(st_geometry(neg)), crs = st_crs(lsoas)))
  pos_area <- st_sf(geometry = st_sfc(st_union(st_geometry(pos)), crs = st_crs(lsoas)))
  borough_case <- boroughs[grepl("Barking", boroughs$borough_name), ]

  origin <- st_point_on_surface(neg_area)
  origin_xy <- st_coordinates(origin)[1, ]
  case_xy <- st_coordinates(st_point_on_surface(neg_area))[1, ]
  pois <- read_pois(st_crs(lsoas))
  local_extent <- st_buffer(neg_area, 1500)
  local_pois <- pois[lengths(st_within(pois, local_extent)) > 0, ]
  roads <- read_roads(neg_area, st_crs(lsoas))

  rings <- rbind(
    st_sf(ring = "Adult: 900 m", geometry = st_geometry(st_buffer(origin, 900))),
    st_sf(ring = "Caregiver / child: 810 m", geometry = st_geometry(st_buffer(origin, 810))),
    st_sf(ring = "Older adult: 675 m", geometry = st_geometry(st_buffer(origin, 675)))
  )
  rings$ring <- factor(rings$ring, levels = c("Adult: 900 m", "Caregiver / child: 810 m", "Older adult: 675 m"))
  ring_lines <- st_sf(ring = rings$ring, geometry = st_boundary(st_geometry(rings)), crs = st_crs(lsoas))

  p_a <- ggplot() +
    geom_sf(data = london_area, fill = pal$london, colour = NA) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 0.35) +
    geom_sf(data = neg_area, fill = NA, colour = "white", linewidth = 1.05) +
    geom_sf(data = neg_area, fill = scales::alpha(pal$case_fill, 0.22), colour = pal$case, linewidth = 0.55) +
    annotate("segment", x = case_xy[1] - 6200, xend = case_xy[1] - 600,
             y = case_xy[2] + 6200, yend = case_xy[2] + 1300,
             linewidth = 0.18, colour = pal$case) +
    annotate("label", x = case_xy[1] - 15000, y = case_xy[2] + 7600,
             label = "Selected\ncase area", hjust = 0, size = 1.7,
             colour = pal$case, fill = scales::alpha("white", 0.82), linewidth = 0.10) +
    labs(title = "a  Case location within Greater London", subtitle = "Red = selected case area") +
    theme_case_map()

  east_context <- st_union(st_geometry(st_buffer(neg_area, 2500)), st_geometry(st_buffer(pos_area, 2500)))
  east_bbox <- st_bbox(st_sf(geometry = st_sfc(st_union(east_context), crs = st_crs(lsoas))))
  p_b <- ggplot() +
    geom_sf(data = lsoas, fill = "#F5F5F5", colour = "white", linewidth = 0.01) +
    geom_sf(data = boroughs, fill = NA, colour = pal$boundary, linewidth = 0.18) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 0.65) +
    geom_sf(data = borough_case, fill = scales::alpha(pal$case_fill, 0.08), colour = "#D7A0A5", linewidth = 0.25) +
    geom_sf(data = pos_area, fill = scales::alpha(pal$comparison, 0.20), colour = pal$comparison, linewidth = 0.48) +
    geom_sf(data = neg_area, fill = scales::alpha(pal$case_fill, 0.20), colour = pal$case, linewidth = 0.55) +
    coord_sf(xlim = c(east_bbox["xmin"], east_bbox["xmax"]), ylim = c(east_bbox["ymin"], east_bbox["ymax"]), expand = FALSE) +
    labs(title = "b  Case and comparison context", subtitle = "Selected case and Newham comparison area") +
    theme_case_map()

  local_focus <- st_union(st_geometry(neg_area), st_geometry(st_buffer(origin, 900)))
  local_bbox <- st_bbox(st_sf(geometry = st_sfc(st_buffer(st_union(local_focus), 1250), crs = st_crs(lsoas))))
  c_xmin <- as.numeric(local_bbox["xmin"])
  c_xmax <- as.numeric(local_bbox["xmax"])
  c_ymin <- as.numeric(local_bbox["ymin"])
  c_ymax <- as.numeric(local_bbox["ymax"])

  poi_cols <- c(
    "School / childcare" = pal$school,
    "Food retail" = pal$food,
    "Health / pharmacy" = pal$health,
    "Transport node" = pal$transport
  )
  poi_shapes <- c("School / childcare" = 16, "Food retail" = 15, "Health / pharmacy" = 17, "Transport node" = 18)
  ring_cols <- c("Adult: 900 m" = pal$adult, "Caregiver / child: 810 m" = pal$caregiver, "Older adult: 675 m" = pal$older)
  ring_types <- c("Adult: 900 m" = "solid", "Caregiver / child: 810 m" = "solid", "Older adult: 675 m" = "22")

  p_c <- ggplot() +
    geom_sf(data = lsoas, fill = pal$local_bg, colour = pal$boundary, linewidth = 0.035) +
    geom_sf(data = roads, colour = pal$road, linewidth = 0.12, alpha = 0.72) +
    geom_sf(data = thames, colour = pal$thames, linewidth = 1.15) +
    geom_sf(data = neg_area, fill = scales::alpha(pal$case_fill, 0.09), colour = pal$case, linewidth = 0.46) +
    geom_sf(data = ring_lines, aes(colour = ring, linetype = ring), linewidth = 0.66, alpha = 0.92) +
    geom_sf(data = local_pois, aes(colour = poi_group, shape = poi_group), size = 0.90, alpha = 0.70) +
    geom_sf(data = origin, shape = 8, size = 1.85, colour = pal$text, linewidth = 0.45) +
    coord_sf(xlim = c(c_xmin, c_xmax), ylim = c(c_ymin, c_ymax), expand = FALSE) +
    scale_colour_manual(values = c(poi_cols, ring_cols), guide = "none") +
    scale_linetype_manual(values = ring_types, guide = "none") +
    scale_shape_manual(values = poi_shapes, guide = "none") +
    annotate("label", x = origin_xy[1] - 760, y = origin_xy[2] - 520,
             label = "Illustrative origin", hjust = 0, size = 1.65,
             colour = pal$text, fill = scales::alpha("white", 0.72), linewidth = 0.10) +
    annotate("label", x = origin_xy[1] + 990, y = origin_xy[2] + 760,
             label = "Adult 900 m", hjust = 0, size = 1.55,
             colour = pal$adult, fill = scales::alpha("white", 0.78), linewidth = 0.10) +
    annotate("label", x = origin_xy[1] + 930, y = origin_xy[2] + 560,
             label = "Caregiver / child 810 m", hjust = 0, size = 1.55,
             colour = pal$caregiver, fill = scales::alpha("white", 0.78), linewidth = 0.10) +
    annotate("label", x = origin_xy[1] + 780, y = origin_xy[2] + 360,
             label = "Older adult 675 m", hjust = 0, size = 1.55,
             colour = pal$older, fill = scales::alpha("white", 0.78), linewidth = 0.10) +
    add_scale_bar(c_xmin + 260, c_ymin + 280, 1000) +
    add_north_arrow(c_xmax - 315, c_ymax - 430) +
    add_custom_legend(c_xmin, c_xmax, c_ymin, c_ymax) +
    labs(
      title = "c  Role-sensitive local service environment",
      subtitle = "Daily facilities, transport nodes, and illustrative 15-minute walking-distance rings"
    ) +
    theme_case_map()

  function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "white", col = NA))
    pushViewport(viewport(layout = grid.layout(
      2, 2,
      widths = unit(c(0.92, 2.08), "null"),
      heights = unit(c(1, 1), "null")
    )))
    print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(p_c, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2))
    popViewport()
    grid.lines(unit(c(0.304, 0.36), "npc"), unit(c(0.66, 0.62), "npc"),
               gp = gpar(col = "#9E9E9E", lwd = 0.55, lty = 2))
    grid.lines(unit(c(0.304, 0.36), "npc"), unit(c(0.29, 0.39), "npc"),
               gp = gpar(col = "#9E9E9E", lwd = 0.55, lty = 2))
  }
}

save_case_figure <- function(draw_fun, stem, width_mm = 180, height_mm = 120, dpi = 600) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  grDevices::svg(file.path(out_dir, paste0(stem, ".svg")), width = w, height = h,
                 family = "Arial", pointsize = 6.4)
  draw_fun()
  dev.off()
  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = w, height = h,
                       family = "Arial", pointsize = 6.4)
  draw_fun()
  dev.off()
  ragg::agg_png(file.path(out_dir, paste0(stem, ".png")), width = w, height = h,
                units = "in", res = dpi, background = "white")
  draw_fun()
  dev.off()
}

figure <- make_case_context_figure()
save_case_figure(figure, "case_context_role_sensitive_accessibility")
message("Saved case context figure to: ", out_dir)
