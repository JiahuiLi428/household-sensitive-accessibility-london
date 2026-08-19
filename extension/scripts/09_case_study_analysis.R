options(java.parameters = "-Xmx10G")

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(ggplot2)
  library(scales)
  library(r5r)
})

set.seed(20260716)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, "UCLdissertation.Rproj")) ||
        (dir.exists(file.path(current, "data")) && dir.exists(file.path(current, "scripts")))
    ) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate project root from: ", start)
    current <- parent
  }
}

project_root <- find_project_root()
rel_path <- function(...) file.path(project_root, ...)
source(rel_path("scripts", "model", "00_config.R"))

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
table_dir <- file.path(extension_dir, "outputs", "tables")
case_dir <- file.path(extension_dir, "outputs", "case_studies")
map_dir <- file.path(extension_dir, "outputs", "maps")
intermediate_dir <- file.path(extension_dir, "data_intermediate")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("09_case_study_analysis_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("09_case_study_analysis.R started at: ", started_at)

required_file <- function(path) {
  if (!file.exists(path)) stop("Required input does not exist: ", path)
  invisible(path)
}

slugify <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

read_poi_sf <- function(file_name, category, crs_target) {
  path <- required_file(file.path(POI_DIR, file_name))
  dt <- fread(path)
  dt <- dt[!is.na(lon) & !is.na(lat)]
  if (!"name" %in% names(dt)) dt[, name := category]
  sf <- st_as_sf(dt[, .(name, lon, lat)], coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sf$facility_type <- category
  st_transform(sf, crs_target)
}

fixed_case_bbox <- function(case_lsoa, width_m = 9000, height_m = 5400, x_offset_m = 1500) {
  centre <- st_coordinates(st_centroid(st_union(st_geometry(case_lsoa))))[1, ]
  centre_x <- unname(centre["X"]) + x_offset_m
  centre_y <- unname(centre["Y"])
  half_width <- width_m / 2
  half_height <- height_m / 2
  st_bbox(
    c(
      xmin = centre_x - half_width,
      ymin = centre_y - half_height,
      xmax = centre_x + half_width,
      ymax = centre_y + half_height
    ),
    crs = st_crs(case_lsoa)
  )
}

format_degree_minutes <- function(x, positive_suffix, negative_suffix) {
  absolute <- abs(x)
  degrees <- floor(absolute)
  minutes <- round((absolute - degrees) * 60)
  rollover <- minutes == 60
  degrees[rollover] <- degrees[rollover] + 1
  minutes[rollover] <- 0
  suffix <- ifelse(x < 0, negative_suffix, positive_suffix)
  paste0(degrees, "°", sprintf("%02d", minutes), "′", suffix)
}

format_longitude <- function(x) format_degree_minutes(x, "E", "W")
format_latitude <- function(x) format_degree_minutes(x, "N", "S")

add_decorations <- function(p, bbox, scalebar_m = 1000) {
  x0 <- bbox["xmin"] + 0.06 * (bbox["xmax"] - bbox["xmin"])
  y0 <- bbox["ymin"] + 0.08 * (bbox["ymax"] - bbox["ymin"])
  nx <- bbox["xmin"] + 0.70 * (bbox["xmax"] - bbox["xmin"])
  ny0 <- bbox["ymin"] + 0.13 * (bbox["ymax"] - bbox["ymin"])
  ny1 <- bbox["ymin"] + 0.235 * (bbox["ymax"] - bbox["ymin"])
  nw <- 0.018 * (bbox["xmax"] - bbox["xmin"])
  p +
    annotate("segment", x = x0, xend = x0 + scalebar_m, y = y0, yend = y0, linewidth = 1.1, colour = "#202020") +
    annotate("text", x = x0 + scalebar_m / 2, y = y0 + 0.035 * (bbox["ymax"] - bbox["ymin"]), label = "1 km", size = 3) +
    annotate("polygon", x = c(nx, nx - nw, nx), y = c(ny1, ny0, ny0 + 0.025 * diff(bbox[c("ymin", "ymax")])),
             fill = "#111111", colour = "#111111", linewidth = 0.25) +
    annotate("polygon", x = c(nx, nx + nw, nx), y = c(ny1, ny0, ny0 + 0.025 * diff(bbox[c("ymin", "ymax")])),
             fill = "white", colour = "#111111", linewidth = 0.25) +
    annotate("text", x = nx, y = bbox["ymin"] + 0.265 * (bbox["ymax"] - bbox["ymin"]),
             label = "N", fontface = "bold", size = 3.2)
}

selection <- fread(required_file(file.path(table_dir, "final_case_selection.csv")))
mismatch <- fread(required_file(file.path(table_dir, "grid_informed_mismatch.csv")))
comparison <- fread(required_file(file.path(table_dir, "centroid_grid_comparison.csv")))
compound <- fread(required_file(file.path(table_dir, "compound_mismatch.csv")))
grid <- st_read(required_file(file.path(intermediate_dir, "london_grid_100m_residential.gpkg")), quiet = TRUE)
grid_access <- as.data.table(arrow::read_parquet(required_file(file.path(intermediate_dir, "grid_accessibility_summary.parquet"))))
long <- as.data.table(arrow::read_parquet(required_file(file.path(intermediate_dir, "grid_accessibility_long.parquet"))))
lsoas <- st_read(required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg")), quiet = TRUE)
thames <- st_transform(st_read(required_file(rel_path("data", "boundaries", "river_thames_osm.geojson")), quiet = TRUE), st_crs(lsoas))
residential_buildings <- st_read(
  required_file(file.path(intermediate_dir, "osm_residential_features.gpkg")),
  layer = "residential_buildings", quiet = TRUE
)
residential_landuse <- st_read(
  required_file(file.path(intermediate_dir, "osm_residential_features.gpkg")),
  layer = "residential_landuse", quiet = TRUE
)
osm_pbf <- required_file(rel_path("data", "raw", "osm", "greater-london-latest.osm.pbf"))

facility_pois <- rbindlist(lapply(names(facility_files), function(category) {
  as.data.table(st_drop_geometry(read_poi_sf(facility_files[[category]], category, st_crs(lsoas))))[, facility_type := category]
}), fill = TRUE)
facility_sf <- st_as_sf(facility_pois, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
facility_sf <- st_transform(facility_sf, st_crs(lsoas))

r5_network <- setup_r5(data_path = R5_NETWORK_DIR, verbose = FALSE)
on.exit(try(stop_r5(r5_network), silent = TRUE), add = TRUE)

facility_palette <- c(
  "Schools" = "#C7864B",
  "GPs" = "#B5534B",
  "Hospitals" = "#7A3E65",
  "Pharmacies" = "#9568A3",
  "Supermarkets" = "#C99A32",
  "Parks" = "#5F8A65",
  "Underground/metro" = "#305F72",
  "Bus stops" = "#79A6BD"
)
facility_shapes <- c(
  "Schools" = 24,
  "GPs" = 23,
  "Hospitals" = 8,
  "Pharmacies" = 18,
  "Supermarkets" = 22,
  "Parks" = 21,
  "Underground/metro" = 17,
  "Bus stops" = 16
)
facility_sizes <- c(
  "Schools" = 1.7,
  "GPs" = 1.7,
  "Hospitals" = 1.8,
  "Pharmacies" = 1.6,
  "Supermarkets" = 1.7,
  "Parks" = 1.6,
  "Underground/metro" = 1.9,
  "Bus stops" = 0.65
)
facility_order <- names(facility_palette)
isochrone_linetypes <- c("5 min" = "solid", "10 min" = "22", "15 min" = "42")

make_case_legend <- function() {
  gradient_steps <- data.frame(
    ymin = seq(80, 92.6, length.out = 60),
    ymax = seq(80, 92.6, length.out = 60) + 12.6 / 59,
    value = seq(0, 65, length.out = 60)
  )
  walk_legend <- data.frame(
    y = c(68, 63, 58),
    label = c("5 min", "10 min", "15 min"),
    linetype = factor(c("5 min", "10 min", "15 min"), levels = names(isochrone_linetypes))
  )
  facility_legend <- data.frame(
    facility_label = factor(facility_order, levels = facility_order),
    x = 0.10,
    label_x = 0.21,
    y = seq(51, 19.5, length.out = length(facility_order))
  )

  ggplot() +
    geom_rect(
      data = gradient_steps,
      aes(xmin = 0.10, xmax = 0.18, ymin = ymin, ymax = ymax, fill = value),
      colour = NA
    ) +
    geom_segment(
      data = data.frame(value = c(0, 15, 30, 45, 60)),
      aes(
        x = 0.10,
        xend = 0.19,
        y = 80 + value / 65 * 12.8,
        yend = 80 + value / 65 * 12.8
      ),
      colour = "white",
      linewidth = 0.28
    ) +
    geom_text(
      data = data.frame(value = c(0, 15, 30, 45, 60)),
      aes(x = 0.23, y = 80 + value / 65 * 12.8, label = value),
      hjust = 0,
      size = 2.75,
      colour = "#2A2A2A"
    ) +
    geom_segment(
      data = transform(walk_legend, y = c(70, 66, 62)),
      aes(x = 0.10, xend = 0.23, y = y, yend = y, linetype = linetype),
      colour = "#315F78",
      linewidth = 0.70
    ) +
    geom_text(
      data = transform(walk_legend, y = c(70, 66, 62)),
      aes(x = 0.28, y = y, label = label),
      hjust = 0,
      size = 2.75,
      colour = "#2A2A2A"
    ) +
    geom_point(
      data = facility_legend,
      aes(x = x, y = y, shape = facility_label, colour = facility_label, size = facility_label),
      fill = "white",
      stroke = 0.45
    ) +
    geom_text(
      data = facility_legend,
      aes(x = label_x, y = y, label = facility_label),
      hjust = 0,
      size = 2.55,
      colour = "#2A2A2A"
    ) +
    annotate("rect", xmin = 0.10, xmax = 0.17, ymin = 7.5, ymax = 10.5,
             fill = NA, colour = "#111111", linewidth = 0.70) +
    annotate("rect", xmin = 0.10, xmax = 0.17, ymin = 2.5, ymax = 5.5,
             fill = NA, colour = "#7C5268", linewidth = 0.58) +
    annotate("text", x = 0.21, y = 9, label = "Case LSOA boundary",
             hjust = 0, size = 2.55, colour = "#2A2A2A") +
    annotate("text", x = 0.21, y = 4, label = "Lowest 10% residential cells",
             hjust = 0, size = 2.42, colour = "#2A2A2A") +
    annotate("text", x = 0.08, y = 97, label = "Accessibility score",
             hjust = 0, fontface = "bold", size = 3.1) +
    annotate("text", x = 0.08, y = 76, label = "Walking isochrones",
             hjust = 0, fontface = "bold", size = 3.1) +
    annotate("text", x = 0.08, y = 56, label = "Facilities",
             hjust = 0, fontface = "bold", size = 3.1) +
    annotate("text", x = 0.08, y = 14, label = "Analytical features",
             hjust = 0, fontface = "bold", size = 3.1) +
    scale_fill_gradientn(
      colours = c("#EEF3F4", "#C8DDE2", "#9FC2CE", "#769FB5", "#4F7894"),
      limits = c(0, 65),
      guide = "none"
    ) +
    scale_linetype_manual(values = isochrone_linetypes, guide = "none") +
    scale_shape_manual(values = facility_shapes, limits = facility_order, guide = "none") +
    scale_colour_manual(values = facility_palette, limits = facility_order, guide = "none") +
    scale_size_manual(
      values = setNames(pmax(unname(facility_sizes), 1.7), names(facility_sizes)),
      limits = facility_order,
      guide = "none"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 100), expand = FALSE, clip = "off") +
    theme_void(base_family = "Arial") +
    theme(
      panel.border = element_rect(fill = NA, colour = "#B8B5AE", linewidth = 0.32),
      plot.background = element_rect(fill = "#FAFAF8", colour = NA),
      plot.margin = margin(3, 4, 3, 4)
    )
}

compose_case_figure <- function(map_plot, legend_plot, title, subtitle) {
  layout <- gtable::gtable(
    widths = grid::unit(c(4.75, 1.55), "null"),
    heights = grid::unit.c(
      grid::unit(0.34, "in"),
      grid::unit(0.23, "in"),
      grid::unit(1, "null")
    )
  )
  layout <- gtable::gtable_add_grob(
    layout,
    grid::textGrob(
      title,
      x = grid::unit(0.012, "npc"),
      hjust = 0,
      gp = grid::gpar(fontfamily = "Arial", fontsize = 12.5, fontface = "bold", col = "#1F1F1F")
    ),
    t = 1, l = 1, r = 2
  )
  layout <- gtable::gtable_add_grob(
    layout,
    grid::textGrob(
      subtitle,
      x = grid::unit(0.012, "npc"),
      hjust = 0,
      gp = grid::gpar(fontfamily = "Arial", fontsize = 8.8, col = "#555555")
    ),
    t = 2, l = 1, r = 2
  )
  layout <- gtable::gtable_add_grob(layout, ggplotGrob(map_plot), t = 3, l = 1)
  layout <- gtable::gtable_add_grob(layout, ggplotGrob(legend_plot), t = 3, l = 2)
  layout <- gtable::gtable_add_grob(
    layout,
    grid::rectGrob(
      gp = grid::gpar(fill = NA, col = "#222222", lwd = 0.70)
    ),
    t = 3, l = 1, r = 2, z = Inf, clip = "off"
  )
  layout <- gtable::gtable_add_grob(
    layout,
    grid::segmentsGrob(
      x0 = grid::unit(4.75 / (4.75 + 1.55), "npc"),
      x1 = grid::unit(4.75 / (4.75 + 1.55), "npc"),
      y0 = grid::unit(0, "npc"),
      y1 = grid::unit(1, "npc"),
      gp = grid::gpar(col = "#D1CEC7", lwd = 0.55)
    ),
    t = 3, l = 1, r = 2, z = Inf, clip = "off"
  )
  layout
}

all_profiles <- list()

for (row_i in seq_len(nrow(selection))) {
  sel <- selection[row_i]
  case_name <- sel$candidate_case
  case_slug <- slugify(case_name)
  lsoa_id <- sel$LSOA21CD
  display_title <- fcase(
    grepl("^Case 1", case_name), "Case 1: Barking Riverside / Thames View",
    grepl("^Case 2", case_name), "Case 2: Compound mismatch — Hillingdon 003D",
    grepl("^Case 3", case_name), "Case 3: Positive counter-case — Barking and Dagenham 015F",
    default = case_name
  )
  message("Building case outputs: ", case_name, " / ", lsoa_id)

  case_lsoa <- lsoas[lsoas$LSOA21CD == lsoa_id, ]
  bbox <- fixed_case_bbox(case_lsoa)
  bbox_poly <- st_as_sfc(bbox)
  local_lsoas <- lsoas[lengths(st_intersects(lsoas, bbox_poly)) > 0, ]
  local_grid <- grid[grid$lsoa21cd == lsoa_id, ]
  local_scores <- grid_access[group == "caregiving_adults" & mode == "walk_15min" & lsoa21cd == lsoa_id]
  local_grid <- merge(local_grid, local_scores[, .(grid_id, basket_score)], by = "grid_id", all.x = TRUE)
  score_cut <- quantile(local_grid$basket_score, 0.10, na.rm = TRUE)
  local_grid$lowest_decile_grid <- local_grid$basket_score <= score_cut

  fac_stats <- long[group == "caregiving_adults" & mode == "walk_15min" & lsoa21cd == lsoa_id,
                    .(mean_score = mean(facility_score, na.rm = TRUE),
                      mean_time = mean(travel_time, na.rm = TRUE)),
                    by = facility_type][order(mean_score)]
  weakest <- fac_stats[1:min(.N, 3)]
  strongest <- fac_stats[order(-mean_score)][1:min(.N, 3)]
  local_facilities <- facility_sf[lengths(st_intersects(facility_sf, bbox_poly)) > 0, ]
  local_facilities$facility_label <- facility_labels[local_facilities$facility_type]
  local_facilities$facility_label <- factor(local_facilities$facility_label, levels = facility_order)
  # Add clipped, off-map points so all eight categories retain a visible legend
  # symbol even when a category has no facility inside a particular case frame.
  legend_facilities <- st_sf(
    facility_label = factor(facility_order, levels = facility_order),
    geometry = st_sfc(
      lapply(seq_along(facility_order), function(i) {
        st_point(c(unname(bbox["xmax"]) + 10000 + i, unname(bbox["ymax"]) + 10000))
      }),
      crs = st_crs(lsoas)
    )
  )
  plot_facilities <- rbind(
    local_facilities[, "facility_label", drop = FALSE],
    legend_facilities
  )

  origin_point <- st_point_on_surface(case_lsoa)
  origin_wgs84 <- st_transform(origin_point, 4326)
  origin_xy <- st_coordinates(origin_wgs84)
  isochrones <- isochrone(
    r5_network,
    origins = data.frame(
      id = lsoa_id,
      lon = origin_xy[1, "X"],
      lat = origin_xy[1, "Y"]
    ),
    mode = "walk",
    cutoffs = c(5, 10, 15),
    departure_datetime = r5_departure_datetime,
    polygon_output = TRUE,
    time_window = r5_time_window_min,
    max_walk_time = 15,
    max_trip_duration = 15,
    walk_speed = r5_walk_speed_kmh,
    percentiles = r5_percentile,
    progress = FALSE,
    verbose = FALSE
  )
  isochrones <- st_transform(isochrones, st_crs(lsoas))
  isochrones$walk_time <- factor(
    paste0(isochrones$isochrone, " min"),
    levels = c("5 min", "10 min", "15 min")
  )
  catchment_15 <- st_union(isochrones[isochrones$walk_time == "15 min", ])
  service_facilities <- local_facilities[
    !local_facilities$facility_label %in% c("Underground/metro", "Bus stops"),
  ]
  n_services_within_15 <- sum(
    lengths(st_intersects(service_facilities, catchment_15)) > 0
  )
  case_2_annotation <- if (n_services_within_15 == 0) {
    "No mapped key-service facilities\nwithin the 15-minute walk catchment"
  } else {
    paste0(
      "Only ", n_services_within_15,
      " mapped key-service facilit",
      ifelse(n_services_within_15 == 1, "y", "ies"),
      "\nwithin the 15-minute walk catchment"
    )
  }

  case_mismatch <- mismatch[LSOA21CD == lsoa_id & group == "caregiving_adults"]
  case_comp <- compound[LSOA21CD == lsoa_id]
  case_compare <- comparison[lsoa21cd == lsoa_id & group == "caregiving_adults" & mode == "walk_15min"]

  profile <- data.table(
    case_name = case_name,
    case_type = fifelse(grepl("Case 1", case_name), "Barking Riverside / Thames View",
                        fifelse(grepl("Case 2", case_name), "Compound mismatch case", "Positive counter-case")),
    lsoa21cd = lsoa_id,
    lsoa_name = sel$LSOA21NM,
    borough = sel$borough_name,
    care_need = sel$care_need_score,
    child_need = sel$child_need,
    older_need = sel$older_need,
    centroid_access = sel$caregiving_centroid_access,
    grid_mean_access = sel$caregiving_grid_access,
    lowest_decile_access = sel$caregiving_lowest_decile_access,
    within_lsoa_variation = sel$within_lsoa_variation,
    mismatch_score = sel$caregiving_grid_mean_mismatch,
    lisa_class = sel$caregiving_lisa,
    compound_type = sel$compound_type,
    compound_count = sel$compound_count,
    strongest_facility_categories = paste(strongest$facility_type, collapse = "; "),
    weakest_facility_categories = paste(weakest$facility_type, collapse = "; "),
    main_service_deficit = weakest$facility_type[1],
    policy_implication = fifelse(sel$compound_count >= 2,
                                 "Prioritise multi-service neighbourhood repair and public-transport-linked care trips.",
                                 "Protect comparatively strong access while targeting remaining internal low-access pockets.")
  )
  all_profiles[[case_slug]] <- profile
  fwrite(profile, file.path(case_dir, paste0(case_slug, "_profile.csv")))

  local_landuse <- residential_landuse[lengths(st_intersects(residential_landuse, bbox_poly)) > 0, ]
  bbox_wgs84 <- st_transform(bbox_poly, 4326)
  local_buildings <- suppressWarnings(st_read(
    osm_pbf,
    query = "SELECT osm_id, building FROM multipolygons WHERE building IS NOT NULL",
    wkt_filter = st_as_text(bbox_wgs84),
    quiet = TRUE
  ))
  if (nrow(local_buildings) > 0) {
    local_buildings <- st_transform(st_make_valid(local_buildings), st_crs(lsoas))
  } else {
    local_buildings <- residential_buildings[
      lengths(st_intersects(residential_buildings, bbox_poly)) > 0,
    ]
  }
  local_roads <- suppressWarnings(st_read(
    osm_pbf, layer = "lines", wkt_filter = st_as_text(bbox_wgs84),
    options = "INTERLEAVED_READING=YES", quiet = TRUE
  ))
  local_roads <- local_roads[!is.na(local_roads$highway), ]
  local_roads <- st_transform(local_roads, st_crs(lsoas))
  local_roads$road_group <- fcase(
    local_roads$highway %in% c("motorway", "trunk", "primary", "secondary"), "Major road",
    local_roads$highway %in% c("tertiary", "unclassified"), "Connector road",
    local_roads$highway %in% c("residential", "living_street", "service"), "Local street",
    default = "Other"
  )
  local_roads <- local_roads[local_roads$road_group != "Other", ]

  base_theme <- theme_void(base_size = 9, base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, colour = "#1F1F1F", margin = margin(b = 2)),
      plot.subtitle = element_text(size = 8.8, colour = "#555555", margin = margin(b = 7)),
      legend.position = "none",
      legend.box = "vertical",
      legend.box.just = "top",
      legend.direction = "vertical",
      panel.border = element_rect(fill = NA, colour = "#222222", linewidth = 0.45),
      legend.background = element_blank(),
      legend.box.background = element_rect(fill = "#FAFAF8", colour = "#C8C6C0", linewidth = 0.30),
      legend.key.height = unit(0.38, "cm"),
      legend.key.width = unit(0.46, "cm"),
      legend.title = element_text(size = 7.8, face = "bold"),
      legend.text = element_text(size = 7.1),
      legend.spacing.x = unit(0.16, "cm"),
      legend.spacing.y = unit(0.12, "cm"),
      legend.box.margin = margin(4, 6, 4, 6),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(8, 10, 8, 8)
    )

  subtitle_text <- paste0(
    sel$LSOA21NM,
    " | caregiving accessibility score 0–65; WALK ≤15 min; common 9.0 × 5.4 km frame; EPSG:27700"
  )

  p <- ggplot() +
    geom_sf(data = local_lsoas, fill = "#F8F7F3", colour = "#D4D1C9", linewidth = 0.16) +
    geom_sf(data = local_landuse, fill = "#E8EEE3", colour = NA, alpha = 0.42) +
    geom_sf(data = local_buildings, fill = "#D5D9DA", colour = NA, linewidth = 0, alpha = 0.56) +
    geom_sf(data = local_roads[local_roads$road_group == "Local street", ],
            colour = "#C5C3BD", linewidth = 0.11, alpha = 0.62) +
    geom_sf(data = local_roads[local_roads$road_group == "Connector road", ],
            colour = "#AAA9A4", linewidth = 0.22, alpha = 0.72) +
    geom_sf(data = local_roads[local_roads$road_group == "Major road", ],
            colour = "#878B89", linewidth = 0.38, alpha = 0.78) +
    geom_sf(data = thames[lengths(st_intersects(thames, st_as_sfc(bbox))) > 0, ], colour = "#9FC3D2", linewidth = 0.72) +
    geom_sf(data = local_grid, aes(fill = basket_score), colour = NA, alpha = 0.90) +
    geom_sf(
      data = local_grid[local_grid$lowest_decile_grid %in% TRUE, ],
      aes(alpha = "Lowest 10% residential cells"),
      fill = NA,
      colour = "#7C5268",
      linewidth = 0.72
    ) +
    geom_sf(
      data = isochrones,
      aes(linetype = walk_time),
      fill = NA,
      colour = "#315F78",
      linewidth = 0.68,
      alpha = 0.95
    ) +
    geom_sf(
      data = case_lsoa,
      aes(alpha = "Case LSOA boundary"),
      fill = NA,
      colour = "#111111",
      linewidth = 0.92
    ) +
    geom_sf(
      data = plot_facilities,
      aes(shape = facility_label, colour = facility_label, size = facility_label),
      fill = "white",
      stroke = 0.42,
      alpha = 0.92
    ) +
    geom_sf(data = origin_point, shape = 21, fill = "white", colour = "white", size = 3.5, stroke = 0) +
    geom_sf(data = origin_point, shape = 4, colour = "#111111", size = 2.2, stroke = 0.82) +
    scale_fill_gradientn(
      colours = c("#EEF3F4", "#C8DDE2", "#9FC2CE", "#769FB5", "#4F7894"),
      limits = c(0, 65),
      breaks = c(0, 15, 30, 45, 60),
      oob = scales::squish,
      name = "Residential-grid\naccess score",
      na.value = "#E6E6E2"
    ) +
    scale_linetype_manual(
      name = "Network walking\nisochrones",
      values = isochrone_linetypes,
      drop = FALSE
    ) +
    scale_shape_manual(
      name = "Eight facility categories",
      values = facility_shapes,
      limits = facility_order,
      drop = FALSE
    ) +
    scale_colour_manual(
      name = "Eight facility categories",
      values = facility_palette,
      limits = facility_order,
      drop = FALSE
    ) +
    scale_size_manual(
      name = "Eight facility categories",
      values = facility_sizes,
      limits = facility_order,
      drop = FALSE
    ) +
    scale_alpha_manual(
      name = "Analytical features",
      values = c(
        "Case LSOA boundary" = 1,
        "Lowest 10% residential cells" = 1
      ),
      breaks = c("Case LSOA boundary", "Lowest 10% residential cells")
    ) +
    guides(
      fill = guide_colourbar(
        title = "Accessibility score",
        order = 1,
        direction = "vertical",
        title.position = "top",
        barwidth = unit(0.34, "cm"),
        barheight = unit(2.4, "cm")
      ),
      linetype = guide_legend(
        title = "Walking isochrones",
        order = 2,
        ncol = 1,
        byrow = TRUE,
        title.position = "top"
      ),
      shape = guide_legend(
        title = "Facilities",
        order = 3,
        ncol = 2,
        byrow = TRUE,
        title.position = "top",
        override.aes = list(
          colour = unname(facility_palette),
          size = unname(facility_sizes),
          fill = "white",
          alpha = 1
        )
      ),
      alpha = guide_legend(
        title = "Analytical features",
        order = 4,
        ncol = 1,
        title.position = "top",
        override.aes = list(
          colour = c("#111111", "#7C5268"),
          fill = NA,
          linewidth = c(0.9, 0.65)
        )
      ),
      colour = "none",
      size = "none"
    ) +
    scale_x_continuous(labels = format_longitude) +
    scale_y_continuous(labels = format_latitude) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      datum = st_crs(4326),
      label_axes = list(
        bottom = "E",
        top = "E",
        left = "N",
        right = "N"
      ),
      expand = FALSE
    ) +
    labs(
      title = display_title,
      subtitle = subtitle_text
    ) +
    base_theme
  if (grepl("^Case 2", case_name)) {
    callout_x <- unname(bbox["xmin"]) + 0.61 * (unname(bbox["xmax"]) - unname(bbox["xmin"]))
    callout_y <- unname(bbox["ymin"]) + 0.69 * (unname(bbox["ymax"]) - unname(bbox["ymin"]))
    callout_point <- st_sfc(
      st_point(c(callout_x, callout_y)),
      crs = st_crs(lsoas)
    )
    callout_connector <- st_sf(
      geometry = st_nearest_points(callout_point, st_boundary(catchment_15))
    )
    connector_xy <- st_coordinates(callout_connector$geometry[[1]])
    p <- p +
      annotate(
        "segment",
        x = connector_xy[1, "X"],
        y = connector_xy[1, "Y"],
        xend = connector_xy[nrow(connector_xy), "X"],
        yend = connector_xy[nrow(connector_xy), "Y"],
        colour = "#6F6A63",
        linewidth = 0.34,
        lineend = "round"
      ) +
      annotate(
      "label",
      x = callout_x,
      y = callout_y,
      label = case_2_annotation,
      hjust = 0.5,
      size = 2.7,
      lineheight = 1.05,
      colour = "#4A4641",
      fill = scales::alpha("#FFFDF7", 0.94),
      label.size = 0.22
    )
    message("Case 2 annotation: ", gsub("\n", " ", case_2_annotation))
  }
  p <- add_decorations(p, bbox)
  p <- p + theme(
    panel.border = element_rect(fill = NA, colour = "#222222", linewidth = 0.48),
    panel.grid.major = element_line(colour = "#D8D6D0", linewidth = 0.20, linetype = "dotted"),
    axis.title = element_blank(),
    axis.text = element_text(colour = "#383838", size = 7.2),
    axis.text.x.top = element_text(margin = margin(b = 2)),
    axis.text.x.bottom = element_text(margin = margin(t = 2)),
    axis.text.y.left = element_text(angle = 90, margin = margin(r = 2)),
    axis.text.y.right = element_text(angle = 90, margin = margin(l = 2)),
    axis.ticks = element_line(colour = "#333333", linewidth = 0.30),
    axis.ticks.length = unit(2.2, "pt"),
    plot.margin = margin(4, 7, 4, 5)
  )
  metrics_device <- tempfile(fileext = ".png")
  ragg::agg_png(metrics_device, width = 9.5, height = 6.3, units = "in", res = 96)
  legend_grob <- ggplotGrob(make_case_legend())
  grDevices::dev.off()
  unlink(metrics_device)
  p <- p + annotation_custom(
    grob = legend_grob,
    xmin = unname(bbox["xmin"]) + 0.733 * (unname(bbox["xmax"]) - unname(bbox["xmin"])),
    xmax = unname(bbox["xmin"]) + 0.973 * (unname(bbox["xmax"]) - unname(bbox["xmin"])),
    ymin = unname(bbox["ymin"]) + 0.035 * (unname(bbox["ymax"]) - unname(bbox["ymin"])),
    ymax = unname(bbox["ymin"]) + 0.965 * (unname(bbox["ymax"]) - unname(bbox["ymin"]))
  )
  output_stem <- file.path(case_dir, paste0(case_slug, "_map"))
  ggsave(paste0(output_stem, ".png"), p, width = 10.8, height = 7.3, dpi = 450, bg = "white")
  ggsave(paste0(output_stem, ".pdf"), p, width = 10.8, height = 7.3, device = cairo_pdf, bg = "white")
  ggsave(paste0(output_stem, ".svg"), p, width = 10.8, height = 7.3, device = svglite::svglite, bg = "white")
  ggsave(paste0(output_stem, ".tiff"), p, width = 10.8, height = 7.3, dpi = 600, device = ragg::agg_tiff, bg = "white", compression = "lzw")

  summary_lines <- c(
    paste0("# ", case_name),
    "",
    paste0("LSOA: ", sel$LSOA21CD, " (", sel$LSOA21NM, "), ", sel$borough_name, "."),
    "",
    "1. Locator map: see the accompanying case map PNG/PDF; the selected LSOA is outlined in black.",
    "2. 100m grid accessibility: cells are shaded by caregiving-adult walk-only basket score.",
    "3. Lowest-accessibility grid: the lowest 10% of residential grid cells are outlined in purple.",
    paste0("4. Main facilities: strongest categories are ", profile$strongest_facility_categories, "."),
    paste0("5. Public transport nodes: bus stops and metro/rail points within the local buffer are shown for reference."),
    "6. LSOA boundary: the selected LSOA is shown against neighbouring LSOAs.",
    paste0("7. Care need: caregiving need score = ", round(profile$care_need, 2),
           "; child need = ", round(profile$child_need, 2),
           "; older need = ", round(profile$older_need, 2), "."),
    paste0("8. Mismatch type: ", profile$lisa_class, "; compound type = ", profile$compound_type, "."),
    paste0("9. Strongest facility categories: ", profile$strongest_facility_categories, "."),
    paste0("10. Weakest facility categories: ", profile$weakest_facility_categories, "."),
    paste0("11. Centroid-grid difference: centroid = ", round(profile$centroid_access, 2),
           ", grid mean = ", round(profile$grid_mean_access, 2),
           ", lowest decile = ", round(profile$lowest_decile_access, 2),
           ", within-LSOA SD = ", round(profile$within_lsoa_variation, 2), "."),
    paste0("12. Spatial mechanism: the grid result shows how within-LSOA residential location and service placement change the accessibility signal relative to the single centroid origin."),
    paste0("13. Policy implication: ", profile$policy_implication)
  )
  writeLines(summary_lines, file.path(case_dir, paste0(case_slug, "_summary.md")), useBytes = TRUE)
}

fwrite(rbindlist(all_profiles, use.names = TRUE, fill = TRUE), file.path(case_dir, "case_study_profiles_combined.csv"))

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
