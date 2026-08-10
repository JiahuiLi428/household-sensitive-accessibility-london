# Shared configuration and helpers for the model pipeline.
# Reads: project structure only. Writes: creates standard output directories.
# Required by: scripts 01-11 and run_pipeline.R.

library(data.table)

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
    if (identical(parent, current)) {
      stop("Could not locate project root from: ", start)
    }
    current <- parent
  }
}

PROJECT_ROOT <- find_project_root()
project_path <- function(...) file.path(PROJECT_ROOT, ...)

DATA_DIR <- project_path("data")
ANALYSIS_DIR <- project_path("data", "output", "analysis")
R5_OUTPUT_DIR <- project_path("data", "output", "r5r")
R5_NETWORK_DIR <- project_path("data", "r5", "london_network")
POI_DIR <- project_path("data", "poi")
PUBLICATION_FIGURE_DIR <- project_path("figures", "publication")
MEETING_FIGURE_DIR <- project_path("figures", "meeting")

dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PUBLICATION_FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

model_facilities <- data.table(
  facility_category = c(
    "education_schools",
    "healthcare_gps",
    "healthcare_hospitals",
    "healthcare_pharmacies",
    "food_retail_supermarkets",
    "green_space_parks",
    "transport_underground_metro_stations",
    "transport_bus_stops"
  ),
  score_name = c(
    "school",
    "gp",
    "hospital",
    "pharmacy",
    "supermarket",
    "park",
    "underground",
    "bus_stop"
  ),
  label = c(
    "Schools",
    "GPs",
    "Hospitals",
    "Pharmacies",
    "Supermarkets",
    "Parks",
    "Underground/metro",
    "Bus stops"
  ),
  poi_csv = c(
    "education_schools_points.csv",
    "healthcare_gps_points.csv",
    "healthcare_hospitals_points.csv",
    "healthcare_pharmacies_points.csv",
    "food_retail_supermarkets_points.csv",
    "green_space_parks_points.csv",
    "transport_underground_metro_stations_points.csv",
    "transport_bus_stops_points.csv"
  )
)

facility_files <- setNames(model_facilities$poi_csv, model_facilities$facility_category)
facility_score_map <- setNames(model_facilities$score_name, model_facilities$facility_category)
facility_labels <- setNames(model_facilities$label, model_facilities$facility_category)

scenario_definitions <- list(
  walk_15min = list(
    label = "Walk 15",
    score_suffix = "walk",
    threshold_min = 15L,
    mode = c("WALK"),
    max_trip_duration = 15L,
    max_walk_time = 15L
  ),
  walk_transit_30min = list(
    label = "Walk + Transit 30",
    score_suffix = "transit",
    threshold_min = 30L,
    mode = c("WALK", "TRANSIT"),
    max_trip_duration = 30L,
    max_walk_time = 15L
  )
)

# Unreached destinations in the walk-15 scenario are represented one minute
# beyond the policy threshold in the Appendix C GP travel-time regression.
GP_UNREACHED_IMPUTED_MIN <- scenario_definitions$walk_15min$threshold_min + 1L

r5_departure_datetime <- as.POSIXct("2026-06-02 08:30:00", tz = "Europe/London")
r5_time_window_min <- 10L
r5_percentile <- 50L
r5_walk_speed_kmh <- 3.6
r5_max_rides <- 3L

service_basket_weights <- list(
  family = c(
    school = 0.30,
    supermarket = 0.08,
    park = 0.20,
    gp = 0.14,
    bus_stop = 0.10,
    pharmacy = 0.10,
    underground = 0.04,
    hospital = 0.04
  ),
  working = c(
    bus_stop = 0.18,
    underground = 0.16,
    supermarket = 0.15,
    park = 0.08,
    gp = 0.10,
    pharmacy = 0.12,
    school = 0.15,
    hospital = 0.06
  ),
  older = c(
    gp = 0.24,
    pharmacy = 0.22,
    park = 0.16,
    supermarket = 0.10,
    bus_stop = 0.14,
    underground = 0.06,
    hospital = 0.05,
    school = 0.03
  )
)
caregiving_weights <- service_basket_weights$working

care_need_weights <- c(
  caregiver_household_intensity = 0.50,
  lone_parent_intensity = 0.30,
  children_pct = 0.20
)

need_score_weights <- list(
  older = c(older_adults_pct = 0.65, older_single_intensity = 0.35),
  family = c(children_pct = 0.65, child_household_intensity = 0.35)
)

assert_weights <- function(weights, name = "weights") {
  total <- sum(weights)
  if (!isTRUE(all.equal(total, 1, tolerance = 1e-8))) {
    stop(name, " must sum to 1. Current sum: ", total)
  }
  invisible(TRUE)
}

invisible(lapply(names(service_basket_weights), function(nm) {
  assert_weights(service_basket_weights[[nm]], paste0("service_basket_weights$", nm))
}))
assert_weights(care_need_weights, "care_need_weights")

score_time <- function(x, threshold) {
  score <- 100 * (1 - pmin(x, threshold) / threshold)
  score[is.na(x)] <- 0
  pmax(score, 0)
}

rescale_0_100 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(NA_real_, length(x)))
  }
  100 * (x - rng[1]) / (rng[2] - rng[1])
}

zscore <- function(x) {
  sd_x <- sd(x, na.rm = TRUE)
  if (!is.finite(sd_x) || sd_x == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sd_x
}

weighted_index <- function(dt, weights, scenario = "walk") {
  cols <- paste0(names(weights), "_", scenario, "_score")
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing score columns: ", paste(missing_cols, collapse = ", "))
  }
  as.numeric(as.matrix(dt[, ..cols]) %*% weights)
}

make_borough_layer <- function(lsoas) {
  required <- c("LSOA21NM", attr(lsoas, "sf_column"))
  missing_cols <- setdiff(required, names(lsoas))
  if (length(missing_cols) > 0) {
    stop("Cannot build borough layer; missing columns: ", paste(missing_cols, collapse = ", "))
  }

  lsoas$borough_name <- sub(" [0-9]{3}[A-Z]$", "", lsoas$LSOA21NM)
  borough_geoms <- aggregate(
    sf::st_geometry(lsoas),
    by = list(borough_name = lsoas$borough_name),
    FUN = sf::st_union
  )
  sf::st_make_valid(sf::st_sf(
    borough_name = borough_geoms$borough_name,
    geometry = sf::st_sfc(borough_geoms$x, crs = sf::st_crs(lsoas))
  ))
}

LSOA_MAP_THEME <- ggplot2::theme_void(base_size = 10.5) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.title = ggplot2::element_text(face = "bold", size = 13, colour = "#1f1f1f"),
    plot.subtitle = ggplot2::element_text(
      size = 9.5,
      colour = "#4d4d4d",
      margin = ggplot2::margin(t = 2, b = 8)
    ),
    plot.caption = ggplot2::element_text(size = 8, colour = "#666666", hjust = 0),
    legend.position = c(0.86, 0.22),
    legend.background = ggplot2::element_rect(fill = "white", colour = "#D7D3CA", linewidth = 0.25),
    legend.title = ggplot2::element_text(size = 8.5, colour = "#333333"),
    legend.text = ggplot2::element_text(size = 8, colour = "#333333"),
    plot.margin = ggplot2::margin(10, 10, 8, 10)
  )

add_facility_scores <- function(dt) {
  for (scenario_name in names(scenario_definitions)) {
    scenario <- scenario_definitions[[scenario_name]]
    for (facility in names(facility_score_map)) {
      source_col <- paste(facility, scenario_name, sep = "_")
      score_col <- paste(facility_score_map[[facility]], scenario$score_suffix, "score", sep = "_")
      if (!source_col %in% names(dt)) {
        stop("Missing travel-time column: ", source_col)
      }
      dt[, (score_col) := score_time(get(source_col), scenario$threshold_min)]
    }
  }
  invisible(dt)
}

add_service_basket_indices <- function(dt) {
  add_facility_scores(dt)
  dt[, family_walk_index := weighted_index(dt, service_basket_weights$family, "walk")]
  dt[, working_walk_index := weighted_index(dt, service_basket_weights$working, "walk")]
  dt[, older_walk_index := weighted_index(dt, service_basket_weights$older, "walk")]
  dt[, family_transit_index := weighted_index(dt, service_basket_weights$family, "transit")]
  dt[, working_transit_index := weighted_index(dt, service_basket_weights$working, "transit")]
  dt[, older_transit_index := weighted_index(dt, service_basket_weights$older, "transit")]
  dt[, children_walk_index := family_walk_index]
  dt[, children_transit_index := family_transit_index]
  invisible(dt)
}

add_need_scores <- function(dt) {
  dt[, older_single_intensity := fifelse(
    household_total > 0,
    household_single_person_aged_66_over / household_total * 100,
    NA_real_
  )]
  dt[, child_household_intensity := fifelse(
    household_total > 0,
    household_dependent_children_total / household_total * 100,
    NA_real_
  )]
  dt[, older_need_score :=
    need_score_weights$older[["older_adults_pct"]] * rescale_0_100(older_adults_pct) +
      need_score_weights$older[["older_single_intensity"]] * rescale_0_100(older_single_intensity)
  ]
  dt[, family_need_score :=
    need_score_weights$family[["children_pct"]] * rescale_0_100(children_pct) +
      need_score_weights$family[["child_household_intensity"]] * rescale_0_100(child_household_intensity)
  ]
  dt[, working_age_pct := pmax(0, 100 - children_pct - older_adults_pct)]
  dt[, working_need_score := rescale_0_100(working_age_pct)]
  dt[, children_need_score := family_need_score]
  invisible(dt)
}

add_care_mobility_scores <- function(dt) {
  add_facility_scores(dt)
  if ("household_dependent_children_total" %in% names(dt)) {
    dt[, caregiver_households_count := household_dependent_children_total]
  } else {
    dt[, caregiver_households_count :=
      household_couple_with_dependent_children + household_lone_parent_with_dependent_children
    ]
  }
  dt[, caregiver_household_intensity := fifelse(
    household_total > 0,
    caregiver_households_count / household_total * 100,
    NA_real_
  )]
  dt[, lone_parent_intensity := fifelse(
    household_total > 0,
    household_lone_parent_with_dependent_children / household_total * 100,
    NA_real_
  )]
  dt[, care_walk_index := weighted_index(dt, caregiving_weights, "walk")]
  dt[, care_transit_index := weighted_index(dt, caregiving_weights, "transit")]
  dt[, care_transit_gain := care_transit_index - care_walk_index]
  dt[, care_need_score :=
    care_need_weights[["caregiver_household_intensity"]] * rescale_0_100(caregiver_household_intensity) +
      care_need_weights[["lone_parent_intensity"]] * rescale_0_100(lone_parent_intensity) +
      care_need_weights[["children_pct"]] * rescale_0_100(children_pct)
  ]
  dt[, care_mismatch_index := care_need_score - care_walk_index]
  invisible(dt)
}

walk15_facility_time_cols <- paste(model_facilities$facility_category, "walk_15min", sep = "_")
walk15_facility_labels <- setNames(model_facilities$label, walk15_facility_time_cols)

inner_london_boroughs <- c(
  "Camden",
  "City of London",
  "Greenwich",
  "Hackney",
  "Hammersmith and Fulham",
  "Islington",
  "Kensington and Chelsea",
  "Lambeth",
  "Lewisham",
  "Southwark",
  "Tower Hamlets",
  "Wandsworth",
  "Westminster"
)
