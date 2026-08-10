suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(ggplot2)
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
    if (identical(parent, current)) {
      stop("Could not locate project root from: ", start)
    }
    current <- parent
  }
}

project_root <- find_project_root()
rel_path <- function(...) file.path(project_root, ...)

args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--test" %in% args) "test" else "full"
suffix <- if (run_mode == "test") "_test" else ""

extension_dir <- rel_path("extension")
log_dir <- file.path(extension_dir, "logs")
table_dir <- file.path(extension_dir, "outputs", "tables")
map_dir <- file.path(extension_dir, "outputs", "maps")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("07_compound_mismatch_", run_mode, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)

started_at <- Sys.time()
message("07_compound_mismatch.R started at: ", started_at)

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required input does not exist: ", path)
  }
  invisible(path)
}

required_fields <- function(x, fields, object_name) {
  missing <- setdiff(fields, names(x))
  if (length(missing) > 0) {
    stop(object_name, " is missing required fields: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

mismatch_path <- required_file(file.path(table_dir, paste0("grid_informed_mismatch", suffix, ".csv")))
lsoa_path <- required_file(rel_path("data", "boundaries", "london_lsoa_2021.gpkg"))
river_path <- rel_path("data", "boundaries", "river_thames_osm.geojson")

mismatch <- fread(mismatch_path)
lsoas <- st_read(lsoa_path, quiet = TRUE)
required_fields(
  mismatch,
  c("LSOA21CD", "LSOA21NM", "borough_name", "group", "grid_mean_lisa_cluster"),
  "grid-informed mismatch table"
)

hh <- mismatch[
  ,
  .(
    high_high = grid_mean_lisa_cluster == "High-high mismatch cluster"
  ),
  by = .(LSOA21CD, LSOA21NM, borough_name, group)
]
wide <- dcast(hh, LSOA21CD + LSOA21NM + borough_name ~ group, value.var = "high_high", fill = FALSE)

for (col in c("children_and_adolescents", "caregiving_adults", "older_adults")) {
  if (!col %in% names(wide)) {
    wide[, (col) := FALSE]
  }
}

setnames(
  wide,
  old = c("children_and_adolescents", "caregiving_adults", "older_adults"),
  new = c("child_hh", "caregiver_hh", "older_hh")
)
wide[, `:=`(
  child_hh = as.integer(child_hh),
  caregiver_hh = as.integer(caregiver_hh),
  older_hh = as.integer(older_hh)
)]
wide[, compound_count := child_hh + caregiver_hh + older_hh]
wide[, compound_class := fifelse(
  compound_count == 0, "0 = no high-high mismatch",
  fifelse(
    compound_count == 1, "1 = single-group mismatch",
    fifelse(compound_count == 2, "2 = two-group compound mismatch", "3 = three-group compound mismatch")
  )
)]
wide[, compound_type := fifelse(
  compound_count == 0, "none",
  fifelse(
    child_hh == 1 & caregiver_hh == 0 & older_hh == 0, "children_only",
    fifelse(
      child_hh == 0 & caregiver_hh == 1 & older_hh == 0, "caregiver_only",
      fifelse(
        child_hh == 0 & caregiver_hh == 0 & older_hh == 1, "older_only",
        fifelse(
          child_hh == 1 & caregiver_hh == 1 & older_hh == 0, "children_caregiver",
          fifelse(
            child_hh == 1 & caregiver_hh == 0 & older_hh == 1, "children_older",
            fifelse(
              child_hh == 0 & caregiver_hh == 1 & older_hh == 1, "caregiver_older",
              "all_three"
            )
          )
        )
      )
    )
  )
)]

compound_path <- file.path(table_dir, paste0("compound_mismatch", suffix, ".csv"))
fwrite(wide, compound_path)

borough_summary <- wide[
  ,
  .(
    borough_lsoa_count = .N,
    single_group_count = sum(compound_count == 1),
    two_group_count = sum(compound_count == 2),
    three_group_count = sum(compound_count == 3),
    share_of_borough_lsoas = round(mean(compound_count > 0) * 100, 2)
  ),
  by = .(borough = borough_name)
][order(-three_group_count, -two_group_count, -single_group_count)]
borough_path <- file.path(table_dir, paste0("compound_mismatch_by_borough", suffix, ".csv"))
fwrite(borough_summary, borough_path)

overall <- wide[
  ,
  .(
    n_lsoas = .N,
    single_group_count = sum(compound_count == 1),
    two_group_count = sum(compound_count == 2),
    three_group_count = sum(compound_count == 3),
    any_compound_or_single = sum(compound_count > 0)
  )
]
message("Wrote compound mismatch: ", compound_path)
message("Wrote borough summary: ", borough_path)
print(overall)
print(wide[, .N, by = compound_type][order(-N)])

if (run_mode == "full") {
  map_data <- merge(lsoas, wide, by = "LSOA21CD", all.x = TRUE)
  river <- NULL
  if (file.exists(river_path)) {
    river <- st_transform(st_read(river_path, quiet = TRUE), st_crs(map_data))
  }
  boroughs <- aggregate(map_data[, "borough_name"], by = list(borough_name_group = map_data$borough_name), FUN = function(x) x[1])
  map_data$compound_class[is.na(map_data$compound_class)] <- "0 = no high-high mismatch"
  palette <- c(
    "0 = no high-high mismatch" = "#ECEBE7",
    "1 = single-group mismatch" = "#A6CEE3",
    "2 = two-group compound mismatch" = "#FDBF6F",
    "3 = three-group compound mismatch" = "#B2182B"
  )
  p <- ggplot(map_data) +
    geom_sf(aes(fill = compound_class), colour = NA) +
    geom_sf(data = boroughs, fill = NA, colour = "#FFFFFF", linewidth = 0.16) +
    scale_fill_manual(values = palette, name = NULL, drop = FALSE) +
    labs(
      title = "Compound grid-informed high-high mismatch",
      subtitle = "Number of population groups in grid-informed LISA high-high clusters"
    ) +
    theme_void(base_size = 9) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "#555555"),
      legend.position = c(0.20, 0.20),
      legend.background = element_rect(fill = "white", colour = "#D0D0D0", linewidth = 0.2),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  if (!is.null(river)) {
    p <- p + geom_sf(data = river, inherit.aes = FALSE, colour = "#B8D7E8", linewidth = 0.45)
  }
  ggsave(file.path(map_dir, "compound_mismatch_map.png"), p, width = 7.2, height = 6.7, dpi = 300)
  ggsave(file.path(map_dir, "compound_mismatch_map.pdf"), p, width = 7.2, height = 6.7)
}

finished_at <- Sys.time()
message("Finished at: ", finished_at)
message("Elapsed seconds: ", round(as.numeric(difftime(finished_at, started_at, units = "secs")), 2))
