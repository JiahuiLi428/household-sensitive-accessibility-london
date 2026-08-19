# Computes global and local Moran statistics for caregiving mismatch.
# Local inference is reported using analytical p-values, conditional
# permutation p-values, and Benjamini-Hochberg FDR-adjusted permutation p-values.
# Reads: step 06 typology output and the complete London LSOA geography.
# Writes: Moran/LISA tables and the LISA cluster map.

library(data.table)
library(sf)
library(spdep)
library(ggplot2)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
fig_dir <- PUBLICATION_FIGURE_DIR
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

lsoa_path <- project_path("data", "boundaries", "london_lsoa_2021.gpkg")
typology_path <- file.path(analysis_dir, "vulnerability_typology_lsoa.csv")
river_path <- project_path("data", "boundaries", "river_thames_osm.geojson")

lsoa <- st_make_valid(st_read(lsoa_path, quiet = TRUE))
typology <- fread(typology_path)

if (anyDuplicated(typology$LSOA21CD)) {
  stop("Typology input contains duplicate LSOA21CD values.")
}

# Build contiguity on the complete London geography, then subset the neighbour
# object to the actual analysis sample so exclusions are explicit and auditable.
full_nb <- poly2nb(lsoa, queen = TRUE)
typology_row <- match(lsoa$LSOA21CD, typology$LSOA21CD)
typology_cols <- setdiff(names(typology), c("LSOA21CD", "LSOA21NM"))
for (column in typology_cols) {
  lsoa[[column]] <- typology[[column]][typology_row]
}

keep <- !is.na(lsoa$family_care_mismatch_z)
n_dropped <- sum(!keep)
message(
  "LISA sample: retaining ", sum(keep), " of ", nrow(lsoa),
  " LSOAs; dropped ", n_dropped,
  " with missing family_care_mismatch_z."
)
if (!any(keep)) {
  stop("No non-missing family_care_mismatch_z observations available for LISA.")
}

g <- lsoa[keep, ]
nb <- subset(full_nb, subset = keep)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

x <- g$family_care_mismatch_z
x_centered <- x - mean(x, na.rm = TRUE)
lag_x <- lag.listw(listw, x_centered, zero.policy = TRUE)

global <- moran.test(x, listw, zero.policy = TRUE)
local <- localmoran(x, listw, zero.policy = TRUE)

# A fixed seed and one core make the conditional permutations reproducible.
# "Pr(folded) Sim" is spdep's empirical two-sided permutation pseudo-p-value.
local_perm_nsim <- 9999L
local_perm_seed <- 20260805L
spdep::set.coresOption(1L)
local_perm <- localmoran_perm(
  x,
  listw,
  nsim = local_perm_nsim,
  zero.policy = TRUE,
  alternative = "two.sided",
  iseed = local_perm_seed
)

perm_p_col <- "Pr(folded) Sim"
if (!perm_p_col %in% colnames(local_perm)) {
  stop(
    "Expected permutation p-value column '", perm_p_col,
    "' was not returned. Available columns: ",
    paste(colnames(local_perm), collapse = ", ")
  )
}

g$local_moran_i <- as.numeric(local[, "Ii"])
g$local_moran_p <- as.numeric(local[, "Pr(z != E(Ii))"])
g$local_moran_p_analytical <- g$local_moran_p
g$local_moran_p_permutation <- as.numeric(local_perm[, perm_p_col])
g$local_moran_p_fdr <- p.adjust(g$local_moran_p_permutation, method = "BH")
g$spatial_lag_mismatch <- lag_x

classify_lisa <- function(p_values, alpha = 0.05) {
  cluster <- rep("Not significant", length(p_values))
  sig <- !is.na(p_values) & p_values < alpha
  cluster[sig & x_centered > 0 & lag_x > 0] <- "High-high mismatch cluster"
  cluster[sig & x_centered < 0 & lag_x < 0] <- "Low-low surplus cluster"
  cluster[sig & x_centered > 0 & lag_x < 0] <- "High-low spatial outlier"
  cluster[sig & x_centered < 0 & lag_x > 0] <- "Low-high spatial outlier"
  cluster
}

g$lisa_cluster_analytical <- classify_lisa(g$local_moran_p_analytical)
g$lisa_cluster_permutation <- classify_lisa(g$local_moran_p_permutation)
g$lisa_cluster_fdr <- classify_lisa(g$local_moran_p_fdr)

# Retain the historical analytical classification under the original column
# name so the Appendix D grid comparison remains on the same inferential basis.
g$lisa_cluster <- g$lisa_cluster_analytical

summary_dt <- as.data.table(st_drop_geometry(g))[
  ,
  .(
    n_lsoas = .N,
    share_lsoas_pct = round(100 * .N / nrow(g), 2),
    mean_family_care_mismatch_z = round(mean(family_care_mismatch_z, na.rm = TRUE), 2),
    mean_family_care_demand_z = round(mean(family_care_demand_z, na.rm = TRUE), 2),
    mean_family_care_access = round(mean(family_care_access_score, na.rm = TRUE), 2),
    mean_imd_decile = round(mean(IMD_Decile, na.rm = TRUE), 2),
    mean_dep_child_hh_intensity = round(mean(child_household_intensity, na.rm = TRUE), 2)
  ),
  by = lisa_cluster
]

inference_comparison <- rbindlist(list(
  data.table(
    inference = "Analytical, unadjusted",
    p_value = g$local_moran_p_analytical,
    lisa_cluster = g$lisa_cluster_analytical
  ),
  data.table(
    inference = paste0("Conditional permutation, unadjusted (", local_perm_nsim, " simulations)"),
    p_value = g$local_moran_p_permutation,
    lisa_cluster = g$lisa_cluster_permutation
  ),
  data.table(
    inference = paste0("Conditional permutation + BH-FDR (", local_perm_nsim, " simulations)"),
    p_value = g$local_moran_p_fdr,
    lisa_cluster = g$lisa_cluster_fdr
  )
))[
  ,
  .(
    n_lsoas = .N,
    share_lsoas_pct = round(100 * .N / nrow(g), 2)
  ),
  by = .(inference, lisa_cluster)
]

high_high_comparison <- inference_comparison[
  lisa_cluster == "High-high mismatch cluster",
  .(inference, high_high_n = n_lsoas, high_high_share_pct = share_lsoas_pct)
]

cluster_order <- c(
  "High-high mismatch cluster",
  "Low-low surplus cluster",
  "High-low spatial outlier",
  "Low-high spatial outlier",
  "Not significant"
)
summary_dt[, lisa_cluster := factor(lisa_cluster, levels = cluster_order)]
setorder(summary_dt, lisa_cluster)
summary_dt[, lisa_cluster := as.character(lisa_cluster)]

global_dt <- data.table(
  indicator = "family_care_mismatch_z",
  global_moran_i = round(as.numeric(global$estimate["Moran I statistic"]), 3),
  expected_i = round(as.numeric(global$estimate["Expectation"]), 3),
  variance_i = round(as.numeric(global$estimate["Variance"]), 4),
  z_score = round(as.numeric(global$statistic), 2),
  # A z of ~81 underflows to 0 in double precision; report a bound rather than "0".
  p_value = if (global$p.value < 2.2e-16) "< 2.2e-16" else format(signif(global$p.value, 3)),
  n_lsoas = nrow(g)
)

p_label <- if (global_dt$p_value < 0.001) "< 0.001" else as.character(global_dt$p_value)

fwrite(summary_dt, file.path(analysis_dir, "family_care_mismatch_lisa_summary.csv"))
fwrite(
  inference_comparison,
  file.path(analysis_dir, "family_care_mismatch_lisa_inference_comparison.csv")
)
fwrite(
  high_high_comparison,
  file.path(analysis_dir, "family_care_mismatch_lisa_high_high_comparison.csv")
)
fwrite(global_dt, file.path(analysis_dir, "family_care_mismatch_global_moran.csv"))
fwrite(
  as.data.table(st_drop_geometry(g))[
    ,
    .(
      LSOA21CD,
      LSOA21NM,
      borough_name,
      london_ring,
      IMD_Decile,
      family_care_demand_z,
      family_care_access_score,
      family_care_mismatch_z,
      child_household_intensity,
      local_moran_i,
      local_moran_p,
      local_moran_p_analytical,
      local_moran_p_permutation,
      local_moran_p_fdr,
      spatial_lag_mismatch,
      lisa_cluster,
      lisa_cluster_analytical,
      lisa_cluster_permutation,
      lisa_cluster_fdr
    )
  ],
  file.path(analysis_dir, "family_care_mismatch_lisa_lsoa.csv")
)

river <- NULL
if (file.exists(river_path)) {
  river <- st_read(river_path, quiet = TRUE)
  river <- st_transform(river, st_crs(g))
}

borough <- aggregate(
  g[, "borough_name"],
  by = list(borough_name_group = g$borough_name),
  FUN = function(x) x[1]
)

cluster_palette <- c(
  "High-high mismatch cluster" = "#B2182B",
  "Low-low surplus cluster" = "#2166AC",
  "High-low spatial outlier" = "#EF8A62",
  "Low-high spatial outlier" = "#67A9CF",
  "Not significant" = "#ECE7DF"
)

map <- ggplot(g) +
  geom_sf(aes(fill = lisa_cluster), colour = NA) +
  geom_sf(data = borough, fill = NA, colour = "#FFFFFF", linewidth = 0.22, alpha = 0.8) +
  scale_fill_manual(values = cluster_palette, name = NULL, drop = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "Local Clusters of Caregiving Adult Need-Access Mismatch",
    subtitle = paste0(
      "Analytical Local Moran's I clusters, unadjusted p < 0.05; Global Moran's I = ",
      global_dt$global_moran_i,
      ", p ",
      p_label
    ),
    caption = "High-high clusters indicate spatially contiguous LSOAs where caregiving adult need exceeds accessibility. Borough boundaries are shown for reference."
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 21, face = "bold", colour = "#202020", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 12, colour = "#555555", margin = margin(b = 12)),
    plot.caption = element_text(size = 8.5, colour = "#666666", hjust = 0, margin = margin(t = 10)),
    legend.position = c(0.17, 0.19),
    legend.background = element_rect(fill = "#FFFFFF", colour = "#D8D8D8", linewidth = 0.25),
    legend.key.size = unit(0.42, "cm"),
    legend.text = element_text(size = 9, colour = "#3A3A3A"),
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    plot.margin = margin(20, 28, 18, 28)
  )

map_fdr <- ggplot(g) +
  geom_sf(aes(fill = lisa_cluster_fdr), colour = NA) +
  geom_sf(data = borough, fill = NA, colour = "#FFFFFF", linewidth = 0.22, alpha = 0.8) +
  scale_fill_manual(values = cluster_palette, name = NULL, drop = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "Permutation- and FDR-Robust Local Clusters of Caregiving Mismatch",
    subtitle = paste0(
      "Conditional Local Moran's I, ", local_perm_nsim,
      " permutations; BH-FDR q < 0.05; seed ", local_perm_seed
    ),
    caption = "High-high clusters are the subset that remains significant after conditional permutation inference and London-wide false-discovery-rate correction."
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 20, face = "bold", colour = "#202020", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 11.5, colour = "#555555", margin = margin(b = 12)),
    plot.caption = element_text(size = 8.5, colour = "#666666", hjust = 0, margin = margin(t = 10)),
    legend.position = c(0.17, 0.19),
    legend.background = element_rect(fill = "#FFFFFF", colour = "#D8D8D8", linewidth = 0.25),
    legend.key.size = unit(0.42, "cm"),
    legend.text = element_text(size = 9, colour = "#3A3A3A"),
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    plot.margin = margin(20, 28, 18, 28)
  )

if (!is.null(river)) {
  map <- map +
    geom_sf(data = river, inherit.aes = FALSE, fill = NA, colour = "#B8D7E8", linewidth = 0.65, alpha = 0.95)
  map_fdr <- map_fdr +
    geom_sf(data = river, inherit.aes = FALSE, fill = NA, colour = "#B8D7E8", linewidth = 0.65, alpha = 0.95)
}

ggsave(
  file.path(fig_dir, "figure18_family_care_mismatch_lisa_clusters.png"),
  map,
  width = 11.5,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(fig_dir, "figure18_family_care_mismatch_lisa_clusters.pdf"),
  map,
  width = 11.5,
  height = 8
)

ggsave(
  file.path(fig_dir, "figure18b_family_care_mismatch_lisa_permutation_fdr.png"),
  map_fdr,
  width = 11.5,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(fig_dir, "figure18b_family_care_mismatch_lisa_permutation_fdr.pdf"),
  map_fdr,
  width = 11.5,
  height = 8
)

message("Global Moran's I: ", global_dt$global_moran_i, " (p=", global_dt$p_value, ")")
print(summary_dt)
print(high_high_comparison)
