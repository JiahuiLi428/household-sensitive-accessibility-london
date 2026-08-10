# Tests service-basket and need-weight sensitivity, including random weights.
# Reads: steps 02, 04 and 05 analysis outputs.
# Writes: deterministic and random sensitivity tables and a figure.

library(data.table)
library(ggplot2)

source(file.path("scripts", "model", "00_config.R"))

analysis_dir <- ANALYSIS_DIR
figure_dir <- PUBLICATION_FIGURE_DIR
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wide <- fread(file.path(analysis_dir, "r5r_accessibility_enriched_wide.csv"))
need <- fread(file.path(analysis_dir, "continuous_need_access_mismatch_indices.csv"))
care <- fread(file.path(analysis_dir, "care_mobility_accessibility_lsoa.csv"))

top_overlap_pct <- function(a, b, prop = 0.20) {
  a_top <- a >= quantile(a, 1 - prop, na.rm = TRUE)
  b_top <- b >= quantile(b, 1 - prop, na.rm = TRUE)
  round(sum(a_top & b_top, na.rm = TRUE) / sum(a_top, na.rm = TRUE) * 100, 1)
}

dirichlet_weights <- function(n, k) {
  x <- matrix(rexp(n * k, rate = 1), nrow = n, ncol = k)
  x / rowSums(x)
}

add_facility_scores(wide)
wide[, working_age_pct := pmax(0, 100 - children_pct - older_adults_pct)]
wide[, working_need_score := {
  rng <- range(working_age_pct, na.rm = TRUE)
  100 * (working_age_pct - rng[1]) / (rng[2] - rng[1])
}]

scores <- merge(
  wide[, .(
    LSOA21CD,
    gp_walk_score,
    pharmacy_walk_score,
    park_walk_score,
    school_walk_score,
    supermarket_walk_score,
    bus_stop_walk_score,
    hospital_walk_score,
    underground_walk_score,
    working_need_score
  )],
  need[, .(LSOA21CD, older_need_score, children_need_score)],
  by = "LSOA21CD",
  all.x = TRUE
)
scores <- merge(
  scores,
  care[, .(LSOA21CD, care_need_score)],
  by = "LSOA21CD",
  all.x = TRUE
)

groups <- list(
  "Children and Adolescents" = list(
    score_cols = paste0(names(service_basket_weights$family), "_walk_score"),
    weights = unname(service_basket_weights$family),
    need_col = "children_need_score"
  ),
  "Caregiving Adults" = list(
    score_cols = paste0(names(service_basket_weights$working), "_walk_score"),
    weights = unname(service_basket_weights$working),
    need_col = "care_need_score"
  ),
  "Older adults" = list(
    score_cols = paste0(names(service_basket_weights$older), "_walk_score"),
    weights = unname(service_basket_weights$older),
    need_col = "older_need_score"
  )
)

set.seed(20260603)
summary_rows <- list()
random_rows <- list()

for (group_name in names(groups)) {
  g <- groups[[group_name]]
  mat <- as.matrix(scores[, lapply(.SD, as.numeric), .SDcols = g$score_cols])
  current_index <- as.numeric(mat %*% g$weights)
  equal_weights <- rep(1 / length(g$weights), length(g$weights))
  equal_index <- as.numeric(mat %*% equal_weights)
  need_vec <- scores[[g$need_col]]
  current_mismatch <- need_vec - current_index
  equal_mismatch <- need_vec - equal_index

  random_weights <- dirichlet_weights(1000, length(g$weights))
  spearman_vals <- numeric(nrow(random_weights))
  overlap_vals <- numeric(nrow(random_weights))
  for (i in seq_len(nrow(random_weights))) {
    random_index <- as.numeric(mat %*% random_weights[i, ])
    random_mismatch <- need_vec - random_index
    spearman_vals[i] <- suppressWarnings(cor(current_index, random_index, method = "spearman", use = "complete.obs"))
    overlap_vals[i] <- top_overlap_pct(current_mismatch, random_mismatch)
  }

  summary_rows[[group_name]] <- data.table(
    group = group_name,
    n_facilities = length(g$weights),
    current_mean_index = round(mean(current_index, na.rm = TRUE), 2),
    equal_weight_mean_index = round(mean(equal_index, na.rm = TRUE), 2),
    equal_vs_current_spearman = round(cor(current_index, equal_index, method = "spearman", use = "complete.obs"), 3),
    equal_top20_mismatch_overlap_pct = top_overlap_pct(current_mismatch, equal_mismatch),
    random_spearman_median = round(median(spearman_vals, na.rm = TRUE), 3),
    random_spearman_p05 = round(quantile(spearman_vals, 0.05, na.rm = TRUE), 3),
    random_spearman_p95 = round(quantile(spearman_vals, 0.95, na.rm = TRUE), 3),
    random_top20_overlap_median_pct = round(median(overlap_vals, na.rm = TRUE), 1),
    random_top20_overlap_p05_pct = round(quantile(overlap_vals, 0.05, na.rm = TRUE), 1),
    random_top20_overlap_p95_pct = round(quantile(overlap_vals, 0.95, na.rm = TRUE), 1)
  )

  random_rows[[group_name]] <- data.table(
    group = group_name,
    iteration = seq_along(spearman_vals),
    spearman_rank_correlation = spearman_vals,
    top20_mismatch_overlap_pct = overlap_vals
  )
}

summary_dt <- rbindlist(summary_rows)
random_dt <- rbindlist(random_rows)

fwrite(summary_dt, file.path(analysis_dir, "service_basket_weight_sensitivity_summary.csv"))
fwrite(random_dt, file.path(analysis_dir, "service_basket_weight_sensitivity_random_iterations.csv"))

sens_fig <- ggplot(random_dt, aes(x = group, y = spearman_rank_correlation, fill = group)) +
  geom_violin(width = 0.82, alpha = 0.78, colour = "#5C5C5C", linewidth = 0.25) +
  geom_boxplot(width = 0.18, outlier.shape = NA, fill = "white", colour = "#333333", linewidth = 0.35) +
  coord_cartesian(ylim = c(0.6, 1.0)) +
  scale_fill_manual(values = c(
    "Children and Adolescents" = "#C77E5B",
    "Caregiving Adults" = "#4F7E9A",
    "Older adults" = "#6F8F65"
  )) +
  labs(
    title = "Service-Basket Weight Sensitivity",
    subtitle = "Spearman rank correlation between current basket scores and 1,000 random-weight alternatives",
    x = NULL,
    y = "Rank correlation with current index",
    caption = "Random weights are drawn from a Dirichlet-like distribution and normalised to sum to 1."
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9.5, colour = "#555555"),
    plot.caption = element_text(size = 7.8, colour = "#666666", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(figure_dir, "figure22_service_basket_weight_sensitivity.png"), sens_fig, width = 6.6, height = 4.2, dpi = 450)
ggsave(file.path(figure_dir, "figure22_service_basket_weight_sensitivity.pdf"), sens_fig, width = 6.6, height = 4.2)

message("Saved service basket weight sensitivity outputs.")
