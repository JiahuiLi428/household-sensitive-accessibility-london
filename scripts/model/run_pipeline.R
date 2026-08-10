# Runs model scripts 01-12 in dependency order and stops on the first failure.
# Reads/writes: delegated to each numbered script.
# Optional: pass --include-r5r to rerun the expensive routing stage.

source(file.path("scripts", "model", "00_config.R"))

MODEL_SCRIPT_DIR <- project_path("scripts", "model")

run_step <- function(label, command) {
  message("\n== ", label, " ==")
  status <- system2(command[1], command[-1])
  if (!identical(status, 0L)) {
    stop("Step failed: ", label)
  }
}

args <- commandArgs(trailingOnly = TRUE)
include_r5r <- "--include-r5r" %in% args

message("Project root: ", PROJECT_ROOT)
message("Use --include-r5r to rerun the expensive R5r travel-time matrix step.")

if (include_r5r) {
  run_step(
    "1. R5r nearest facility travel times",
    c("Rscript", file.path(MODEL_SCRIPT_DIR, "01_route_r5r.R"), "full")
  )
} else {
  message("\n== 1. R5r nearest facility travel times ==")
  message("Skipped; using existing data/output/r5r/nearest_facility_times_full_*.csv")
}

pipeline <- list(
  "2. Enrich R5r outputs with Census and IMD context" = "02_enrich_inputs.R",
  "3. Build service-basket accessibility and IMD equity outputs" = "03_service_baskets.R",
  "4. Build continuous need-access mismatch indices" = "04_mismatch_indices.R",
  "5. Build care-mobility accessibility and transit-gain outputs" = "05_care_mobility.R",
  "6. Build vulnerability typology and appendix regression outputs" = "06_typology_regression.R",
  "7. Run service-basket weight sensitivity checks" = "07_weight_sensitivity.R",
  "8. Run analytical and permutation-FDR Local Moran's I clustering" = "08_lisa_clusters.R",
  "9. Run demand-gradient summaries" = "09_demand_gradient.R",
  "10. Run transit-gain winners/losers summaries" = "10_transit_gain.R",
  "11. Run group-specific walking-speed sensitivity" = "11_walking_speed_sensitivity.R",
  "12. Run RQ2 group-level Kruskal-Wallis + BH-FDR tests" = "12_rq2_group_level_tests.R"
)

for (label in names(pipeline)) {
  run_step(label, c("Rscript", file.path(MODEL_SCRIPT_DIR, pipeline[[label]])))
}

message("\nPaper-model pipeline completed.")
