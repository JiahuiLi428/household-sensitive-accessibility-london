# Main model pipeline

Run all commands from the repository root.

```powershell
Rscript scripts/model/run_pipeline.R
```

This default run reuses `data/output/r5r/nearest_facility_times_full_*.csv` and
runs analysis steps 02-12. Use `--include-r5r` only when the large OSM and GTFS
inputs are locally available:

```powershell
Rscript scripts/model/run_pipeline.R --include-r5r
```

## Script order

1. `01_route_r5r.R`: nearest-facility routing for 4,994 London LSOAs.
2. `02_enrich_inputs.R`: Census and IMD enrichment.
3. `03_service_baskets.R`: facility scores, group baskets and facility-level IMD tests.
4. `04_mismatch_indices.R`: continuous need-access mismatch indices.
5. `05_care_mobility.R`: caregiving accessibility, need and transit-gain outputs.
6. `06_typology_regression.R`: vulnerability typology and exploratory regressions.
7. `07_weight_sensitivity.R`: equal and random service-basket weight checks.
8. `08_lisa_clusters.R`: analytical and permutation/FDR Local Moran analysis.
9. `09_demand_gradient.R`: caregiving-need gradient summaries.
10. `10_transit_gain.R`: top/bottom scenario-difference comparisons.
11. `11_walking_speed_sensitivity.R`: group-specific walking-speed sensitivity.
12. `12_rq2_group_level_tests.R`: separate nine-test RQ2 group-level
    Kruskal-Wallis and BH-FDR family.

`00_config.R` is the authoritative source for paths, thresholds, facility lists,
weights and scenario settings. The pipeline stops on the first failed step.

## Core outputs

- `data/output/analysis/composite_service_basket_accessibility_indices.csv`
- `data/output/analysis/continuous_need_access_mismatch_indices.csv`
- `data/output/analysis/care_mobility_accessibility_lsoa.csv`
- `data/output/analysis/vulnerability_typology_lsoa.csv`
- `data/output/analysis/family_care_mismatch_lisa_lsoa.csv`
- `data/output/analysis/service_basket_weight_sensitivity_summary.csv`
- `data/output/analysis/walking_speed_sensitivity_summary.csv`
- `data/output/analysis/examiner_checks/rq2_group_level_kw_fdr.csv`

The final item is created when step 12 is run and may be absent from a checkout
that contains only previously stored outputs.
