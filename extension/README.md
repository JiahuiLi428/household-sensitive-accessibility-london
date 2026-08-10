# Appendix D residential-grid robustness extension

This folder contains the 100 m residential-grid sensitivity analysis,
centroid-grid comparison, grid-informed mismatch and LISA analysis, compound
mismatch analysis, and comparative neighbourhood cases.

## Contents

- `scripts/`: numbered extension workflow.
- `data_intermediate/`: grid and residential-feature intermediate data.
- `outputs/tables/`: final extension tables.
- `outputs/maps/`: PNG/PDF/SVG map exports.
- `outputs/case_studies/`: case profiles and PNG/PDF/SVG maps.

Duplicate TIFF exports were moved to `非必须材料`; equivalent PNG files remain.

## Run order

Run from the repository root:

1. `01_build_100m_grid.R`
2. `02_filter_residential_grid.R`
3. `03_run_grid_accessibility.R`
4. `helpers/derive_all_group_grid_baskets.R`
5. `04_aggregate_grid_to_lsoa.R`
6. `05_compare_centroid_grid.R`
7. `06_recalculate_grid_informed_mismatch.R`
8. `07_compound_mismatch.R`
9. `08_select_case_studies.R`
10. `09_case_study_analysis.R`
11. `10_make_extension_figures.R`

The 229 MB `data_intermediate/osm_residential_features.gpkg` file is retained
locally but excluded from the standard GitHub commit; see `data/LARGE_FILES.md`.
