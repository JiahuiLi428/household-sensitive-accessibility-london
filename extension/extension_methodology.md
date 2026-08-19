# Extension methodology

This extension keeps the original LSOA analysis and adds a 100m residential-grid robustness layer for accessibility.

The LSOA scale is retained because the Census-derived need indicators are area-level proxies. The 100m grid improves the spatial precision of origins for accessibility only; it does not create new individual-level need estimates.

Grid cells were constructed in EPSG:27700 and assigned to LSOAs by centroid location. Residential origins were filtered using OSM residential building footprints and residential land-use polygons. Residential weights use intersected residential building footprint area where available, with land-use-only residential cells retained at weight 1.

Grid accessibility was calculated using the original R5r network, POI categories, travel-time settings, decay function and basket weights. Grid scores were aggregated back to LSOAs using mean, median, p10, lowest-decile mean, within-LSOA SD/IQR/min/max, and weighted mean/median.

Centroid-grid comparison uses Pearson and Spearman correlations, absolute differences, RMSE, lowest-accessibility quintile overlap, high-mismatch quintile overlap, Jaccard indices and threshold sensitivity at 0.25, 0.50 and 0.75 SD.

Grid-informed mismatch keeps the original need score and z-score mismatch logic, replacing only centroid-based accessibility with grid-informed LSOA accessibility. LISA uses queen contiguity, row-standardised spatial weights and analytical p < 0.05, matching the original approach. Conditional-permutation (9,999 simulations) and Benjamini-Hochberg FDR results are additionally reported for every group in Appendix D.

Compound mismatch counts whether children/adolescents, caregiving adults and older adults are each in grid-informed high-high mismatch clusters. Case studies were selected from automatic candidate lists: one fixed Barking Riverside / Thames View case, one compound mismatch case and one positive counter-case matched on need-related variables.
