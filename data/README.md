# Data structure

## Original source data

- `raw/census/`: downloaded Census 2021 source archives.
- `raw/lookups/`: original LSOA lookup data.
- `raw/osm/`: original Greater London OSM PBF retained locally.
- `r5/london_network/`: OSM and GTFS inputs used to build the R5 network.

## Preprocessed inputs

- `boundaries/`: national and London LSOA boundary data.
- `census/`: cleaned London LSOA Census indicators.
- `imd/`: IMD2019 source and prepared data.
- `poi/`: extracted and point-normalised OSM facility data.
- `derived/`: derived spatial layers used in figures and cases.
- `tfl_step_free/`: TfL station topology and step-free input data.

## Model outputs

- `output/r5r/`: stored nearest-facility travel-time matrices.
- `output/analysis/`: service baskets, mismatch indices, spatial statistics,
  sensitivity checks and additional robustness results.

## Provenance

- `download_manifest.json`: data sources, expected paths and core counts.
- `CHECKSUMS.sha256`: checksums for the retained model inputs.
- `LARGE_FILES.md`: large local files excluded from the normal GitHub commit.

R5-generated `network.dat` and `*.mapdb*` caches are not original data and have
been archived under `非必须材料/原目录结构/data/r5/`. They can be rebuilt from
the OSM and GTFS inputs.
