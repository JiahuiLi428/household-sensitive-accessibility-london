# Data audit - 2026-07-02

## Overall status

The required project data are present and internally consistent for the London LSOA 2021 accessibility analysis. Manifest-declared files exist, ZIP files passed integrity checks, all major LSOA-level analytical tables cover 4,994 unique London LSOA 2021 codes, and POI counts match the manifest.

## Source authenticity check

The local manifest points to real official or established source datasets:

- Census household composition: Nomis / ONS Census 2021 TS003.
- Census age data: Nomis / ONS Census 2021 age tables. The local file is a broad-age-band processed table derived from Census 2021 age categories.
- IMD: GOV.UK English Indices of Deprivation 2019, File 1.
- LSOA 2021 boundaries: ONS Open Geography Portal LSOA December 2021 boundaries.
- LSOA 2011 to 2021 lookup: ONS Open Geography best-fit lookup.
- Bus GTFS: DfT Bus Open Data Service GTFS download.
- OSM PBF: Geofabrik Greater London OpenStreetMap extract.
- TfL step-free station data: TfL open data / station data files.

Important reproducibility note: dynamic transport and OSM feeds are real source snapshots, but they are not timeless. The local Geofabrik file matches the June 2026-era Greater London PBF size seen in the public listing, while newer OSM extracts exist as of 2026-07-02. For dissertation reproducibility, keep the local snapshot date explicit rather than describing it as the current OSM state.

## Completeness checks

### Core spatial and demographic coverage

- London LSOA boundary subset: 4,994 LSOAs recorded in manifest.
- `data/census/TS007B_age_broad_bands_london_lsoa_2021.csv`: 4,994 rows, 4,994 unique `LSOA21CD`, no blank IDs, no duplicate IDs.
- `data/census/TS003_household_composition_selected_london_lsoa_2021.csv`: 4,994 rows, 4,994 unique `LSOA21CD`, no blank IDs, no duplicate IDs.
- `data/census/TS003_household_composition_london_lsoa_2021_enhanced.csv`: 4,994 rows, 4,994 unique `LSOA21CD`, no blank IDs, no duplicate IDs.

### R5r travel-time outputs

- `nearest_facility_times_full_wide.csv`: 4,994 rows, 4,994 unique `LSOA21CD`.
- `nearest_facility_times_full_long.csv`: 79,904 rows. This equals 4,994 LSOAs x 8 facility groups x 2 scenarios.
- `r5r_accessibility_enriched_wide.csv`: 4,994 rows, 4,994 unique `LSOA21CD`.
- `r5r_accessibility_enriched_long.csv`: 79,904 rows, 4,994 unique `LSOA21CD`.

Blank travel-time cells are expected where no facility is reachable within the scenario threshold. They are not evidence of file corruption.

### POI counts

All processed POI point CSV row counts match `download_manifest.json`:

- Schools: 3,266
- GPs: 955
- Hospitals: 206
- Pharmacies: 1,289
- Supermarkets: 1,258
- Parks: 3,230
- Underground/metro stations: 1,540
- Bus stops: 26,359

### ZIP integrity

All checked ZIP files returned no bad entries:

- `data/r5/london_gtfs_bods.zip`
- `data/r5/london_network/london_gtfs_bods.zip`
- `data/raw/census/census2021-ts003.zip`
- `data/raw/census/census2021-ts007.zip`
- `data/tfl_step_free/tfl-stationdata-detailed.zip`
- `data/tfl_step_free/tfl-stationdata-gtfs.zip`

## Issues and limitations

### IMD boundary-year mismatch

IMD 2019 is published on 2011 LSOA geography, while the project analysis uses 2021 LSOA geography. The local matching summary reports:

- 4,994 London LSOA rows.
- 4,659 direct IMD matches.
- 4,813 matched after best-fit lookup.
- 154 LSOAs assigned via best-fit lookup.
- 181 LSOAs still missing IMD after matching.

This is the main real coverage gap. It should be described as a boundary-version limitation, not as missing raw data.

### R5/GTFS quality note

`data/r5/london_network/gtfs_errors.csv` contains one medium-priority suspect stop location error:

- `stops`, line 21672, `SuspectStopLocationError`, stop_id `940GZZLUBZP`.

This is small relative to the feed and does not indicate a failed GTFS import, but it should be retained in the methodology limitations.

### Reachability gaps are model outcomes

Some scenario columns have high missing percentages because facilities are not reachable within the travel-time threshold. Examples from the quality summary:

- Hospitals within 15-minute walk: 87.4% missing.
- Underground/metro within 15-minute walk: 71.9% missing.
- GPs within 15-minute walk: 39.3% missing.
- Supermarkets within 15-minute walk: 34.2% missing.

These are substantive accessibility findings, not omitted records.

## Recommended dissertation wording

The dataset is sufficiently complete for the stated London LSOA accessibility analysis, with one explicit caveat: deprivation measures use IMD 2019 on 2011 LSOA geography and are joined to 2021 LSOAs using direct and best-fit matching, leaving 181 unmatched LSOAs. Dynamic OSM and GTFS inputs should be reported as dated snapshots, not current live data.
