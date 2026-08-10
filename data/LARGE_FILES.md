# Large data files retained locally

The following files are required only when rebuilding expensive preprocessing or
routing stages. They are retained in the local project but excluded from the
standard GitHub commit by `.gitignore`.

| Local path | Approximate size | Purpose / recovery |
|---|---:|---|
| `data/raw/osm/greater-london-latest.osm.pbf` | 120 MB | Original Geofabrik Greater London OSM extract; source URL is in `download_manifest.json` |
| `data/r5/london_network/greater-london-latest.osm.pbf` | 120 MB | R5 network input copy of the OSM extract |
| `data/r5/london_network/london_gtfs_bods.zip` | 300 MB | London BODS GTFS input; source URL is in `download_manifest.json` |
| `data/poi/transport_bus_stops.geojson` | 219 MB | Reproducible OSM extraction; a compact point CSV and extraction code are retained |
| `extension/data_intermediate/osm_residential_features.gpkg` | 229 MB | Reproducible Appendix D residential-feature intermediate |

Generated R5 files such as `network.dat` and `*.mapdb*` are not source data.
They have been moved to `非必须材料/原目录结构/data/r5/` because R5r can rebuild
them from the OSM and GTFS inputs.

If the full binary inputs must be distributed, use an institutional data store,
a release asset or a configured large-file service and record the permanent link
in this file. Do not commit them to a normal Git history.
