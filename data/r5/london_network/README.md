# R5 network inputs

To rebuild the R5 network, place these files in this directory:

- `greater-london-latest.osm.pbf`
- `london_gtfs_bods.zip`

Source URLs and checksums are recorded in `data/download_manifest.json` and
`data/CHECKSUMS.sha256`. Both files are retained locally but excluded from the
normal Git commit because of their size.

R5r will regenerate `network.dat` and `*.mapdb*`. Those generated cache files
are not source data and are excluded from version control.
