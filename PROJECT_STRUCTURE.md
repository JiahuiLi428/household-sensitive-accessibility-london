# UCL Dissertation Project Structure

Last reorganised 19 August 2026.

## Entry points

- Main pipeline: `scripts/model/run_pipeline.R`
- Main configuration: `scripts/model/00_config.R`
- Repository guide: `README.md`
- GitHub checklist: `GITHUB_UPLOAD_CHECKLIST.md`
- Local code and data audit: `docs/audit/` (excluded from GitHub)

## Live project

| Path | Contents |
|---|---|
| `data/raw/` | Original downloaded Census, OSM and lookup files |
| `data/boundaries/`, `data/census/`, `data/derived/`, `data/imd/`, `data/poi/`, `data/tfl_step_free/` | Cleaned and preprocessed inputs |
| `data/r5/london_network/` | R5 network inputs (build caches are regenerated in place and gitignored) |
| `data/output/r5r/`, `data/output/analysis/` | Routing outputs, main and robustness analysis tables |
| `scripts/model/` | Numbered main pipeline (01-12), configuration and run wrapper |
| `scripts/` | Supporting robustness, data-preparation and figure code |
| `extension/` | Appendix D residential-grid pipeline, intermediate data and outputs |
| `figures/` | The 26 image files actually embedded in the current manuscript, including the cover logo and all main/appendix figures |
| `outputs/manuscript/final/` | Local submitted manuscript and supporting revision files; excluded from GitHub |
| `cache/` | osmnx Overpass cache; required to rebuild Figure 12 offline |
| `docs/audit/` | Local code and data audit reports; excluded from GitHub |

## Archive

`Non‑essential Materials/` holds superseded drafts, QA renders, working
directories, E-series duplicates and test artefacts. Original relative paths are
preserved in an internal archive mirror. Nothing was deleted; see that folder's
`README_ARCHIVE_20260819.md` for what was moved and why.

The former multi-format `figures/` tree was additionally archived under
`Non‑essential Materials/figure_cleanup_20260819/`; the live figure-to-code map is
`docs/FIGURE_PROVENANCE_20260819.md`.

`_to_delete/` is now empty; its former contents were preserved under the archive.

## Regenerating the figures

- Figure 9: `Rscript scripts/make_figure9_only.R` for the submission figure only,
  or `Rscript scripts/additional_robustness_20260806.R` for the full robustness set
  (requires ggplot2 >= 3.5)
- Figure 12: `python scripts/create_visual_revision_assets.py` (uses `cache/` and
  `.codex_work/figure12/detail_context_*`, so no Overpass access is needed)
