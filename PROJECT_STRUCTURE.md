# UCL Dissertation Project Structure

## Entry points

- Final dissertation: `outputs/manuscript/final/newdraft_CASA_compliance_polished_20260806_v13.9_crossreview_fixes2.docx`
- Main pipeline: `scripts/model/run_pipeline.R`
- Main configuration: `scripts/model/00_config.R`
- Repository guide: `README.md`
- GitHub checklist: `GITHUB_UPLOAD_CHECKLIST.md`

## Submission materials retained in the main project

- `outputs/manuscript/final/`: one active dissertation manuscript.
- `data/raw/`: original downloaded source data retained locally.
- `data/boundaries/`, `data/census/`, `data/derived/`, `data/imd/`,
  `data/poi/`, `data/tfl_step_free/`: preprocessed research inputs.
- `data/output/r5r/`: stored routing results.
- `data/output/analysis/`: final analytical and robustness outputs.
- `scripts/model/`: numbered main analysis pipeline, including RQ2 group-level tests.
- `scripts/`: supporting data, robustness and figure code.
- `extension/`: Appendix D scripts, intermediate data and final outputs.
- `figures/`: final publication, revised non-map and additional-robustness figures.

## Local-only large files

Files exceeding normal GitHub limits remain in their research folders but are
excluded through `.gitignore`. See `data/LARGE_FILES.md`.

## Reversible archive

`非必须材料/原目录结构/` contains superseded drafts, QA renders, caches, logs,
old presentation material, internal review documents, the optional legacy
ArcMap branch and regenerated R5 cache files. Nothing was deleted, and the
original relative paths are preserved.
