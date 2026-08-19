# Final manuscript figure provenance

The canonical figure folder is `figures/`. Its 26 files were
extracted byte-for-byte from
`outputs/manuscript/final/casa0010JiahuiLi.docx` on 19 August 2026. The SHA-256
manifest is `docs/FINAL_FIGURE_MANIFEST_20260819.csv`.

The older publication folders, alternative formats, source-data copies and
superseded exports were moved without deletion to
`Non‑essential Materials/figure_cleanup_20260819/figures_previous/`.

## Figure-to-code map

| Manuscript item | Canonical file | Maintained generating code | Status / invocation |
|---|---|---|---|
| Cover UCL logo | `figure0_ucl_logo.jpeg` | None | Static institutional logo asset; not analytically generated. |
| Figure 1 | `figure1_conceptual_framework.png` | `scripts/create_theoretical_framework_figure.R` | Maintained code lineage. The embedded final is a preserved earlier snapshot and is not pixel-identical to the current script output. |
| Figure 2 | `figure2_analytical_workflow.png` | `scripts/create_methods_workflow_figure.R` | Maintained code lineage. The embedded final is a preserved earlier snapshot and is not pixel-identical to the current script output. |
| Figure 3 | `figure3_rq_evidence_structure.png` | `scripts/create_rq_evidence_structure_figure.R` | `Rscript scripts/create_rq_evidence_structure_figure.R` |
| Figure 4 | `figure4_household_accessibility.png` | `scripts/create_nature_revised_dissertation_figures.R` | `Rscript scripts/create_nature_revised_dissertation_figures.R fig4` |
| Figure 5 | `figure5_deprivation_accessibility.png` | `scripts/revise_nonmap_figures_20260805.R` | `Rscript scripts/revise_nonmap_figures_20260805.R fig5` |
| Figure 6 | `figure6_household_mismatch.png` | `scripts/create_nature_revised_dissertation_figures.R` | `Rscript scripts/create_nature_revised_dissertation_figures.R fig6` |
| Figure 7 | `figure7_caregiving_diagnostics.png` | `scripts/create_nature_revised_dissertation_figures.R` | `Rscript scripts/create_nature_revised_dissertation_figures.R fig7` |
| Figure 8 | `figure8_need_access_quadrants.png` | `scripts/revise_nonmap_figures_20260805.R` | `Rscript scripts/revise_nonmap_figures_20260805.R fig8` |
| Figure 9 | `figure9_scenario_decomposition.png` | `scripts/make_figure9_only.R` | Preferred submission-only runner: `Rscript scripts/make_figure9_only.R`. The full source workflow is `scripts/additional_robustness_20260806.R`. |
| Figure 10 | `figure10_high_low_difference.png` | `scripts/revise_nonmap_figures_20260805.R` | `Rscript scripts/revise_nonmap_figures_20260805.R fig10` |
| Figure 11 | `figure11_care_need_gradient.png` | `scripts/revise_nonmap_figures_20260805.R` | `Rscript scripts/revise_nonmap_figures_20260805.R fig11` |
| Figure 12 | `figure12_case_accessibility.png` | `scripts/create_visual_revision_assets.py`, function `figure12_v10()` | The script writes `figure12_case_accessibility_v10.png`; the canonical file is the final walking-speed-corrected snapshot embedded in Word. |
| Appendix Figure A1 | `appendixA_figure_A1.png` | `scripts/create_visual_revision_assets.py`, function `appendix_a_step_free()` | The function is retained but is not called by the script's current `main()`; invoke it explicitly if regeneration is required. |
| Appendix Figure B1 | `appendixB_figure_B1.png` | `scripts/model/03_service_baskets.R` | Generated as `figure11_inner_outer_service_basket_accessibility.png` during the service-basket step. |
| Appendix Figure C1 | `appendixC_figure_C1.png` | `scripts/additional_robustness_20260806.R` | Generated as `appendix_figure_C1_imd_missingness_audit.png`. |
| Figure D1 | `appendixD_figure_D1.png` | `extension/scripts/10_make_extension_figures.R` | Extension workflow diagram. |
| Figure D2 | `appendixD_figure_D2.png` | `extension/scripts/10_make_extension_figures.R` | Centroid/grid accessibility comparison. |
| Figure D3 | `appendixD_figure_D3.png` | `extension/scripts/10_make_extension_figures.R` | Centroid-minus-grid difference. |
| Figure D4 | `appendixD_figure_D4.png` | `extension/scripts/10_make_extension_figures.R` | Within-LSOA variation. |
| Figure D5 | `appendixD_figure_D5.png` | `scripts/additional_robustness_20260806.R` | Final FDR-revised composite; the older unadjusted base version is also produced by `extension/scripts/10_make_extension_figures.R`. |
| Figure D6 | `appendixD_figure_D6.png` | `extension/scripts/10_make_extension_figures.R` | Compound mismatch map. |
| Figure D7 | `appendixD_figure_D7.png` | `extension/scripts/10_make_extension_figures.R` | Three-case comparison. |
| Figure D8 | `appendixD_figure_D8.png` | `extension/scripts/09_case_study_analysis.R` | Barking Riverside / Thames View detailed case map. |
| Figure D9 | `appendixD_figure_D9.png` | `extension/scripts/09_case_study_analysis.R` | Hillingdon 003D compound-mismatch case map. |
| Figure D10 | `appendixD_figure_D10.png` | `extension/scripts/09_case_study_analysis.R` | Barking and Dagenham 015F positive counter-case map. |

## Reproducibility note

The canonical PNGs are submission snapshots and should not be overwritten in
place. Most generating scripts still write to their historical working output
folders. Regenerate there, compare against the canonical image and replace the
Word image only after checking the resulting layout and caption.
