"""Read-only structural audit of the live dissertation repository."""

from __future__ import annotations

import csv
import sys
import zipfile
from pathlib import Path


EXCLUDED_PARTS = {".git", ".codex_work", "Non-essential Materials", "Non‑essential Materials"}


def is_live(path: Path, root: Path) -> bool:
    relative = path.relative_to(root)
    if set(relative.parts) & EXCLUDED_PARTS:
        return False
    return relative.parts[:3] != ("outputs", "manuscript", "final")


def main() -> None:
    root = Path(sys.argv[1]).resolve()
    files = [p for p in root.rglob("*") if p.is_file() and is_live(p, root)]
    print(f"LIVE_FILES={len(files)}")
    print(f"LIVE_BYTES={sum(p.stat().st_size for p in files)}")

    zero = [p.relative_to(root).as_posix() for p in files if p.stat().st_size == 0]
    print(f"ZERO_BYTE_FILES={len(zero)}")
    for item in zero:
        print(f"  ZERO {item}")

    malformed: list[str] = []
    csv_count = 0
    csv_rows = 0
    for path in files:
        if path.suffix.lower() not in {".csv", ".tsv"}:
            continue
        csv_count += 1
        delimiter = "\t" if path.suffix.lower() == ".tsv" else ","
        try:
            with path.open("r", encoding="utf-8-sig", newline="") as handle:
                reader = csv.reader(handle, delimiter=delimiter, strict=True)
                header = next(reader, None)
                if not header:
                    malformed.append(f"{path.relative_to(root)}: missing header")
                    continue
                width = len(header)
                for line_no, row in enumerate(reader, 2):
                    csv_rows += 1
                    if len(row) != width:
                        malformed.append(
                            f"{path.relative_to(root)}:{line_no}: {len(row)} columns, expected {width}"
                        )
                        break
        except (OSError, UnicodeError, csv.Error) as exc:
            malformed.append(f"{path.relative_to(root)}: {exc}")
    print(f"TABULAR_FILES={csv_count} DATA_ROWS_SCANNED={csv_rows} MALFORMED={len(malformed)}")
    for item in malformed:
        print(f"  MALFORMED {item}")

    docx_errors: list[str] = []
    for path in files:
        if path.suffix.lower() != ".docx":
            continue
        try:
            with zipfile.ZipFile(path) as archive:
                bad = archive.testzip()
                if bad:
                    docx_errors.append(f"{path.relative_to(root)}: corrupt member {bad}")
        except zipfile.BadZipFile as exc:
            docx_errors.append(f"{path.relative_to(root)}: {exc}")
    print(f"DOCX_ERRORS={len(docx_errors)}")
    for item in docx_errors:
        print(f"  DOCX_ERROR {item}")

    required = [
        "scripts/model/run_pipeline.R",
        "scripts/model/00_config.R",
        "data/output/analysis/r5r_accessibility_enriched_wide.csv",
        "data/output/analysis/explanatory_regression_model_summary.csv",
        "data/output/analysis/four_faces_london_borough_case_studies.csv",
        "extension/outputs/tables/table_D5a_case_metrics_compact.csv",
        "figures/figure9_scenario_decomposition.png",
        "figures/figure12_case_accessibility.png",
    ]
    missing = [item for item in required if not (root / item).is_file()]
    print(f"REQUIRED_MISSING={len(missing)}")
    for item in missing:
        print(f"  MISSING {item}")

    if zero or malformed or docx_errors or missing:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
