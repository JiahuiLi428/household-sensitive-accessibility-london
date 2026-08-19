# GitHub upload checklist

## Before upload

1. Keep working dissertation DOCX files excluded while supervisory revisions
   remain in progress. Add only the final submitted manuscript, if desired,
   after submission.
2. Complete the cover placeholders, repository URL and Appendix E research log
   inside the dissertation before final submission.
3. Review `data/LARGE_FILES.md`. Large local files are intentionally excluded
   from GitHub; do not force-add them unless Git LFS has been configured.
4. Do not upload `Non‑essential Materials/`. It contains old drafts, internal review documents,
   QA renders and caches.

## Inspect and stage the repository

```powershell
git add .
git status --short
```

Check that no staged file is unexpectedly large:

```powershell
git ls-files | ForEach-Object {
  if (Test-Path -LiteralPath $_) {
    $item = Get-Item -LiteralPath $_
    if ($item.Length -ge 90MB) {
      [pscustomobject]@{ MB = [math]::Round($item.Length / 1MB, 2); File = $_ }
    }
  }
}
```

## Final checks

- `README.md` opens correctly on GitHub.
- `data/download_manifest.json` contains usable source links.
- `data/CHECKSUMS.sha256` is included.
- The main and extension scripts are included.
- Numerical CSV outputs needed to verify manuscript results are included.
- No personal notes, supervisor feedback, temporary Word locks or old drafts are staged.
- No working dissertation DOCX file is staged before submission.
- The GitHub repository URL has been inserted into the final dissertation if required.
