# ForestPlots Upload Checker

Validates Excel files against ForestPlots upload rules before submission.
Returns a tidy data frame of issues — one row per problem, with the Excel
row number, Tree ID, affected census and column, and a plain-English
description of each issue.

---

## Project structure

```
fp/
├── R/
│   ├── constants.R              # Column definitions and valid-value sets
│   ├── utils.R                  # Shared helpers (log_issue, is_valid_f2, …)
│   └── check_new_multicensus.R  # Checks for new-plot multi-census uploads
├── run_checks.R                 # Entry point — source this file
└── README.md
```

---

## Quick start

```r
source("run_checks.R")

issues <- run_checks(
  dataset_type = "new_multicensus",
  file_path    = "path/to/your/upload.xlsx",
  sheet_name   = "sheet_name_or_index"  # e.g. "plot001" or 1
)

issues          # view in RStudio — zero rows means file passed all checks
nrow(issues)

# Export results to Excel for sharing with field teams
run_checks(
  "new_multicensus",
  "path/to/your/upload.xlsx",
  export_path = "path/to/your/upload_issues.xlsx"
)
```

---

## Supported dataset types

| `dataset_type`    | Description                                                                     |
|-------------------|---------------------------------------------------------------------------------|
| `new_multicensus` | New plots with no existing ForestPlots ID, containing two or more census blocks |

---

## Checks performed (`new_multicensus`)

| # | Section | What is checked |
|---|---------|-----------------|
| 1 | Row 1 — header structure | A1 label; `Census No:` headers present, correctly positioned, and contain a 4-digit year |
| 2 | Row 2 — column names | All 13 fixed columns and every per-census block column match expected names exactly |
| 3 | Required fields | TreeID **blank**; Family, Species, Tag No, D, POM, Flag1, Flag2 not empty in active census blocks |
| 4 | Stem Grouping | Value shared across ≥ 2 rows; T1 and Species consistent within each group; Flag1 contains `h` |
| 5 | Flag1 — character rules | Only valid characters; `a` only with `n`/`h`; stems absent in prior census must have `n` |
| 6 | Flag2 — validity | Blank when Flag1 blank; valid code when Flag1 alive |
| 7 | Flag3 — validity | Empty when dead (Flag1 = 0); filled with 0–6 when alive |
| 8 | Flag4 — validity | Empty when dead; valid code when alive; `≠ 0` when D0 ≠ D; `= 60` when POM changed |
| 9 | LI / CI / CF | Values within allowed ranges (1–4, ordinal codes, 0–4 respectively) |
| 10 | Flag5 / Height | Flag5 filled (1–6) when Height present; Flag5 empty when Height empty |
| 11 | Vouchers | Code format (3 letters + 3 digits); code-species consistency; collected value 0/1; each code collected at most once |
| 12 | Alive/dead consistency | Flag1 = 0 and Flag2 = 1 never in the same census; dead stems cannot become alive in later censuses |
| 13 | Dead — subsequent blocks | Entire census block must be empty for all censuses after a stem's death |
| 14 | Recruit — prior blocks | Entire census block must be empty for all censuses before a stem's first appearance (`n` in Flag1) |

---

## Output columns

| Column      | Description                                          |
|-------------|------------------------------------------------------|
| `excel_row` | Row number in the Excel file (includes header rows)  |
| `TreeID`    | Tree identifier from the file (blank for new plots)  |
| `census`    | Affected census, e.g. `Census 2` (`NA` for structural checks) |
| `column`    | Affected column name or position label               |
| `issue`     | Plain-English description of the problem             |

---

## Adding a new dataset type

1. Create `R/check_<type>.R` with a top-level function
   `check_<type>(file_path, sheet_name)` that returns the issues data frame.
2. Add `source("R/check_<type>.R")` near the top of `run_checks.R`.
3. Add a new case to the `switch()` call inside `run_checks()`:
   ```r
   new_type = check_new_type(file_path, sheet_name),
   ```
4. Document the new type in the table above.

---

## Dependencies

```r
install.packages(c("readxl", "dplyr", "stringr", "writexl"))
```

---

## Toward a package

This project is structured to make the transition to an R package
straightforward when the time comes:

- All logic lives in `R/` as plain functions with no side effects.
- `run_checks.R` is the only file that calls `source()` and `library()`;
  in a package these become `@import` / `Imports:` declarations.
- Each `check_*` function is independently testable (no global state).
- roxygen2-style `@param` / `@return` / `@examples` tags are already in place,
  ready for `devtools::document()`.
