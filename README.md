# ForestPlots Upload Checker

**Work in progress** - this repository is evolving into a broader set of tools 
for working with ForestPlots data.

At present, it focuses on **validating Excel upload files before submission.**

The checker runs a suite of rules against an upload file and returns a tidy
data frame of issues — one row per problem — including:Excel row number, 
Tree identifier, Affected census (if applicable), Column name, and Plain-English 
description of the issue.

---

## Project structure

```
fp/
├── R/
│   ├── constants.R               # Column definitions and valid-value sets
│   ├── utils.R                   # Shared helpers (log_issue, is_valid_f2, …)
│   ├── check_new_multicensus.R   # Checks for new-plot multi-census uploads
│   ├── check_new_single_census.R # Checks for new-plot single census uploads
│   └── check_single_recensus.R   # Checks for new single census uploads for existing plots
├── run_checks.R                  # Entry point — source this file
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

| `dataset_type`     | Description                                                                |
|--------------------|------------------------------------------------------------------------|
| `new_multicensus`  | New plots not existing in ForestPlots, containing two or more censuses |
| `new_single_census`| New plots not existing in ForestPlots, containing one census           |
| `single_recensus`  | Existing plots already uploaded to ForestPlots, containing one census  |

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
