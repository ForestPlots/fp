# ForestPlots Upload Checker (`fp`)

A lightweight R-based validation tool for ForestPlots Excel upload files.

This repository provides a suite of checks that help ensure data quality before submission. It detects formatting errors, missing values, and inconsistencies, and returns a structured report for easy review.

---

## Overview

The checker runs a set of validation rules on an Excel file and produces a tidy data frame of issues. Each row corresponds to a single problem.

Output includes:
- Excel row number
- Tree identifier (if applicable)
- Census information
- Column name
- Plain-English description of the issue

---

## Main Function

### `run_checks()`

This is the primary entry point for the tool. It:
- Loads the dataset
- Selects the appropriate validation routine
- Runs all checks
- Returns a tidy data frame of issues

---

## Quick Start

```r
# Load the checker
source("run_checks.R")

# Run validation
issues <- run_checks(
  dataset_type = "new_multicensus",
  file_path = "path/to/your/upload.xlsx",
  sheet_name = "plot001"  # or sheet index (e.g. 1)
)

# View results
issues
nrow(issues)  # 0 = no issues found

# Export to Excel
run_checks(
  dataset_type = "new_multicensus",
  file_path = "path/to/your/upload.xlsx",
  export_path = "path/to/output_issues.xlsx"
)
```

---

## Supported Dataset Types

| dataset_type        | Description |
|--------------------|-------------|
| new_multicensus    | New plots with 2+ censuses |
| new_single_census  | New plots with 1 census |
| single_recensus    | Existing plots with 1 new census |

---

## Example Output

```r
# A tibble: 3 × 5
excel_row TreeID census   column   issue
12        T001   Census 2 DBH      Missing value
15        T005   NA       PlotID   Invalid format
22        T002   Census 1 Species  Unknown species code
```

---

## Project Structure

```
fp/
├── R/
│   ├── constants.R
│   ├── utils.R
│   ├── check_new_multicensus.R
│   ├── check_new_single_census.R
│   └── check_single_recensus.R
├── run_checks.R
└── README.md
```

- `run_checks.R`: Entry point
- `R/check_*`: Individual validation modules
- `utils.R`: Shared helper functions
- `constants.R`: Valid values and definitions

---

## Installation

```r
# Install dependencies
install.packages(c("readxl", "dplyr", "stringr", "writexl"))

# Clone the repository
git clone https://github.com/ForestPlots/fp.git

# Set working directory
setwd("fp")
```

---

## Adding a New Dataset Type

1. Create a new file:
   `R/check_<type>.R`

2. Define:
```r
check_<type> <- function(file_path, sheet_name) {
  # return issues dataframe
}
```

3. Add to `run_checks.R`:
```r
"new_type" = check_new_type(file_path, sheet_name)
```

4. Document in README.

---

## Status

- ✅ Core validation functionality implemented
- ✅ Modular check system
- 🚧 Expansion into broader ForestPlots tools

---

## About

"fp" stands for ForestPlots tools. This repository provides utilities for validating and working with ForestPlots datasets.
