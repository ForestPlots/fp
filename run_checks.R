# run_checks.R
# ============================================================
# Main entry point for ForestPlots upload validation.
#
# Usage:
#   source("run_checks.R")
#
#   issues <- run_checks(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1"
#   )
#
#   issues          # inspect in RStudio viewer
#   nrow(issues)    # 0 = passed all checks
#
#   # Export results to Excel
#   run_checks("new_multicensus", "path/to/upload.xlsx",
#              export_path = "validation_issues.xlsx")
#
#   # Plot stem X/Y locations coloured by subplot (T1 or T2)
#   plot_stem_locations(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1"
#   )
#
#   # Coordinates recorded with the RAINFOR zig-zag protocol
#   plot_stem_locations("single_recensus", "path/to/file.xlsx",
#                       subplot_col = "T2",
#                       coord_scale = "rainfor",
#                       x_breaks    = seq(0, 100, by = 20),
#                       y_breaks    = seq(0, 100, by = 20))
#
#   # Supported coord_scale values (default: "plot"):
#   #   "plot"          — XY already in full-plot space
#   #   "rainfor"       — RAINFOR zig-zag, column-by-column
#   #   "rainfor-north" — subplot space, always facing north
#   #   "rainfor-east"  — RAINFOR zig-zag, row-by-row
#
#   # Taxonomy validation (requires legacy reference taxonomy file)
#   tax <- check_taxonomy(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1",
#     legacy_path  = "path/to/Taxonomy_reference.csv"
#   )
#
#   tax$issues       # per-issue table
#   tax$per_species  # one row per species, issue codes collapsed
#
#   # Export taxonomy issues to Excel
#   check_taxonomy(..., export_path = "taxonomy_issues.xlsx")
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

source("R/constants.R")
source("R/utils.R")
source("R/fill_coord.R")
source("R/check_new_multicensus.R")
source("R/check_single_recensus.R")
source("R/check_new_single_census.R")
source("R/check_stem_locations.R")
source("R/check_taxonomy.R")


#' Validate a ForestPlots upload Excel file.
#'
#' Dispatches to the appropriate check function based on dataset_type and
#' returns a data frame of all validation issues found.
#'
#' @param dataset_type Character. Type of upload to validate against.
#'   Supported values:
#'   \describe{
#'     \item{"new_multicensus"}{New plots (no existing ForestPlots ID)
#'       with two or more census periods.}
#'     \item{"new_single_census"}{New plots (no existing ForestPlots ID)
#'       with a single census.}
#'     \item{"single_recensus"}{Existing plots with one new census,
#'       submitted via a filled field sheet.}
#'   }
#' @param file_path   Character. Path to the .xlsx file to validate.
#' @param sheet_name  Sheet name or index passed to readxl. Default: 1.
#' @param export_path Optional character path. If supplied, the issues table
#'   is written to this .xlsx file. Default: NULL (no export).
#'
#' @return A data frame with columns: excel_row, TreeID, census, column, issue.
#'   Zero rows means the file passed all checks. Returned invisibly so the
#'   console is not flooded when sourcing; assign the result to inspect it.
#'
#' @examples
#' \dontrun{
#' source("run_checks.R")
#'
#' # New plot — multiple censuses
#' issues <- run_checks(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "plot001"
#' )
#'
#' # New plot — single census
#' issues <- run_checks(
#'   dataset_type = "new_single_census",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "Sheet1"
#' )
#'
#' # Existing plot — field sheet with one new census
#' issues <- run_checks(
#'   dataset_type = "single_recensus",
#'   file_path    = "data/field_sheet.xlsx",
#'   sheet_name   = "Field Sheet"
#' )
#'
#' issues  # view in RStudio
#'
#' # Export to Excel for sharing with field teams
#' run_checks(
#'   "new_multicensus", "data/my_upload.xlsx",
#'   export_path = "data/my_upload_issues.xlsx"
#' )
#' }
run_checks <- function(dataset_type, file_path, sheet_name = 1, export_path = NULL) {

  result <- switch(
    dataset_type,
    new_multicensus     = check_new_multicensus(file_path, sheet_name),
    new_single_census   = check_new_single_census(file_path, sheet_name),
    single_recensus = check_single_recensus(file_path, sheet_name),
    stop("Unknown dataset_type: '", dataset_type, "'. ",
         "Supported types: 'new_multicensus', 'new_single_census', 'single_recensus'")
  )

  if (nrow(result) == 0) {
    message("No issues found — file passed all checks.")
  } else {
    message(nrow(result), " issue(s) found. Inspect the returned data frame.")
  }

  if (!is.null(export_path)) {
    write_xlsx(list(`Validation Issues` = result), path = export_path)
    message("Issues written to: ", export_path)
  }

  invisible(result)
}
