# run_checks.R
# ============================================================
# Check whether your dataset follows all the ForestPlot rules prior to upload.
#
# Usage:
#   source("run_checks.R")
#
#   issues <- run_checks(
#     dataset_type = "new_multicensus", # the other option is existing_single_census
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
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

source("R/constants.R")
source("R/utils.R")
source("R/check_new_multicensus.R")
source("R/check_existing_single_census.R")


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
#'     \item{"existing_single_census"}{Existing plots with one new census,
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
#' issues <- run_checks(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "plot001"
#' )
#'
#' issues
#'
#' # Export to Excel for sharing with field teams
#' run_checks(
#'   "new_multicensus", "data/my_upload.xlsx",
#'   export_path = "data/my_upload_issues.xlsx"
#' )
#'
#' # Existing plot — field sheet with one new census
#' issues <- run_checks(
#'   dataset_type = "existing_single_census",
#'   file_path    = "data/field_sheet.xlsx",
#'   sheet_name   = "Field Sheet"
#' )
#' }
run_checks <- function(dataset_type, file_path, sheet_name = 1, export_path = NULL) {

  result <- switch(
    dataset_type,
    new_multicensus      = check_new_multicensus(file_path, sheet_name),
    existing_single_census  = check_existing_single_census(file_path, sheet_name),
    stop("Unknown dataset_type: '", dataset_type, "'. ",
         "Supported types: 'new_multicensus', 'existing_single_census'")
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
