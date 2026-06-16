# R/check_taxonomy.R
# Taxonomy validation for ForestPlots upload files.
#
# Validates species names and family assignments against a legacy reference
# taxonomy (APG family names) and the TNRS (Taxonomic Name Resolution Service)
# via the BIOMASS package. Validation runs at the unique Species × Family
# level, not row-by-row.
#
# Species–Family pairs already present in the legacy dataset are accepted
# immediately without TNRS queries (legacy-first strategy).
#
# Usage (after sourcing run_checks.R):
#
#   tax <- check_taxonomy(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1",
#     legacy_path  = "path/to/Taxonomy_reference.csv"
#   )
#
#   tax$issues       # per-issue table (Species, Family, code, description, …)
#   tax$per_species  # one row per species, issue codes collapsed
#
#   # Export both tables to Excel
#   check_taxonomy(..., export_path = "taxonomy_issues.xlsx")
#
# Depends on: BIOMASS, dplyr, readxl, readr, writexl, stringr, tools

library(BIOMASS)
library(dplyr)
library(readxl)
library(readr)
library(writexl)
library(stringr)
library(tools)

# ── Issue accumulation ────────────────────────────────────────────────────────
# Prefixed tax_ to avoid shadowing new_issues() and log_issue() from utils.R,
# which have a different signature (row-level vs species-level).

tax_new_issues <- function() {
  tibble(
    Species           = character(),
    Family            = character(),
    Issue_Code        = character(),
    Issue_Description = character(),
    Severity          = character(),
    Suggested_Action  = character()
  )
}

tax_log_issue <- function(issues, species, family,
                          code, desc, severity,
                          action = NA_character_) {
  if (length(species) == 0) return(issues)
  bind_rows(
    issues,
    tibble(
      Species           = as.character(species),
      Family            = as.character(family),
      Issue_Code        = code,
      Issue_Description = desc,
      Severity          = severity,
      Suggested_Action  = action
    )
  )
}

# ── File loading ──────────────────────────────────────────────────────────────

#' Read a taxonomy Excel or CSV file, auto-detecting whether column names are
#' in row 1 or row 2.
#'
#' @param path    Path to the file (.xlsx, .xls, or .csv).
#' @param sheet   Sheet name or index (Excel only; ignored for CSV).
#' @param key_col Column name expected in the header row, used to decide
#'   whether headers are in row 1 or row 2.
#' @return A tibble with correct column names.
read_taxonomy_file <- function(path, sheet = NULL, key_col) {
  ext <- tolower(file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    raw    <- read_excel(path, sheet = sheet, col_names = FALSE)
    header <- if (key_col %in% as.character(raw[1, ])) 1L else 2L
    read_excel(path, sheet = sheet, skip = header - 1L)
  } else {
    raw    <- read_csv(path, col_names = FALSE, show_col_types = FALSE, n_max = 2)
    header <- if (key_col %in% as.character(raw[1, ])) 1L else 2L
    read_csv(path, skip = header - 1L, show_col_types = FALSE)
  }
}

# ── TNRS validation block ─────────────────────────────────────────────────────

#' Run TNRS checks for a subset of species records.
#'
#' Shared helper used for both \emph{Genus indet} (Case C) and fully named
#' (regular) species. Calls \code{correctTaxo()} to test whether names are
#' resolvable, then validates families via \code{getTaxonomy()} for any species
#' absent from the legacy dataset.
#'
#' @param species_df  Tibble with at minimum columns \code{Species} and
#'   \code{Family}. Rows must already have \code{indet_case} assigned.
#' @param legacy_data Reference taxonomy tibble (\code{FullSpeciesName},
#'   \code{FamilyAPGName}).
#' @return A taxonomy issues tibble (same structure as \code{tax_new_issues()}).
run_tnrs_checks <- function(species_df, legacy_data) {
  issues <- tax_new_issues()
  if (nrow(species_df) == 0) return(issues)

  tnrs_raw <- correctTaxo(species_df$Species)
  tnrs <- bind_cols(species_df, tnrs_raw) |>
    mutate(
      nameModified = as.logical(nameModified),
      updated_name = paste(genusCorrected, speciesCorrected)
    )

  # TNRS did not modify the name — cannot confirm validity
  unresolved <- filter(tnrs, !nameModified)
  issues <- tax_log_issue(
    issues, unresolved$Species, unresolved$Family,
    "TNRS_CHECK_REQUIRED",
    "TNRS did not modify the name — taxonomy must be manually checked",
    "WARNING"
  )

  updated_in     <- filter(tnrs, nameModified,  updated_name %in% legacy_data$FullSpeciesName)
  updated_not_in <- filter(tnrs, nameModified, !updated_name %in% legacy_data$FullSpeciesName)

  issues <- tax_log_issue(
    issues, updated_in$Species, updated_in$Family,
    "TNRS_USE_UPDATED",
    paste0("TNRS updated name to '", updated_in$updated_name,
           "', found in legacy data — use updated taxonomy"),
    "INFO",
    paste0("Replace with: ", updated_in$updated_name)
  )
  issues <- tax_log_issue(
    issues, updated_not_in$Species, updated_not_in$Family,
    "TNRS_CHECK_UPDATED",
    paste0("TNRS updated name to '", updated_not_in$updated_name,
           "', not found in legacy data — taxonomy must be manually checked"),
    "WARNING",
    paste0("Verify: ", updated_not_in$updated_name)
  )

  # Family check via TNRS for species absent from legacy under both names
  not_in_legacy <- filter(tnrs,
    !Species      %in% legacy_data$FullSpeciesName,
    !updated_name %in% legacy_data$FullSpeciesName
  )
  if (nrow(not_in_legacy) > 0) {
    tnrs_tax <- getTaxonomy(unique(not_in_legacy$genusCorrected))
    not_in_legacy_fam <- not_in_legacy |>
      left_join(
        tnrs_tax |> select(inputGenus, family),
        by = c("genusCorrected" = "inputGenus")
      )
    fam_found <- filter(not_in_legacy_fam, !is.na(family))
    fam_none  <- filter(not_in_legacy_fam,  is.na(family))

    issues <- tax_log_issue(
      issues, fam_found$Species, fam_found$Family,
      "FAMILY_NOT_IN_LEGACY_TNRS",
      paste0("Not in legacy dataset; TNRS suggests family '",
             fam_found$family, "'"),
      "WARNING",
      paste0("Consider using family: ", fam_found$family)
    )
    issues <- tax_log_issue(
      issues, fam_none$Species, fam_none$Family,
      "FAMILY_REVIEW_REQUIRED",
      "Family not found in legacy dataset or TNRS — review the taxonomy",
      "WARNING"
    )
  }

  issues
}

# ── Main function ─────────────────────────────────────────────────────────────

#' Validate species taxonomy for a ForestPlots upload file.
#'
#' Checks species names and family assignments against a legacy reference
#' taxonomy and TNRS. Taxonomy validation runs at the unique
#' Species \eqn{\times} Family level, not row-by-row.
#'
#' @param dataset_type Character. One of \code{"new_multicensus"},
#'   \code{"new_single_census"}, or \code{"single_recensus"}. Used to locate
#'   the correct header row in the upload file.
#' @param file_path   Character. Path to the upload \code{.xlsx} file.
#' @param sheet_name  Sheet name or index. Default: 1.
#' @param legacy_path Character. Path to the reference taxonomy file
#'   (\code{.xlsx} or \code{.csv}). Must contain columns
#'   \code{FullSpeciesName} and \code{FamilyAPGName}.
#' @param legacy_sheet Sheet name or index for Excel legacy files. Default:
#'   \code{NULL} (first sheet).
#' @param export_path Optional character path. If supplied, \code{issues} and
#'   \code{per_species} are written to this \code{.xlsx} file. Default:
#'   \code{NULL}.
#'
#' @return A named list, returned invisibly:
#'   \describe{
#'     \item{\code{issues}}{Tibble with one row per flagged case. Columns:
#'       \code{Species}, \code{Family}, \code{Issue_Code},
#'       \code{Issue_Description}, \code{Severity}, \code{Suggested_Action}.}
#'     \item{\code{per_species}}{Tibble with one row per species. All issue
#'       codes for that species are collapsed into a single
#'       semicolon-separated string.}
#'   }
#'   Zero rows in \code{issues} means the file passed all taxonomy checks.
#'
#' @examples
#' \dontrun{
#' source("run_checks.R")
#'
#' tax <- check_taxonomy(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "plot001",
#'   legacy_path  = "data/Taxonomy_reference.csv"
#' )
#'
#' tax$issues       # detailed issue table
#' tax$per_species  # one row per species, codes collapsed
#'
#' # Export both tables to Excel
#' check_taxonomy(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   legacy_path  = "data/Taxonomy_reference.csv",
#'   export_path  = "data/my_upload_taxonomy_issues.xlsx"
#' )
#' }
check_taxonomy <- function(dataset_type,
                            file_path,
                            sheet_name   = 1,
                            legacy_path,
                            legacy_sheet = NULL,
                            export_path  = NULL) {

  # ── Load data ───────────────────────────────────────────────────────────────

  user_data <- read_taxonomy_file(file_path, sheet_name, "Species") |>
    select(any_of(c("Tree ID", "original identification", "Species", "Family")))

  legacy_data <- read_taxonomy_file(legacy_path, legacy_sheet, "FullSpeciesName") |>
    select(FullSpeciesName, FamilyAPGName)

  # ── Required columns ────────────────────────────────────────────────────────

  missing_cols <- setdiff(c("Species", "Family"), names(user_data))
  if (length(missing_cols) > 0) {
    stop(
      "Required column(s) not found in '", basename(file_path), "': ",
      paste(missing_cols, collapse = ", "), ".\n",
      "Ensure the file contains 'Species' and 'Family' columns."
    )
  }

  issues <- tax_new_issues()

  # ── Check 0a: blank Species ─────────────────────────────────────────────────

  blank_species <- filter(user_data, is.na(Species) | trimws(Species) == "")
  issues <- tax_log_issue(
    issues,
    rep(NA_character_, nrow(blank_species)),
    blank_species$Family,
    "SPECIES_MISSING",
    "Species is missing",
    "ERROR",
    "Provide a species name or use 'Indet indet' if unknown"
  )

  # ── Check 0b: inconsistent Species–Family combinations ──────────────────────
  # Catches the same species assigned to different families within the dataset,
  # which distinct() would otherwise silently hide.

  multi_family <- user_data |>
    filter(!is.na(Species), trimws(Species) != "",
           !is.na(Family),  trimws(Family)  != "") |>
    group_by(Species) |>
    summarise(n_families = n_distinct(Family), .groups = "drop") |>
    filter(n_families > 1)

  issues <- tax_log_issue(
    issues,
    multi_family$Species,
    rep(NA_character_, nrow(multi_family)),
    "INCONSISTENT_FAMILY",
    paste0("Species is assigned to ", multi_family$n_families,
           " different Family values within this dataset"),
    "ERROR",
    "Ensure every row for this species has the same Family value"
  )

  # ── Build unique Species × Family table ─────────────────────────────────────
  # Work at species level from here. Both rows of an inconsistent pair are kept
  # so that subsequent checks can report against both.

  species_unique <- user_data |>
    filter(!is.na(Species), trimws(Species) != "") |>
    distinct(Species, Family) |>
    mutate(
      genus      = word(Species, 1),
      indet_case = case_when(
        Family == "Indet" & Species == "Indet indet" ~ "A",
        Species == "Indet indet"                     ~ "B",
        grepl("^[A-Z][a-z]+ indet$", Species)        ~ "C",
        grepl("indet", Species, ignore.case = TRUE) |
          grepl("indet", Family,   ignore.case = TRUE) ~ "invalid",
        TRUE                                           ~ "none"
      )
    )

  # ── Check 1: species word count ─────────────────────────────────────────────
  # A valid binomial has exactly two words. Extra words indicate subspecies,
  # variety, hybrid markers, author abbreviations, or stray whitespace —
  # all of which cause silent failures in legacy and TNRS string matching.

  extra_words <- species_unique |>
    mutate(word_count = str_count(trimws(Species), "\\S+")) |>
    filter(word_count > 2)

  issues <- tax_log_issue(
    issues,
    extra_words$Species,
    extra_words$Family,
    "SPECIES_EXTRA_WORDS",
    paste0("Species contains ", extra_words$word_count,
           " words; expected a two-word binomial (Genus + epithet)"),
    "WARNING",
    "Review for subspecies, variety, hybrid marker, author abbreviation, or extra whitespace"
  )

  # ── Check 2: invalid indet format ───────────────────────────────────────────

  bad_indet <- filter(species_unique, indet_case == "invalid")
  issues <- tax_log_issue(
    issues,
    bad_indet$Species,
    bad_indet$Family,
    "INVALID_INDET_FORMAT",
    "Non-canonical indet format",
    "ERROR",
    "Use: Family = 'Indet', Species = 'Indet indet' or '<Genus> indet'"
  )

  # ── Legacy short-circuit ────────────────────────────────────────────────────
  # Exact Species × Family matches in the legacy dataset are accepted.
  # Remaining species proceed to family and TNRS checks.

  legacy_check <- species_unique |>
    left_join(
      legacy_data |> mutate(legacy_ok = TRUE),
      by = c("Species" = "FullSpeciesName", "Family" = "FamilyAPGName")
    ) |>
    mutate(legacy_ok = coalesce(legacy_ok, FALSE))

  to_validate <- filter(legacy_check, !legacy_ok)

  # ── Check 3: family validation ──────────────────────────────────────────────
  # Single consolidated pass: for each species in to_validate, look up its
  # expected family in the legacy dataset and flag missing or wrong values.
  # (Previously split across two code blocks, causing double-flagging.)

  legacy_lookup <- legacy_data |>
    distinct(FullSpeciesName, .keep_all = TRUE) |>
    rename(legacy_family = FamilyAPGName)

  family_check <- to_validate |>
    filter(indet_case != "A") |>
    left_join(legacy_lookup, by = c("Species" = "FullSpeciesName"))

  # Species name found in legacy — check that the user's family matches
  species_in_legacy <- filter(family_check, !is.na(legacy_family))

  fam_missing_known <- filter(species_in_legacy,
                              is.na(Family) | trimws(Family) == "")
  issues <- tax_log_issue(
    issues,
    fam_missing_known$Species,
    fam_missing_known$Family,
    "FAMILY_MISSING",
    paste0("Family is missing; legacy dataset suggests '",
           fam_missing_known$legacy_family, "'"),
    "ERROR",
    paste0("Set Family to: ", fam_missing_known$legacy_family)
  )

  fam_wrong <- filter(species_in_legacy,
                      !is.na(Family), trimws(Family) != "",
                      Family != legacy_family)
  issues <- tax_log_issue(
    issues,
    fam_wrong$Species,
    fam_wrong$Family,
    "FAMILY_MISMATCH_REFERENCE",
    paste0("Family '", fam_wrong$Family,
           "' does not match legacy dataset (expected '",
           fam_wrong$legacy_family, "')"),
    "WARNING",
    paste0("Replace Family with: ", fam_wrong$legacy_family)
  )

  # Species name not found in legacy at all — suggest family via TNRS
  species_not_in_legacy <- filter(family_check, is.na(legacy_family))
  fam_missing_unknown   <- filter(species_not_in_legacy,
                                  is.na(Family) | trimws(Family) == "")

  if (nrow(fam_missing_unknown) > 0) {
    indet_blank <- filter(fam_missing_unknown, trimws(Species) == "Indet indet")
    issues <- tax_log_issue(
      issues, indet_blank$Species, indet_blank$Family,
      "FAMILY_MISSING",
      "Family is missing; recommended family is 'Indet' for 'Indet indet'",
      "ERROR",
      "Set Family to: Indet"
    )

    other_blank <- filter(fam_missing_unknown, trimws(Species) != "Indet indet")
    if (nrow(other_blank) > 0) {
      tnrs_fam    <- getTaxonomy(unique(other_blank$genus))
      other_blank <- other_blank |>
        left_join(tnrs_fam |> select(inputGenus, family),
                  by = c("genus" = "inputGenus"))

      fam_found <- filter(other_blank, !is.na(family))
      fam_none  <- filter(other_blank,  is.na(family))

      issues <- tax_log_issue(
        issues, fam_found$Species, fam_found$Family,
        "FAMILY_MISSING",
        paste0("Family is missing; not in legacy dataset — TNRS suggests '",
               fam_found$family, "'"),
        "ERROR",
        paste0("Set Family to: ", fam_found$family)
      )
      issues <- tax_log_issue(
        issues, fam_none$Species, fam_none$Family,
        "FAMILY_MISSING",
        "Family is missing; species not found in legacy dataset or TNRS — review the taxonomy",
        "ERROR"
      )
    }
  }

  # ── Check 4a: Case A — fully unknown (Indet indet, Family = Indet) ──────────
  # Accepted as-is; no further checks needed.

  # ── Check 4b: Case B — Indet indet with non-standard family ─────────────────

  case_B <- filter(to_validate, indet_case == "B")
  if (nrow(case_B) > 0) {
    bad_fam_B <- filter(case_B, !Family %in% legacy_data$FamilyAPGName)
    issues <- tax_log_issue(
      issues, bad_fam_B$Species, bad_fam_B$Family,
      "FAMILY_MISMATCH_REFERENCE",
      "Family not found in legacy taxonomy",
      "WARNING",
      "Check family spelling or authority"
    )
  }

  # ── Check 4c: Case C — Genus indet species ──────────────────────────────────

  case_C  <- filter(to_validate, indet_case == "C")
  issues  <- bind_rows(issues, run_tnrs_checks(case_C, legacy_data))

  # ── Check 4d: regular (fully named) species ─────────────────────────────────

  regular <- filter(to_validate, indet_case == "none")
  issues  <- bind_rows(issues, run_tnrs_checks(regular, legacy_data))

  # ── Compile output ──────────────────────────────────────────────────────────

  issues <- arrange(issues, Severity, Species)

  per_species <- issues |>
    group_by(Species, Family) |>
    summarise(
      Issue_Codes  = paste(unique(Issue_Code), collapse = "; "),
      Max_Severity = case_when(
        any(Severity == "ERROR")   ~ "ERROR",
        any(Severity == "WARNING") ~ "WARNING",
        TRUE                       ~ "INFO"
      ),
      .groups = "drop"
    ) |>
    arrange(Max_Severity, Species)

  if (nrow(issues) == 0) {
    message("No issues found — taxonomy passed all checks.")
  } else {
    message(nrow(issues), " issue(s) flagged across ",
            n_distinct(issues$Species), " species.")
  }

  if (!is.null(export_path)) {
    write_xlsx(
      list(Issues = issues, Issues_Per_Species = per_species),
      path = export_path
    )
    message("Taxonomy issues written to: ", export_path)
  }

  invisible(list(issues = issues, per_species = per_species))
}
