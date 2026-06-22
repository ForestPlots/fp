# R/check_new_single_census.R
# Validation checks for new-plot single-census uploads.
#
# Use this checker when uploading a brand-new plot to ForestPlots for the
# first time with a single census. Do NOT use for:
#   - Existing plots  → use check_existing_one_census
#   - New plots with multiple censuses  → use check_new_multicensus
#
# File format:
#   Row 1 or 2  — column names (auto-detected by presence of "Tag No")
#   Remaining   — data rows, one per stem
#
# Column structure:
#   Flat — no front/middle/final block distinction. All census data and
#   tree identifiers are in a single set of columns.
#
# Key rules specific to this dataset type:
#   - No TreeID column (assigned by ForestPlots after upload)
#   - No reference census (first upload, nothing to compare against)
#   - Tag No must be unique across all rows
#   - LI valid range is 0–4 (includes 0; differs from other checkers)
#   - Flag1 "0" must appear alone; duplicate characters are not allowed
#   - Lianas (Family = "LIANA") are excluded before validation
#
# Internal functions use the _sc suffix to avoid name collisions with
# check_new_multicensus.R and check_existing_one_census.R when all three
# files are sourced together.
#
# Depends on: constants.R (CI_VALID, F3_VALID, F4_VALID, FLAG1_VALID_CHARS,
#             FLAG1_A_VALID, F2_G1, F2_G2, F2_G3), utils.R (is_empty,
#             new_issues, log_issue, is_valid_f2)

# ── Module-level constants ────────────────────────────────────────────────────

# All columns that must be present and non-empty
REQUIRED_COLS_SC <- c(
  "New Tag No", "Family", "Species", "original identification",
  "D", "POM", "Flag1", "Flag2", "Flag3", "Flag4"
)

# ── File loading and parsing ──────────────────────────────────────────────────

#' Read and parse a new-plot single-census upload file.
#'
#' Auto-detects whether column names are in row 1 or row 2 by looking for
#' "Tag No". Strips LIANA rows and empty rows, normalises whitespace, and
#' attaches an excel_row column.
#'
#' @param file_path  Path to the .xlsx file.
#' @param sheet_name Sheet name or index.
#' @return Named list: data (tibble), header_row (int), col_names (character).
parse_new_single_census_data <- function(file_path, sheet_name) {
  raw        <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)
  header_row <- if ("New Tag No" %in% as.character(raw[1, ])) 1L else 2L
  message("Column names detected in row ", header_row, ".")

  col_names   <- as.character(raw[header_row, ])
  data        <- raw[-(seq_len(header_row)), ]
  names(data) <- col_names

  data <- data |>
    mutate(excel_row = row_number() + header_row) |>
    filter(rowSums(!is.na(across(everything()))) > 0) |>
    # Normalise: strip whitespace and convert blank-only strings to NA
    mutate(across(where(is.character), \(x) na_if(trimws(x), "")))

  n_lianas <- sum(trimws(as.character(data$Family)) == "LIANA", na.rm = TRUE)
  data     <- data |> filter(is.na(Family) | Family != "LIANA")

  if (n_lianas > 0) {
    message(n_lianas, " LIANA row(s) excluded from validation.")
  }

  list(data = data, header_row = header_row, col_names = col_names)
}

# ── Checks ────────────────────────────────────────────────────────────────────

#' Check that all required columns exist and contain no empty values.
#'
#' Any column in REQUIRED_COLS_SC that is missing from the file produces a
#' fatal-style issue (reported as a row-1 structural problem). Existing
#' columns are then checked row by row for emptiness.
#'
#' @param data     Parsed data frame.
#' @param col_names Character vector of column names from the file.
#' @param header_row Integer row number where column names were detected.
#' @return List of issue tibbles.
check_required_fields_sc <- function(data, col_names, header_row) {
  issues <- new_issues()

  missing_cols <- setdiff(REQUIRED_COLS_SC, col_names)
  for (col in missing_cols) {
    issues <- log_issue(issues, header_row, NA, NA, col,
      paste0("Required column '", col, "' is missing from the file"))
  }

  for (col in intersect(REQUIRED_COLS_SC, names(data))) {
    bad <- which(is_empty(data[[col]]))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, col, paste0(col, " must not be empty"))
  }

  issues
}

#' Check that Tag No is unique across all rows.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_tag_no_uniqueness <- function(data) {
  issues   <- new_issues()
  tag      <- trimws(as.character(data$`New Tag No`))
  dup_tags <- tag[duplicated(tag) & !is.na(tag)]
  bad      <- which(tag %in% dup_tags)

  log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "New Tag No",
    "New Tag No is duplicated — must be unique across all rows")
}

#' Check that D, POM, Height, and Height Broken At are numeric when filled.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_numeric_fields_sc <- function(data) {
  issues <- new_issues()

  for (col in c("D", "POM", "Height", "Height Broken At")) {
    if (!col %in% names(data)) next
    raw_val <- data[[col]]
    num_val <- suppressWarnings(as.numeric(as.character(raw_val)))
    bad     <- which(!is_empty(raw_val) & is.na(num_val))
    issues  <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, col, paste0(col, " must be numeric"))
  }

  issues
}

#' Check Flag1 rules.
#'
#' Checks: valid character set, "0" must appear alone (dead stems),
#' no duplicate characters, and 'a' combination rule.
#'
#' Note: this checker has no reference census so there is no recruit-'n' rule
#' based on a prior F1 value. Flag1 = "n" alone is valid for first-upload recruits.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_flag1_sc <- function(data) {
  issues <- new_issues()
  f1     <- trimws(as.character(data$Flag1))
  has_f1 <- !is_empty(data$Flag1)

  # Valid character set: 0, a–q, s, w–z
  bad <- which(has_f1 & !grepl(FLAG1_VALID_CHARS, f1))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1",
    "Flag1 contains invalid character(s) \u2014 allowed: 0, a\u2013q, s, w\u2013z")

  # "0" must appear alone — dead stems cannot carry additional flags
  bad <- which(has_f1 & grepl("0", f1, fixed = TRUE) & nchar(f1) > 1)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1",
    "Flag1: '0' must appear alone \u2014 dead stems must not combine '0' with other characters")

  # No duplicate characters
  has_dups <- nchar(f1) != nchar(gsub("(.)(?=.*\\1)", "", f1, perl = TRUE))
  bad <- which(has_f1 & has_dups)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1", "Flag1 contains duplicate characters")

  # 'a' may only be combined with 'n' and/or 'h'
  has_a <- has_f1 & grepl("a", f1, fixed = TRUE)
  bad   <- which(has_a & !f1 %in% FLAG1_A_VALID)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1",
    "Flag1: 'a' may only be combined with 'n' and/or 'h'")

  # 'n' and/or 'h' must be accompanied by at least one other valid character
  bad <- which(has_f1 & grepl("^[nh]+$", f1))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1",
    "Flag1: 'n' and/or 'h' cannot appear alone \u2014 must be accompanied by another valid character")

  issues
}

#' Check Flag2 rules.
#'
#' When Flag1 is present and alive (not "0"), Flag2 must be a valid value.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_flag2_sc <- function(data) {
  issues  <- new_issues()
  f1      <- trimws(as.character(data$Flag1))
  f2      <- trimws(as.character(data$Flag2))
  has_f1  <- !is_empty(data$Flag1)
  f1_alive <- has_f1 & f1 != "0"

  bad <- which(f1_alive & !is_valid_f2(f2))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag2",
    paste0("Flag2 is invalid \u2014 must be '1' alone or at most one character from each group: ",
           "[abcdefghiklm] / [pqr] / [jnostuvwxyz234567]"))

  # Dead stems (Flag1 = 0): when Flag2 is filled it must still follow the group rules
  f1_zero  <- has_f1 & f1 == "0"
  f2_blank <- is_empty(data$Flag2)
  bad <- which(f1_zero & !f2_blank & !is_valid_f2(f2))
  log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag2",
    paste0("Flag2 is invalid \u2014 at most one character from each group is allowed: ",
           "[abcdefghiklm] / [pqr] / [jnostuvwxyz234567]"))
}

#' Check Flag3 and Flag4 rules.
#'
#' Both flags must be empty when Flag1 is missing or "0" (dead or absent).
#' When Flag1 indicates a living stem, both must be valid values.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_flag3_flag4_sc <- function(data) {
  issues        <- new_issues()
  f1            <- trimws(as.character(data$Flag1))
  has_f1        <- !is_empty(data$Flag1)
  dead_or_absent <- !has_f1 | f1 == "0"
  alive          <- has_f1 & f1 != "0"

  if ("Flag3" %in% names(data)) {
    f3  <- trimws(as.character(data$Flag3))
    bad <- which(dead_or_absent & !is_empty(data$Flag3))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "Flag3", "Flag3 must be empty when Flag1 is missing or '0'")

    bad <- which(alive & !f3 %in% F3_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "Flag3",
      paste0("Flag3 invalid value \u2014 must be one of: ", paste(F3_VALID, collapse = ", ")))
  }

  if ("Flag4" %in% names(data)) {
    f4  <- trimws(as.character(data$Flag4))
    bad <- which(dead_or_absent & !is_empty(data$Flag4))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "Flag4", "Flag4 must be empty when Flag1 is missing or '0'")

    bad <- which(alive & !f4 %in% F4_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "Flag4",
      paste0("Flag4 invalid value \u2014 must be one of: ", paste(F4_VALID, collapse = ", ")))
  }

  issues
}

#' Check D and POM values when Flag1 = "0" (dead).
#'
#' For dead stems, D must equal 0 and POM must equal 0.
#' Issues are reported separately for each column.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_d_pom_dead_sc <- function(data) {
  issues <- new_issues()
  if (!"D" %in% names(data) || !"POM" %in% names(data)) return(issues)

  f1  <- trimws(as.character(data$Flag1))
  d   <- suppressWarnings(as.numeric(as.character(data$D)))
  pom <- suppressWarnings(as.numeric(as.character(data$POM)))

  bad <- which(f1 == "0" & !is.na(d) & d != 0)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "D", "Flag1 = '0' (dead) \u2014 D must equal 0")

  bad <- which(f1 == "0" & !is.na(pom) & pom != 0)
  log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "POM", "Flag1 = '0' (dead) \u2014 POM must equal 0")
}

#' Check LI, CI, and CF values.
#'
#' All three fields are optional. When filled:
#'   LI — must be 0, 1, 2, 3, or 4  (note: 0 is valid here, unlike other checkers)
#'   CI — must be one of CI_VALID (5, 4, 3b, 3a, 2c, 2b, 2a, 1)
#'   CF — must be 0, 1, 2, 3, or 4
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_li_ci_cf_sc <- function(data) {
  issues <- new_issues()

  if ("LI" %in% names(data)) {
    li  <- trimws(as.character(data$LI))
    # Valid range 0–4 (0 included — differs from check_new_multicensus / check_existing_one_census)
    bad <- which(!is_empty(data$LI) & !li %in% c("0", "1", "2", "3", "4"))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "LI",
      "LI invalid value \u2014 must be empty or one of: 0, 1, 2, 3, 4")
  }

  if ("CI" %in% names(data)) {
    ci  <- trimws(as.character(data$CI))
    bad <- which(!is_empty(data$CI) & !ci %in% CI_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "CI",
      paste0("CI invalid value \u2014 must be empty or one of: ",
             paste(CI_VALID, collapse = ", ")))
  }

  if ("CF" %in% names(data)) {
    cf  <- trimws(as.character(data$CF))
    bad <- which(!is_empty(data$CF) & !cf %in% c("0", "1", "2", "3", "4"))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "CF",
      "CF invalid value \u2014 must be empty or one of: 0, 1, 2, 3, 4")
  }

  issues
}

#' Check Height and Flag5 consistency.
#'
#' Flag5 must be filled (1–6) when Height is present, and empty when Height
#' is absent.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_height_flag5_sc <- function(data) {
  issues <- new_issues()
  if (!"Height" %in% names(data) || !"Flag5" %in% names(data)) return(issues)

  h  <- trimws(as.character(data$Height))
  f5 <- trimws(as.character(data$Flag5))

  has_height <- !is_empty(data$Height)
  has_f5     <- !is_empty(data$Flag5)

  bad <- which(has_height & !has_f5)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag5",
    "Flag5 must be filled (1\u20136) when Height is present")

  bad <- which(has_height & has_f5 & !f5 %in% as.character(1:6))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag5",
    "Flag5 invalid value \u2014 must be 1, 2, 3, 4, 5, or 6")

  bad <- which(!has_height & has_f5)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag5",
    "Flag5 is filled but Height is empty \u2014 clear Flag5 or add a Height value")

  issues
}

#' Check voucher code and voucher collected rules.
#'
#' Checks: code format (3 letters + 3 digits), code-species consistency,
#' collected field only present when code present, collected value 0/1,
#' each code may have collected = 1 only once across the file.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_vouchers_sc <- function(data) {
  issues <- new_issues()
  if (!"voucher code" %in% names(data)) return(issues)

  vc     <- trimws(as.character(data$`voucher code`))
  has_vc <- !is_empty(data$`voucher code`)

  # Format: 3 letters + 3 digits, no dashes or spaces
  bad <- which(has_vc & !grepl("^[A-Za-z]{3}[0-9]{3}$", vc))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "voucher code",
    "Voucher code invalid format \u2014 must be 3 letters + 3 digits, no dashes (e.g. ABC123)")

  if ("voucher collected" %in% names(data)) {
    vcl     <- trimws(as.character(data$`voucher collected`))
    has_vcl <- !is_empty(data$`voucher collected`)

    # voucher collected requires a voucher code
    bad <- which(!has_vc & has_vcl)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "voucher collected",
      "voucher collected is filled but voucher code is empty")

    # Must be 0 or 1
    bad <- which(has_vc & has_vcl & !vcl %in% c("0", "1"))
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "voucher collected",
      "voucher collected invalid value \u2014 must be 0 or 1")

    # Each voucher code may have collected = 1 at most once
    multi_collected <- data.frame(vc = vc, vcl = vcl) |>
      filter(has_vc & vcl == "1") |>
      count(vc) |>
      filter(n > 1) |>
      pull(vc)

    bad <- which(has_vc & vcl == "1" & vc %in% multi_collected)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "voucher collected",
      "voucher collected = 1 appears more than once for the same voucher code")
  }

  # Each voucher code must map to exactly one Species
  if ("Species" %in% names(data)) {
    multi_sp <- data.frame(vc = vc, Species = data$Species) |>
      filter(has_vc) |>
      group_by(vc) |>
      summarise(sp_n = n_distinct(Species, na.rm = TRUE), .groups = "drop") |>
      filter(sp_n > 1) |>
      pull(vc)

    bad <- which(has_vc & vc %in% multi_sp)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "voucher code",
      "Voucher code is linked to more than one Species \u2014 all rows with the same code must share the same Species")
  }

  issues
}

#' Check Stem Grouping rules.
#'
#' The grouping column for this dataset type is "New stem grouping" (lowercase s).
#' Checks: value shared by at least two rows, T1 consistent within group,
#' Species consistent within group, Flag1 contains 'h' for every group member.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_stem_grouping_sc <- function(data) {
  issues <- new_issues()
  if (!"New stem grouping" %in% names(data)) return(issues)

  sg_present <- !is_empty(data$`New stem grouping`)
  if (!any(sg_present)) return(issues)

  sg_vals <- as.character(data$`New stem grouping`)
  f1      <- trimws(as.character(data$Flag1))

  # Every grouping value must appear in at least two rows
  sg_counts <- table(sg_vals[sg_present])
  unique_sg <- names(sg_counts)[sg_counts == 1]
  bad <- which(sg_present & sg_vals %in% unique_sg)
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "New stem grouping",
    "Stem grouping value appears only once \u2014 must be shared with at least one other row")

  # T1 must be the same within each group
  if ("T1" %in% names(data)) {
    bad_t1 <- data[sg_present, ] |>
      mutate(sg = sg_vals[sg_present]) |>
      group_by(sg) |>
      summarise(t1_n = n_distinct(T1, na.rm = TRUE), .groups = "drop") |>
      filter(t1_n > 1) |>
      pull(sg)
    bad <- which(sg_present & sg_vals %in% bad_t1)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "T1",
      "All rows in a Stem Grouping must share the same T1 value")
  }

  # Species must be the same within each group
  if ("Species" %in% names(data)) {
    bad_sp <- data[sg_present, ] |>
      mutate(sg = sg_vals[sg_present]) |>
      group_by(sg) |>
      summarise(sp_n = n_distinct(Species, na.rm = TRUE), .groups = "drop") |>
      filter(sp_n > 1) |>
      pull(sg)
    bad <- which(sg_present & sg_vals %in% bad_sp)
    issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
      NA, "Species",
      "All rows in a Stem Grouping must share the same Species")
  }

  # Flag1 must contain 'h' for every member of a group
  bad <- which(sg_present & !grepl("h", f1, fixed = TRUE))
  issues <- log_issue(issues, data$excel_row[bad], data$`New Tag No`[bad],
    NA, "Flag1",
    "Flag1 must contain 'h' for all members of a Stem Grouping")

  issues
}

# ── Orchestrator ──────────────────────────────────────────────────────────────

#' Run all validation checks for a new-plot single-census upload.
#'
#' @param file_path  Path to the .xlsx file.
#' @param sheet_name Sheet name or index (default: 1).
#' @return A data frame with one row per issue, sorted by excel_row.
#'   Zero rows means the file passed all checks.
check_new_single_census <- function(file_path, sheet_name = 1) {

  parsed     <- parse_new_single_census_data(file_path, sheet_name)
  data       <- parsed$data
  header_row <- parsed$header_row
  col_names  <- parsed$col_names

  message("Parsed ", nrow(data), " data rows.")

  issues <- new_issues()

  issues <- c(issues, check_required_fields_sc(data, col_names, header_row))
  issues <- c(issues, check_tag_no_uniqueness(data))
  issues <- c(issues, check_numeric_fields_sc(data))
  issues <- c(issues, check_flag1_sc(data))
  issues <- c(issues, check_flag2_sc(data))
  issues <- c(issues, check_flag3_flag4_sc(data))
  issues <- c(issues, check_d_pom_dead_sc(data))
  issues <- c(issues, check_li_ci_cf_sc(data))
  issues <- c(issues, check_height_flag5_sc(data))
  issues <- c(issues, check_vouchers_sc(data))
  issues <- c(issues, check_stem_grouping_sc(data))

  issues_df <- bind_rows(issues)
  if (nrow(issues_df) > 0) {
    issues_df <- issues_df |> arrange(excel_row, census, column)
  }
  issues_df
}
