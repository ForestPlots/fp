# R/check_existing_one_census.R
# Validation checks for existing-plot field sheets with one new census.
#
# File format:
#   Row 1 or 2  — column names (auto-detected by presence of "Tag No")
#   Remaining   — data rows, one per stem
#
# Column structure:
#   Front block   — 17 fixed columns (Tag No … Old Tree Notes)
#   Middle block  — 1 or 2 previous census columns, dated suffix \nYYYY.Y
#                   (D, POM, F1, F2, Height, F5, Height Broken At,
#                    F3, F4, LI, CI, CF, CD1, CD2, Census Notes)
#   Final block   — new census columns, no suffix on most
#                   (D, POM, Flag1, Flag2, Height, Flag5,
#                    Height Broken At, LI, CI, CF, CD1, CD2,
#                    Census Notes, Tree ID, New Tag No,
#                    New Stem Grouping, Flag3, Flag4)
#
# Key rules specific to this dataset type:
#   - Tree ID must be filled for all non-recruit stems
#   - Reference census F1 used for cross-census consistency checks
#   - Lianas (Family = "LIANA") are excluded before validation
#
# Internal functions use the check_final_* prefix for checks on the
# final census block, to avoid name collisions with check_new_multicensus.R
# when both files are sourced together.
#
# Depends on: constants.R (CI_VALID, F3_VALID, F4_VALID, FLAG1_VALID_CHARS,
#             FLAG1_A_VALID, F2_G1, F2_G2, F2_G3), utils.R

# ── Module-level constants ────────────────────────────────────────────────────

# Front block: 17 fixed columns for existing-plot field sheets
FIXED_COLS_EX <- c(
  "Tag No", "Old No", "Stem Group ID", "Main Stem Tag",
  "T1", "T2", "X", "Y",
  "Family", "Species", "Subspecies", "Variety",
  "Rec. Family", "Rec. Species", "Rec. Subspecies", "Rec. Variety",
  "Old Tree Notes"
)

# Subset that must be present — order is not enforced because column names
# are auto-detected from the file rather than validated by position.
ESSENTIAL_COLS_EX <- c(
  "Tag No", "Old No", "Stem Group ID", "Main Stem Tag",
  "T1", "T2", "X", "Y",
  "Family", "Species", "Subspecies", "Variety"
)

# Final census block column names — used for the reference-death emptiness check.
# Admin columns (Tree ID, New Tag No, New Stem Grouping) are excluded from that
# check because they may legitimately be filled even for dead-reference stems.
FINAL_COLS_EX <- c(
  "D", "POM", "Flag1", "Flag2",
  "Height", "Flag5", "Height Broken At",
  "LI", "CI", "CF", "CD1", "CD2",
  "Census Notes", "Flag3", "Flag4"
)

FINAL_ADMIN_COLS_EX <- c("Tree ID", "New Tag No", "New Stem Grouping")

# ── File loading and parsing ──────────────────────────────────────────────────

#' Read and parse an existing-plot field sheet.
#'
#' Auto-detects whether column names are in row 1 or row 2 by looking for
#' "Tag No" in each candidate row. Strips LIANA rows and empty rows, then
#' attaches an excel_row column reflecting each row's original file position.
#'
#' @param file_path  Path to the .xlsx file.
#' @param sheet_name Sheet name or index.
#' @return Named list: data (tibble), header_row (int), col_names (character).
parse_existing_data <- function(file_path, sheet_name) {
  raw        <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)
  header_row <- if ("Tag No" %in% as.character(raw[1, ])) 1L else 2L
  message("Column names detected in row ", header_row, ".")

  col_names        <- as.character(raw[header_row, ])
  data             <- raw[-(seq_len(header_row)), ]
  names(data)      <- col_names

  data <- data |>
    mutate(excel_row = row_number() + header_row) |>
    filter(rowSums(!is.na(across(everything()))) > 0) |>
    filter(is.na(Family) | trimws(as.character(Family)) != "LIANA")

  list(data = data, header_row = header_row, col_names = col_names)
}

# ── Reference block detection ─────────────────────────────────────────────────

#' Locate the most recent reference census block.
#'
#' Identifies the reference F1 values and reference POM values by finding the
#' last occurrence of columns named "F1" / "F2" (abbreviated form used in
#' middle blocks) before the final-block "Flag1" column.
#'
#' @param data Parsed data frame.
#' @return Named list: ref_f1 (character vector), ref_pom (numeric vector).
#'   ref_pom is NA when no dated POM column is found.
locate_reference_block <- function(data) {
  all_cols  <- names(data)
  flag1_idx <- which(all_cols == "Flag1")

  if (length(flag1_idx) != 1) {
    stop("Exactly one 'Flag1' column is required in the final census block.")
  }

  before_final <- all_cols[seq_len(flag1_idx - 1)]
  ref_f1_idx   <- suppressWarnings(max(which(before_final == "F1")))
  ref_f2_idx   <- suppressWarnings(max(which(before_final == "F2")))

  if (is.infinite(ref_f1_idx) || is.infinite(ref_f2_idx)) {
    stop("Reference census columns F1 / F2 not found before the final census block.")
  }

  ref_f1 <- trimws(as.character(data[[ref_f1_idx]]))

  # Last column matching "^POM\n..." pattern — the most recent reference POM
  ref_pom_col <- grep("^POM\\s*\n", before_final, value = TRUE)
  ref_pom <- if (length(ref_pom_col) > 0) {
    suppressWarnings(as.numeric(data[[ref_pom_col[length(ref_pom_col)]]]))
  } else {
    NA_real_
  }

  list(ref_f1 = ref_f1, ref_pom = ref_pom)
}

# ── Front block checks ────────────────────────────────────────────────────────

#' Check that all essential front columns are present.
#'
#' Reports one issue per missing column. Column order is not enforced here
#' because the field sheet layout uses auto-detected column names.
#'
#' @param col_names  Character vector of column names read from the file.
#' @param header_row Integer row number where column names were detected.
#' @return List of issue tibbles.
check_front_columns <- function(col_names, header_row) {
  issues <- new_issues()
  for (col in ESSENTIAL_COLS_EX) {
    if (!col %in% col_names) {
      issues <- log_issue(issues, header_row, NA, NA, col,
        paste0("Essential column '", col, "' is missing from the sheet"))
    }
  }
  issues
}

#' Check that Tag No, Family, and Species are not empty.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_required_front_fields <- function(data) {
  issues <- new_issues()
  for (col in c("Tag No", "Family", "Species")) {
    if (!col %in% names(data)) next
    bad <- which(is.na(data[[col]]) | trimws(as.character(data[[col]])) == "")
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      NA, col, paste0(col, " must not be empty"))
  }
  issues
}

#' Check Tree ID rules.
#'
#' Tree ID must be filled for all stems except recruits (Flag1 contains 'n').
#' This is the opposite of the new-plot rule, where Tree ID must be blank.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_tree_id <- function(data) {
  issues <- new_issues()
  if (!"Tree ID" %in% names(data)) return(issues)

  tid     <- trimws(as.character(data$`Tree ID`))
  recruit <- grepl("n", trimws(as.character(data$Flag1)), fixed = TRUE)
  bad     <- which((is.na(tid) | tid == "") & !recruit)

  log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    NA, "Tree ID",
    "Tree ID must not be empty unless Flag1 contains 'n' (recruit)")
}

# ── Cross-census checks ───────────────────────────────────────────────────────

#' Check the reference-death rule.
#'
#' If a stem was dead in the most recent reference census (F1 = "0"), the
#' entire final census block must be empty. Back-to-life stems are a valid
#' exception but require manual verification — the issue message prompts for
#' this rather than silently passing.
#'
#' Admin columns (Tree ID, New Tag No, New Stem Grouping) are exempt because
#' they may legitimately be filled for dead-reference stems.
#'
#' @param data      Parsed data frame.
#' @param prev_dead Logical vector: TRUE for rows where reference F1 = "0".
#' @return List of issue tibbles.
check_reference_death <- function(data, prev_dead) {
  issues <- new_issues()

  final_cols_present <- intersect(FINAL_COLS_EX, names(data))
  final_has_data <- apply(
    data[, final_cols_present, drop = FALSE], 1,
    function(r) any(!is.na(r) & trimws(as.character(r)) != "")
  )

  bad <- which(prev_dead & final_has_data)
  log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    NA, "Final block",
    paste0("Stem was dead in reference census (F1 = 0) — ",
           "verify this is a back-to-life stem; ",
           "if not, all final block columns must be empty"))
}

# ── Final census block checks ─────────────────────────────────────────────────

#' Check New Tag No rules.
#'
#' Recruits — identified by Flag1 containing 'n', or Tree ID being blank —
#' must have New Tag No filled.
#'
#' @param data    Parsed data frame.
#' @param recruit Logical vector: TRUE where Flag1 contains 'n'.
#' @param tid     Character vector of trimmed Tree ID values.
#' @return List of issue tibbles.
check_new_tag_no <- function(data, recruit, tid) {
  issues <- new_issues()
  if (!"New Tag No" %in% names(data)) return(issues)

  new_tag    <- trimws(as.character(data$`New Tag No`))
  is_recruit <- recruit | (is.na(tid) | tid == "")
  bad        <- which(is_recruit & (is.na(new_tag) | new_tag == ""))

  log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "New Tag No",
    "Stem is a recruit (Tree ID blank or Flag1 contains 'n') — New Tag No must be filled")
}

#' Check Flag1 rules for the final census block.
#'
#' Checks: not blank, valid character set, 'a' combination rule,
#' recruit 'n' rule when reference F1 was absent.
#'
#' @param data   Parsed data frame.
#' @param ref_f1 Character vector of trimmed reference-census F1 values.
#' @return List of issue tibbles.
check_final_flag1 <- function(data, ref_f1) {
  issues <- new_issues()
  f1     <- trimws(as.character(data$Flag1))
  has_f1 <- !is.na(data$Flag1) & f1 != ""

  # Flag1 must not be blank
  bad <- which(!has_f1 & ref_f1 == "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag1",
    "Stem was dead in reference census (F1 = 0) — remove this row unless it is a back-to-life stem")

  bad <- which(!has_f1 & ref_f1 != "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag1", "Flag1 is blank")

  # Valid character set
  bad <- which(has_f1 & !grepl(FLAG1_VALID_CHARS, f1))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag1",
    "Flag1 contains invalid character(s). Allowed: 0, a\u2013q, s, w\u2013z")

  # 'a' may only appear alongside 'n' or 'h'
  bad <- which(has_f1 & grepl("a", f1, fixed = TRUE) & !f1 %in% FLAG1_A_VALID)
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag1",
    "Flag1: 'a' can only be combined with 'n' or 'h' (no other letters)")

  # Recruit rule: blank reference F1 means the stem is new — must contain 'n'
  ref_blank <- is.na(ref_f1) | ref_f1 == ""
  bad <- which(ref_blank & has_f1 & f1 != "0" & !grepl("n", f1, fixed = TRUE))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag1",
    "Reference F1 is empty — Flag1 must contain 'n' for new recruits")

  issues
}

#' Check Flag2 rules for the final census block.
#'
#' Flag2 must not be blank. When Flag1 indicates an alive stem, Flag2 must be
#' a valid code (validated by is_valid_f2() from utils.R).
#'
#' @param data   Parsed data frame.
#' @param ref_f1 Character vector of trimmed reference-census F1 values.
#' @return List of issue tibbles.
check_final_flag2 <- function(data, ref_f1) {
  issues <- new_issues()
  if (!"Flag2" %in% names(data)) return(issues)

  f2       <- trimws(as.character(data$Flag2))
  f2_blank <- is.na(data$Flag2) | f2 == ""
  f1       <- trimws(as.character(data$Flag1))
  has_f1   <- !is.na(data$Flag1) & f1 != ""

  # Flag2 must not be blank
  bad <- which(f2_blank & ref_f1 == "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag2",
    "Stem was dead in reference census (F1 = 0) — remove this row unless it is a back-to-life stem")

  bad <- which(f2_blank & ref_f1 != "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag2", "Flag2 is blank")

  # Alive stems must have a valid Flag2 value
  f1_alive <- has_f1 & f1 != "0"
  bad <- which(f1_alive & !is_valid_f2(f2))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag2",
    paste0("Flag2 is invalid. Must be '1' or at most one character from each group: ",
           "[abcdefghiklm] / [pqr] / [jnostuvwxyz234567]"))

  issues
}

#' Check Flag3 and Flag4 rules for the final census block.
#'
#' Both flags must be empty when Flag1 is 0 or blank (dead or absent).
#' When Flag1 indicates a living stem, both must be valid values.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_final_flag3_flag4 <- function(data) {
  issues <- new_issues()

  f1            <- trimws(as.character(data$Flag1))
  has_f1        <- !is.na(data$Flag1) & f1 != ""
  dead_or_blank <- !has_f1 | f1 == "0"
  alive         <- has_f1 & f1 != "0"

  if ("Flag3" %in% names(data)) {
    f3  <- trimws(as.character(data$Flag3))
    bad <- which(dead_or_blank & !is.na(data$Flag3) & f3 != "")
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag3",
      "Flag3 must be empty when Flag1 is 0 or blank")

    bad <- which(alive & !f3 %in% F3_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag3",
      paste0("Flag3 invalid value — must be one of: ", paste(F3_VALID, collapse = ", ")))
  }

  if ("Flag4" %in% names(data)) {
    f4  <- trimws(as.character(data$Flag4))
    bad <- which(dead_or_blank & !is.na(data$Flag4) & f4 != "")
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag4",
      "Flag4 must be empty when Flag1 is 0 or blank")

    bad <- which(alive & !f4 %in% F4_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag4",
      paste0("Flag4 invalid value — must be one of: ", paste(F4_VALID, collapse = ", ")))
  }

  issues
}

#' Check D and POM rules for the final census block.
#'
#' Checks: D and POM must be numeric; D must not be blank; when Flag1 = 0
#' (dead), both D and POM must equal 0.
#'
#' @param data   Parsed data frame.
#' @param ref_f1 Character vector of trimmed reference-census F1 values.
#' @return List of issue tibbles.
check_final_d_pom <- function(data, ref_f1) {
  issues <- new_issues()
  if (!"D" %in% names(data) || !"POM" %in% names(data)) return(issues)

  d_raw   <- trimws(as.character(data$D))
  pom_raw <- trimws(as.character(data$POM))
  d       <- suppressWarnings(as.numeric(d_raw))
  pom     <- suppressWarnings(as.numeric(pom_raw))

  # Must be numeric
  bad <- which(!is.na(data$D) & d_raw != "" & is.na(d))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "D",
    "D contains non-numeric characters — must be a number")

  bad <- which(!is.na(data$POM) & pom_raw != "" & is.na(pom))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "POM",
    "POM contains non-numeric characters — must be a number")

  # D must not be blank
  d_blank <- is.na(d) & (is.na(data$D) | d_raw == "")
  bad <- which(d_blank & ref_f1 == "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "D",
    "Stem was dead in reference census (F1 = 0) — remove this row unless it is a back-to-life stem")

  bad <- which(d_blank & ref_f1 != "0")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "D", "D is blank")

  # Flag1 = 0 (dead) → D and POM must both equal 0
  f1_curr <- trimws(as.character(data$Flag1))
  bad <- which(f1_curr == "0" & !is.na(d) & !is.na(pom) & (d != 0 | pom != 0))
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "D / POM",
    "Flag1 = 0 (dead) — D and POM must both be 0")

  issues
}

#' Check Height, Flag5, and Height Broken At for the final census block.
#'
#' Height and Height Broken At must be numeric when filled.
#' Flag5 must be filled (1–6) when Height is present, and empty when Height
#' is absent.
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_final_height <- function(data) {
  issues <- new_issues()

  if ("Height" %in% names(data) && "Flag5" %in% names(data)) {
    height_raw     <- trimws(as.character(data$Height))
    height_num     <- suppressWarnings(as.numeric(height_raw))
    height_present <- !is.na(data$Height) & height_raw != ""

    bad <- which(height_present & is.na(height_num))
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Height",
      "Height contains non-numeric characters — must be a number")

    f5_raw     <- trimws(as.character(data$Flag5))
    f5_present <- !is.na(data$Flag5) & f5_raw != ""

    bad <- which(height_present & (!f5_present | !f5_raw %in% as.character(1:6)))
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag5",
      "Height is filled — Flag5 must be a value from 1 to 6")

    bad <- which(!height_present & f5_present)
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Flag5",
      "Flag5 is filled but Height is empty — Flag5 must be blank when Height is blank")
  }

  if ("Height Broken At" %in% names(data)) {
    hba_raw <- trimws(as.character(data$`Height Broken At`))
    hba_num <- suppressWarnings(as.numeric(hba_raw))
    bad <- which(!is.na(data$`Height Broken At`) & hba_raw != "" & is.na(hba_num))
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "Height Broken At",
      "Height Broken At contains non-numeric characters — must be a number")
  }

  issues
}

#' Check LI, CI, and CF values for the final census block.
#'
#' All three fields are optional but, when filled, must contain a value from
#' the defined valid sets (shared with check_new_multicensus.R via constants.R):
#'   LI — 1, 2, 3, or 4
#'   CI — one of CI_VALID (5, 4, 3b, 3a, 2c, 2b, 2a, 1)
#'   CF — 0, 1, 2, 3, or 4
#'
#' @param data Parsed data frame.
#' @return List of issue tibbles.
check_final_li_ci_cf <- function(data) {
  issues <- new_issues()

  if ("LI" %in% names(data)) {
    li  <- trimws(as.character(data$LI))
    bad <- which(!is.na(data$LI) & li != "" & !li %in% c("1", "2", "3", "4"))
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "LI",
      "LI invalid value — must be empty or 1, 2, 3, or 4")
  }

  if ("CI" %in% names(data)) {
    ci  <- trimws(as.character(data$CI))
    bad <- which(!is.na(data$CI) & ci != "" & !ci %in% CI_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "CI",
      paste0("CI invalid value — must be empty or one of: ",
             paste(CI_VALID, collapse = ", ")))
  }

  if ("CF" %in% names(data)) {
    cf  <- trimws(as.character(data$CF))
    bad <- which(!is.na(data$CF) & cf != "" & !cf %in% c("0", "1", "2", "3", "4"))
    issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
      "New census", "CF",
      "CF invalid value — must be empty or 0, 1, 2, 3, or 4")
  }

  issues
}

#' Check POM / Flag4 cross-census consistency.
#'
#' POM changed from the reference census → Flag4 must be 60.
#' Flag4 = 60 but POM unchanged → Flag4 should not be 60.
#'
#' @param data    Parsed data frame.
#' @param ref_pom Numeric vector of reference-census POM values.
#' @return List of issue tibbles.
check_pom_change <- function(data, ref_pom) {
  issues <- new_issues()
  if (!"Flag4" %in% names(data) || all(is.na(ref_pom))) return(issues)

  pom_curr     <- suppressWarnings(as.numeric(data$POM))
  f4           <- trimws(as.character(data$Flag4))
  both_present <- !is.na(pom_curr) & !is.na(ref_pom)

  bad <- which(both_present & pom_curr != ref_pom & f4 != "60")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag4",
    "POM changed from reference census — Flag4 must be 60")

  bad <- which(both_present & pom_curr == ref_pom & f4 == "60")
  issues <- log_issue(issues, data$excel_row[bad], data$`Tag No`[bad],
    "New census", "Flag4",
    "Flag4 = 60 but POM matches reference census — Flag4 should not be 60")

  issues
}

# ── Orchestrator ──────────────────────────────────────────────────────────────

#' Run all validation checks for an existing-plot one-census upload.
#'
#' @param file_path  Path to the .xlsx file.
#' @param sheet_name Sheet name or index (default: 1).
#' @return A data frame with one row per issue, sorted by excel_row.
#'   Zero rows means the file passed all checks.
check_existing_one_census <- function(file_path, sheet_name = 1) {

  # Parse data and capture layout metadata
  parsed     <- parse_existing_data(file_path, sheet_name)
  data       <- parsed$data
  header_row <- parsed$header_row
  col_names  <- parsed$col_names

  message("Parsed ", nrow(data), " data rows.")

  issues <- new_issues()

  # Structure: are all essential columns present?
  issues <- c(issues, check_front_columns(col_names, header_row))

  # Locate the reference census block — stop early if not found
  ref     <- locate_reference_block(data)
  ref_f1  <- ref$ref_f1
  ref_pom <- ref$ref_pom

  # Derived vectors reused across multiple checks
  prev_dead <- !is.na(ref_f1) & ref_f1 == "0"
  recruit   <- grepl("n", trimws(as.character(data$Flag1)), fixed = TRUE)
  tid       <- trimws(as.character(data$`Tree ID`))

  # Front block checks
  issues <- c(issues, check_required_front_fields(data))
  issues <- c(issues, check_tree_id(data))

  # Cross-census checks
  issues <- c(issues, check_reference_death(data, prev_dead))

  # Final census block checks
  issues <- c(issues, check_new_tag_no(data, recruit, tid))
  issues <- c(issues, check_final_flag1(data, ref_f1))
  issues <- c(issues, check_final_flag2(data, ref_f1))
  issues <- c(issues, check_final_flag3_flag4(data))
  issues <- c(issues, check_final_d_pom(data, ref_f1))
  issues <- c(issues, check_final_height(data))
  issues <- c(issues, check_final_li_ci_cf(data))
  issues <- c(issues, check_pom_change(data, ref_pom))

  # Compile and sort
  issues_df <- bind_rows(issues)
  if (nrow(issues_df) > 0) {
    issues_df <- issues_df |> arrange(excel_row, census, column)
  }
  issues_df
}
