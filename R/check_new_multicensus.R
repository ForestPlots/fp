# R/check_new_multicensus.R
# Validation checks for new-plot multi-census ForestPlots uploads.
#
# Key rules specific to this dataset type:
#   - TreeID must be BLANK (assigned by ForestPlots after upload)
#   - File must contain at least two census blocks
#
# Depends on: constants.R, utils.R

# ── File loading ──────────────────────────────────────────────────────────────

#' Read the raw Excel file without column-name parsing.
#' @param file_path Path to the .xlsx file.
#' @param sheet_name Sheet name or index.
#' @return A tibble of raw cell values (all columns character/numeric as read).
read_fp_excel <- function(file_path, sheet_name) {
  read_excel(file_path, sheet = sheet_name, col_names = FALSE)
}

# ── Row 1: header structure ───────────────────────────────────────────────────

#' Check row 1 (filler / census date header row).
#'
#' Validates: A1 label, presence and positions of "Census No:" headers,
#' correct spacing between blocks, and 4-digit year in each header.
#'
#' @param raw Raw tibble from read_fp_excel().
#' @return Named list: issues (list of tibbles), n_censuses (int),
#'   census_header_cols (int vector of column positions).
check_row1_headers <- function(raw) {
  row1   <- as.character(raw[1, ])
  issues <- new_issues()

  if (is.na(row1[1]) || !grepl("Tree information", row1[1], ignore.case = TRUE)) {
    issues <- log_issue(issues, 1, NA, NA, "A1",
      paste0('Expected "Tree information (bold=required fields)", found: "', row1[1], '"'))
  }

  census_header_cols <- which(!is.na(row1) & grepl("Census No:", row1, ignore.case = TRUE))
  n_censuses <- length(census_header_cols)

  if (n_censuses < 2) {
    issues <- log_issue(issues, 1, NA, NA, "Row 1",
      paste0("File must have at least Census No: 1 AND Census No: 2 headers to be multi-census. ",
             "Found ", n_censuses, " census header(s)."))
  } else {
    if (census_header_cols[1] != CENSUS_START) {
      issues <- log_issue(issues, 1, NA, NA, paste0("Row 1, col ", census_header_cols[1]),
        paste0("Census No: 1 header expected at column ", CENSUS_START,
               " but found at column ", census_header_cols[1]))
    }

    for (i in 2:n_censuses) {
      gap <- census_header_cols[i] - census_header_cols[i - 1]
      if (gap != N_CENSUS) {
        issues <- log_issue(issues, 1, NA, NA, paste0("Row 1, col ", census_header_cols[i]),
          paste0("Census No: ", i, " header is ", gap, " columns after Census No: ", i - 1,
                 " (expected ", N_CENSUS, ")"))
      }
    }

    for (i in seq_len(n_censuses)) {
      val <- row1[census_header_cols[i]]
      if (!grepl("Date:\\s*\\d{4}", val)) {
        issues <- log_issue(issues, 1, NA, NA, paste0("Row 1, col ", census_header_cols[i]),
          paste0("Census No: ", i, ' header is missing a 4-digit year after "Date:". Found: "', val, '"'))
      }
    }
  }

  list(issues = issues, n_censuses = n_censuses, census_header_cols = census_header_cols)
}

# ── Row 2: column names ───────────────────────────────────────────────────────

#' Check row 2 (column names row).
#'
#' Verifies that the 13 fixed columns and all per-census block columns
#' match the expected names exactly.
#'
#' @param raw        Raw tibble from read_fp_excel().
#' @param n_censuses Number of census blocks detected in row 1.
#' @return List of issue tibbles.
check_row2_colnames <- function(raw, n_censuses) {
  row2   <- as.character(raw[2, ])
  issues <- new_issues()

  for (i in seq_along(FIXED_COLS)) {
    found <- if (i <= length(row2)) trimws(row2[i]) else NA_character_
    if (is.na(found) || found != FIXED_COLS[i]) {
      issues <- log_issue(issues, 2, NA, NA, paste0("Column ", i),
        paste0('Fixed column ', i, ': expected "', FIXED_COLS[i], '", found: "',
               ifelse(is.na(found), "(missing)", found), '"'))
    }
  }

  for (c in seq_len(n_censuses)) {
    offset <- N_FIXED + (c - 1) * N_CENSUS
    for (j in seq_along(CENSUS_COLS)) {
      pos      <- offset + j
      expected <- CENSUS_COLS[j]
      found    <- if (pos <= length(row2)) trimws(row2[pos]) else NA_character_
      if (is.na(found) || found != expected) {
        issues <- log_issue(issues, 2, NA, paste0("Census ", c), paste0("Column ", pos),
          paste0('Census ', c, ', position ', pos, ': expected "', expected, '", found: "',
                 ifelse(is.na(found), "(missing)", found), '"'))
      }
    }
  }

  issues
}

# ── Data parsing ──────────────────────────────────────────────────────────────

#' Parse rows 3+ into a tidy data frame with named columns.
#'
#' Census columns are suffixed _c1, _c2, ... to disambiguate blocks.
#' Entirely empty rows are dropped. An excel_row column records each row's
#' original position in the file (1-based, including the two header rows).
#'
#' @param raw        Raw tibble from read_fp_excel().
#' @param n_censuses Number of census blocks.
#' @return Tibble with one row per stem record plus an excel_row column.
parse_fp_data <- function(raw, n_censuses) {
  all_col_names <- c(
    FIXED_COLS,
    unlist(lapply(seq_len(n_censuses), \(c) paste0(CENSUS_COLS, "_c", c)))
  )

  n_expected_cols <- length(all_col_names)
  data_raw        <- raw[-(1:2), seq_len(min(n_expected_cols, ncol(raw)))]
  colnames(data_raw) <- all_col_names[seq_len(ncol(data_raw))]

  data <- data_raw |>
    filter(rowSums(!is.na(across(everything()))) > 0)

  data$excel_row <- which(
    rowSums(!is.na(raw[-(1:2), seq_len(ncol(data_raw))])) > 0
  ) + 2

  data
}

# ── Data checks ───────────────────────────────────────────────────────────────

#' Section 3 & 4: Required fields.
#'
#' Checks: TreeID blank, Family/Species not empty, Tag No not empty,
#' D/POM/Flag1/Flag2 not empty in any census block that has data.
#'
#' @param data           Parsed data frame from parse_fp_data().
#' @param n_censuses     Number of census blocks.
#' @param block_has_data List of logical vectors (one per census) from
#'   census_block_has_data(); used to skip empty blocks.
#' @return List of issue tibbles.
check_required_fields <- function(data, n_censuses, block_has_data) {
  issues <- new_issues()

  # TreeID must be blank — assigned by ForestPlots after upload
  tree_id_filled <- !is.na(data$TreeID) & trimws(as.character(data$TreeID)) != ""
  bad <- which(tree_id_filled)
  issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
    NA, "TreeID",
    "TreeID must be blank for a new plot upload — TreeIDs are assigned by ForestPlots after upload")

  for (col in c("Family", "Species")) {
    bad <- which(is.na(data[[col]]) | trimws(as.character(data[[col]])) == "")
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      NA, col, paste0(col, " must not be empty"))
  }

  bad <- which(is.na(data$`Tag No`) | trimws(as.character(data$`Tag No`)) == "")
  issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
    NA, "Tag No", "Tag No must not be empty")

  for (c in seq_len(n_censuses)) {
    active <- block_has_data[[c]]
    for (col in c("D", "POM", "Flag1", "Flag2")) {
      col_c <- paste0(col, "_c", c)
      if (!col_c %in% names(data)) next
      bad <- which(active & (is.na(data[[col_c]]) | trimws(as.character(data[[col_c]])) == ""))
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), col_c,
        paste0(col, " must not be empty when census data is present (Census ", c, ")"))
    }
  }

  issues
}

#' Section 5: Stem Grouping rules.
#'
#' Checks: value not unique, T1 consistent within group, Species consistent,
#' Flag1 contains 'h' for every group member in every census.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_stem_grouping <- function(data, n_censuses) {
  issues <- new_issues()

  sg_present <- !is.na(data$`Stem Grouping`) &
    trimws(as.character(data$`Stem Grouping`)) != ""
  if (!any(sg_present)) return(issues)

  sg_vals <- as.character(data$`Stem Grouping`)

  # Value must appear in at least two rows
  sg_counts <- table(sg_vals[sg_present])
  unique_sg <- names(sg_counts)[sg_counts == 1]
  bad <- which(sg_present & sg_vals %in% unique_sg)
  issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
    NA, "Stem Grouping",
    "Stem Grouping value appears only once — must be shared with at least one other row")

  # T1 must be consistent within each group
  sg_t1 <- data[sg_present, ] |>
    mutate(sg = sg_vals[sg_present]) |>
    group_by(sg) |>
    summarise(t1_n = n_distinct(T1, na.rm = TRUE), .groups = "drop") |>
    filter(t1_n > 1)
  bad <- which(sg_present & sg_vals %in% sg_t1$sg)
  issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
    NA, "T1",
    "All rows in a Stem Grouping must share the same T1 value")

  # Species must be consistent within each group
  sg_sp <- data[sg_present, ] |>
    mutate(sg = sg_vals[sg_present]) |>
    group_by(sg) |>
    summarise(sp_n = n_distinct(Species, na.rm = TRUE), .groups = "drop") |>
    filter(sp_n > 1)
  bad <- which(sg_present & sg_vals %in% sg_sp$sg)
  issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
    NA, "Species",
    "All rows in a Stem Grouping must share the same Species")

  # Flag1 must contain 'h' for all group members in every census
  for (c in seq_len(n_censuses)) {
    f1_col <- paste0("Flag1_c", c)
    if (!f1_col %in% names(data)) next
    f1  <- trimws(as.character(data[[f1_col]]))
    bad <- which(sg_present & !grepl("h", f1, fixed = TRUE))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f1_col,
      paste0("Flag1 must contain 'h' for all members of a Stem Grouping (Census ", c, ")"))
  }

  issues
}

#' Section 6: Flag1 rules.
#'
#' Checks: valid characters only, 'a' combination rule, recruit 'n' rule.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_flag1 <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    f1_col <- paste0("Flag1_c", c)
    if (!f1_col %in% names(data)) next
    f1        <- trimws(as.character(data[[f1_col]]))
    has_value <- !is.na(data[[f1_col]]) & f1 != ""

    bad <- which(has_value & !grepl(FLAG1_VALID_CHARS, f1))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f1_col,
      paste0("Flag1 contains invalid character(s). Allowed: 0, a–q, s, w–z (Census ", c, ")"))

    has_a <- has_value & grepl("a", f1, fixed = TRUE)
    bad   <- which(has_a & !f1 %in% FLAG1_A_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f1_col,
      paste0("Flag1: 'a' can only be combined with 'n' or 'h' (no other letters) (Census ", c, ")"))

    # 'n' and/or 'h' must be accompanied by at least one other valid character
    bad <- which(has_value & grepl("^[nh]+$", f1))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f1_col,
      paste0("Flag1: 'n' and/or 'h' cannot appear alone \u2014 must be accompanied by another valid character (Census ", c, ")"))

    if (c > 1) {
      f1_prev     <- trimws(as.character(data[[paste0("Flag1_c", c - 1)]]))
      prev_blank  <- is.na(data[[paste0("Flag1_c", c - 1)]]) | f1_prev == ""
      bad <- which(prev_blank & has_value & f1 != "0" & !grepl("n", f1, fixed = TRUE))
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), f1_col,
        paste0("Flag1: stem was absent in Census ", c - 1,
               " — Flag1 should contain 'n' (recruit) in Census ", c))
    }
  }

  issues
}

#' Section 7: Flag2 rules.
#'
#' Checks: blank when Flag1 blank; valid value when Flag1 alive.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_flag2 <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    f1_col <- paste0("Flag1_c", c)
    f2_col <- paste0("Flag2_c", c)
    if (!f2_col %in% names(data)) next

    f1       <- trimws(as.character(data[[f1_col]]))
    f2       <- trimws(as.character(data[[f2_col]]))
    f1_na    <- is.na(data[[f1_col]]) | f1 == ""
    f1_alive <- !f1_na & f1 != "0"

    bad <- which(f1_na & (!is.na(data[[f2_col]]) & f2 != ""))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f2_col,
      paste0("Flag2 must be blank when Flag1 is blank (Census ", c, ")"))

    bad <- which(f1_alive & !is_valid_f2(f2))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f2_col,
      paste0("Flag2 is invalid. Must be '1' or at most one character from each group: ",
             "[abcdefghiklm] / [pqr] / [jnostuvwxyz234567] (Census ", c, ")"))
  }

  issues
}

#' Section 8: Flag3 rules.
#'
#' Checks: empty when dead, filled (0–6) when alive.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_flag3 <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    f1_col <- paste0("Flag1_c", c)
    f3_col <- paste0("Flag3_c", c)
    if (!f3_col %in% names(data)) next

    f1       <- trimws(as.character(data[[f1_col]]))
    f3       <- trimws(as.character(data[[f3_col]]))
    f1_zero  <- !is.na(data[[f1_col]]) & f1 == "0"
    f1_alive <- !is.na(data[[f1_col]]) & f1 != "0" & f1 != ""

    bad <- which(f1_zero & !is.na(data[[f3_col]]) & f3 != "")
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f3_col,
      paste0("Flag3 must be empty when Flag1 = 0 (dead) (Census ", c, ")"))

    bad <- which(f1_alive & (is.na(data[[f3_col]]) | f3 == ""))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f3_col,
      paste0("Flag3 must be filled when Flag1 is not 0 (Census ", c, ")"))

    bad <- which(f1_alive & !is.na(data[[f3_col]]) & f3 != "" & !f3 %in% as.character(0:6))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f3_col,
      paste0("Flag3 invalid value — must be 0, 1, 2, 3, 4, 5, or 6 (Census ", c, ")"))
  }

  issues
}

#' Section 9: Flag4 rules.
#'
#' Checks: empty when dead, valid value when alive, Flag4 ≠ 0 when D0 ≠ D,
#' Flag4 = 60 when POM changed since previous census.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_flag4 <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    f1_col  <- paste0("Flag1_c", c)
    f4_col  <- paste0("Flag4_c", c)
    d0_col  <- paste0("D0_c", c)
    d_col   <- paste0("D_c", c)
    pom_col <- paste0("POM_c", c)
    if (!f4_col %in% names(data)) next

    f1       <- trimws(as.character(data[[f1_col]]))
    f4       <- trimws(as.character(data[[f4_col]]))
    f1_zero  <- !is.na(data[[f1_col]]) & f1 == "0"
    f1_alive <- !is.na(data[[f1_col]]) & f1 != "0" & f1 != ""

    bad <- which(f1_zero & !is.na(data[[f4_col]]) & f4 != "")
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f4_col,
      paste0("Flag4 must be empty when Flag1 = 0 (dead) (Census ", c, ")"))

    bad <- which(f1_alive & (is.na(data[[f4_col]]) | f4 == ""))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f4_col,
      paste0("Flag4 must be filled when Flag1 is not 0 (Census ", c, ")"))

    bad <- which(f1_alive & !is.na(data[[f4_col]]) & f4 != "" & !f4 %in% F4_VALID)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f4_col,
      paste0("Flag4 invalid value — must be one of: ",
             paste(F4_VALID, collapse = ", "), " (Census ", c, ")"))

    if (d0_col %in% names(data) && d_col %in% names(data)) {
      d0  <- suppressWarnings(as.numeric(data[[d0_col]]))
      d   <- suppressWarnings(as.numeric(data[[d_col]]))
      bad <- which(!is.na(d0) & !is.na(d) & d0 != d & f4 == "0")
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), f4_col,
        paste0("Flag4 = 0 but D0 \u2260 D — Flag4 should not be 0 when the diameter has changed (Census ", c, ")"))
    }

    if (c > 1) {
      pom_prev_col <- paste0("POM_c", c - 1)
      if (pom_prev_col %in% names(data) && pom_col %in% names(data)) {
        pom_curr <- suppressWarnings(as.numeric(data[[pom_col]]))
        pom_prev <- suppressWarnings(as.numeric(data[[pom_prev_col]]))
        bad <- which(!is.na(pom_curr) & !is.na(pom_prev) & pom_curr != pom_prev & f4 != "60")
        issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
          paste0("Census ", c), f4_col,
          paste0("POM changed from Census ", c - 1, " to Census ", c, " — Flag4 must be 60"))
      }
    }
  }

  issues
}

#' Section 10: LI, CI, CF value range checks.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_li_ci_cf <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    li_col <- paste0("LI_c", c)
    ci_col <- paste0("CI_c", c)
    cf_col <- paste0("CF_c", c)

    if (li_col %in% names(data)) {
      li  <- trimws(as.character(data[[li_col]]))
      bad <- which(!is.na(data[[li_col]]) & li != "" & !li %in% c("1", "2", "3", "4"))
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), li_col,
        paste0("LI invalid value — must be empty or 1, 2, 3, or 4 (Census ", c, ")"))
    }

    if (ci_col %in% names(data)) {
      ci  <- trimws(as.character(data[[ci_col]]))
      bad <- which(!is.na(data[[ci_col]]) & ci != "" & !ci %in% CI_VALID)
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), ci_col,
        paste0("CI invalid value — must be empty or one of: ",
               paste(CI_VALID, collapse = ", "), " (Census ", c, ")"))
    }

    if (cf_col %in% names(data)) {
      cf  <- trimws(as.character(data[[cf_col]]))
      bad <- which(!is.na(data[[cf_col]]) & cf != "" & !cf %in% c("0", "1", "2", "3", "4"))
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), cf_col,
        paste0("CF invalid value — must be empty or 0, 1, 2, 3, or 4 (Census ", c, ")"))
    }
  }

  issues
}

#' Section 11: Flag5 / Height consistency.
#'
#' Checks: Flag5 filled (1–6) when Height present; Flag5 empty when Height empty.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_flag5_height <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    h_col  <- paste0("Height_c", c)
    f5_col <- paste0("Flag5_c", c)
    if (!f5_col %in% names(data) || !h_col %in% names(data)) next

    h          <- trimws(as.character(data[[h_col]]))
    f5         <- trimws(as.character(data[[f5_col]]))
    has_height <- !is.na(data[[h_col]])  & h  != ""
    has_f5     <- !is.na(data[[f5_col]]) & f5 != ""

    bad <- which(has_height & !has_f5)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f5_col,
      paste0("Flag5 must be filled (1\u20136) when Height is present (Census ", c, ")"))

    bad <- which(has_height & has_f5 & !f5 %in% as.character(1:6))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f5_col,
      paste0("Flag5 invalid value — must be 1, 2, 3, 4, 5, or 6 (Census ", c, ")"))

    bad <- which(!has_height & has_f5)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), f5_col,
      paste0("Flag5 is filled but Height is empty — clear Flag5 or add a Height value (Census ", c, ")"))
  }

  issues
}

#' Section 12: Voucher code and voucher collected rules.
#'
#' Checks: code format (3 letters + 3 digits), code-species consistency,
#' collected field only present when code present, collected value 0/1,
#' each code collected at most once.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_vouchers <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    vc_col  <- paste0("voucher code_c", c)
    vcl_col <- paste0("voucher collected_c", c)
    if (!vc_col %in% names(data)) next

    vc  <- trimws(as.character(data[[vc_col]]))
    vcl <- trimws(as.character(data[[vcl_col]]))

    has_vc  <- !is.na(data[[vc_col]])  & vc  != ""
    has_vcl <- !is.na(data[[vcl_col]]) & vcl != ""

    bad <- which(has_vc & !grepl("^[A-Za-z]{3}[0-9]{3}$", vc))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), vc_col,
      paste0("Voucher code invalid format — must be 3 letters + 3 digits, no dashes (e.g. ABC123) (Census ", c, ")"))

    vc_species <- data |>
      filter(has_vc) |>
      mutate(vc_val = vc[has_vc]) |>
      group_by(vc_val) |>
      summarise(sp_n = n_distinct(Species, na.rm = TRUE), .groups = "drop") |>
      filter(sp_n > 1)
    bad <- which(has_vc & vc %in% vc_species$vc_val)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), vc_col,
      paste0("Voucher code is linked to more than one Species — ",
             "all occurrences of the same code must have the same Species (Census ", c, ")"))

    bad <- which(!has_vc & has_vcl)
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), vcl_col,
      paste0("voucher collected is filled but voucher code is empty (Census ", c, ")"))

    bad <- which(has_vc & has_vcl & !vcl %in% c("0", "1"))
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), vcl_col,
      paste0("voucher collected invalid value — must be 0 or 1 (Census ", c, ")"))

    vc_multi_collected <- data |>
      filter(has_vc & vcl == "1") |>
      mutate(vc_val = vc[has_vc & vcl == "1"]) |>
      group_by(vc_val) |>
      summarise(n = n(), .groups = "drop") |>
      filter(n > 1)
    bad <- which(has_vc & vc %in% vc_multi_collected$vc_val & vcl == "1")
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), vcl_col,
      paste0("voucher collected = 1 appears more than once for the same voucher code — ",
             "only one collection per code is allowed (Census ", c, ")"))
  }

  issues
}

#' Section 13: Alive / dead consistency.
#'
#' Checks: Flag1 = 0 and Flag2 = 1 in the same census;
#' stem dead in census N but Flag2 = 1 in any later census.
#'
#' @inheritParams check_required_fields
#' @return List of issue tibbles.
check_alive_dead <- function(data, n_censuses) {
  issues <- new_issues()

  for (c in seq_len(n_censuses)) {
    f1_col <- paste0("Flag1_c", c)
    f2_col <- paste0("Flag2_c", c)
    if (!f1_col %in% names(data) || !f2_col %in% names(data)) next

    f1  <- trimws(as.character(data[[f1_col]]))
    f2  <- trimws(as.character(data[[f2_col]]))
    bad <- which(f1 == "0" & f2 == "1")
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c), paste0("Flag1 / Flag2_c", c),
      paste0("Flag1 = 0 (dead) but Flag2 = 1 (alive) — ",
             "a stem cannot be both dead and alive (Census ", c, ")"))
  }

  if (n_censuses > 1) {
    for (c in 2:n_censuses) {
      f1_prev_col <- paste0("Flag1_c", c - 1)
      f2_curr_col <- paste0("Flag2_c", c)
      if (!f1_prev_col %in% names(data) || !f2_curr_col %in% names(data)) next

      f1_prev <- trimws(as.character(data[[f1_prev_col]]))
      f2_curr <- trimws(as.character(data[[f2_curr_col]]))
      bad     <- which(f1_prev == "0" & f2_curr == "1")
      issues  <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", c), f2_curr_col,
        paste0("Dead-to-alive: stem was dead (Flag1 = 0) in Census ", c - 1,
               " but appears alive (Flag2 = 1) in Census ", c))
    }
  }

  issues
}

#' Section 14: Dead stem — subsequent census blocks must be empty.
#'
#' Once Flag1 = 0 in census N, the entire census block for every later census
#' (New Tag Number → voucher collected) must be blank.
#'
#' @param data           Parsed data frame.
#' @param n_censuses     Number of census blocks.
#' @param block_has_data List of logical vectors from census_block_has_data().
#' @return List of issue tibbles.
check_dead_subsequent <- function(data, n_censuses, block_has_data) {
  issues <- new_issues()
  if (n_censuses <= 1) return(issues)

  for (c in 2:n_censuses) {
    prior_dead <- Reduce("|", lapply(seq_len(c - 1), function(prev_c) {
      f1_col <- paste0("Flag1_c", prev_c)
      if (!f1_col %in% names(data)) return(rep(FALSE, nrow(data)))
      trimws(as.character(data[[f1_col]])) == "0"
    }))

    bad <- which(prior_dead & block_has_data[[c]])
    issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
      paste0("Census ", c),
      paste0("Census ", c, " block (cols ",
             N_FIXED + (c - 1) * N_CENSUS + 1, "\u2013",
             N_FIXED + c * N_CENSUS, ")"),
      paste0("Stem was dead before Census ", c,
             " — entire Census ", c,
             " block (New Tag Number \u2192 voucher collected) must be empty"))
  }

  issues
}

#' Section 15: Recruit — prior census blocks must be empty.
#'
#' If Flag1 contains 'n' in census N (first appearance), every census block
#' before N must be entirely blank.
#'
#' @param data           Parsed data frame.
#' @param n_censuses     Number of census blocks.
#' @param block_has_data List of logical vectors from census_block_has_data().
#' @return List of issue tibbles.
check_recruit_prior <- function(data, n_censuses, block_has_data) {
  issues <- new_issues()
  if (n_censuses <= 1) return(issues)

  for (c in 2:n_censuses) {
    f1_col <- paste0("Flag1_c", c)
    if (!f1_col %in% names(data)) next
    is_recruit <- grepl("n", trimws(as.character(data[[f1_col]])), fixed = TRUE)

    for (prev_c in seq_len(c - 1)) {
      bad <- which(is_recruit & block_has_data[[prev_c]])
      issues <- log_issue(issues, data$excel_row[bad], data$TreeID[bad],
        paste0("Census ", prev_c),
        paste0("Census ", prev_c, " block (cols ",
               N_FIXED + (prev_c - 1) * N_CENSUS + 1, "\u2013",
               N_FIXED + prev_c * N_CENSUS, ")"),
        paste0("Stem is recruited in Census ", c,
               " (Flag1 contains 'n') — Census ", prev_c,
               " block (New Tag Number \u2192 voucher collected) must be empty"))
    }
  }

  issues
}

# ── Orchestrator ──────────────────────────────────────────────────────────────

#' Run all validation checks for a new-plot multi-census upload.
#'
#' @param file_path  Path to the .xlsx file.
#' @param sheet_name Sheet name or index (default: 1).
#' @return A data frame with one row per issue, sorted by excel_row.
#'   Zero rows means the file passed all checks.
check_new_multicensus <- function(file_path, sheet_name = 1) {
  raw    <- read_fp_excel(file_path, sheet_name)
  issues <- new_issues()

  # Row 1 — header structure (also extracts n_censuses)
  row1_result <- check_row1_headers(raw)
  issues      <- c(issues, row1_result$issues)
  n_censuses  <- row1_result$n_censuses

  # Row 2 — column names
  issues <- c(issues, check_row2_colnames(raw, n_censuses))

  # Cannot safely parse data if the header structure is broken
  if (n_censuses < 2) {
    warning("Stopping early: fewer than 2 census headers detected. Fix header issues first.")
    return(bind_rows(issues))
  }

  # Parse data rows (row 3 onward)
  data <- parse_fp_data(raw, n_censuses)
  message("Parsed ", nrow(data), " data rows across ", n_censuses, " censuses.")

  # Precompute census block occupancy once — reused across several checks
  block_has_data <- lapply(seq_len(n_censuses), census_block_has_data, df = data)

  # Data-level checks
  issues <- c(issues, check_required_fields(data, n_censuses, block_has_data))
  issues <- c(issues, check_stem_grouping(data, n_censuses))
  issues <- c(issues, check_flag1(data, n_censuses))
  issues <- c(issues, check_flag2(data, n_censuses))
  issues <- c(issues, check_flag3(data, n_censuses))
  issues <- c(issues, check_flag4(data, n_censuses))
  issues <- c(issues, check_li_ci_cf(data, n_censuses))
  issues <- c(issues, check_flag5_height(data, n_censuses))
  issues <- c(issues, check_vouchers(data, n_censuses))
  issues <- c(issues, check_alive_dead(data, n_censuses))
  issues <- c(issues, check_dead_subsequent(data, n_censuses, block_has_data))
  issues <- c(issues, check_recruit_prior(data, n_censuses, block_has_data))

  # Compile and sort
  issues_df <- bind_rows(issues)
  if (nrow(issues_df) > 0) {
    issues_df <- issues_df |> arrange(excel_row, census, column)
  }
  issues_df
}
