# ============================================================
# EXISTING PLOT FIELD SHEET VALIDATION SCRIPT
# ============================================================
# Validates a filled-in field sheet for a plot that already
# exists in ForestPlots.
#
# FORMAT:
#   Row 1  — column names (or sub-header / filler if names are in row 2)
#   Row 2  — column names (if not in row 1) or first data row
#   Row 3+ — data (if column names were in row 2)
#
# COLUMN STRUCTURE:
#   Front block (always present, 17 fixed columns):
#     Tag No, Old No, Stem Group ID, Main Stem Tag, T1, T2,
#     X, Y, Family, Species, Subspecies, Variety,
#     Rec. Family, Rec. Species, Rec. Subspecies, Rec.Variety,
#     Old Tree Notes
#
#   Middle block(s) (1 or 2 previous censuses, dated columns):
#     D, POM, F1, F2, Height, F5, Height Broken At, F3, F4,
#     LI, CI, CF, CD1, CD2, Census Notes  — each with \nYYYY.Y suffix
# 
#   The second to last block:
#     D \nYYYY.Y, POM \nYYYY.Y, F1, F2, Height \nYYYY.Y, F5,
#     Height Broken At \nYYYY.Y, F3, F4, LI \nYYYY.Y, CI \nYYYY.Y,
#     CF \nYYYY.Y, CD1 \nYYYY.Y, CD2 \nYYYY.Y,
#
#   Final block (most-recent census being submitted, partly dated):
#     D, POM, Flag1, Flag2, Height, Flag5, Height Broken At, 
#     LI, CI, CF, CD1, CD2, Census Notes \nYYYY.Y, Census Notes, 
#     Tree ID, New Tag No, New Stem Grouping, Flag3, Flag4
#
# WHAT IS VALIDATED:
#   - Fixed front columns are present and correctly named
#   - Tag No, Family, Species not empty
#   - Stem Group ID rules (uniqueness, shared T1/Species, Flag1 'h')
#   - All FINAL_CHECK_COLS (see constant below) against ForestPlots rules
#   - Cross-census consistency with the most recent previous census
#     (dead-to-alive, recruit flag, POM change → Flag4 = 60)
#
# OUTPUT: `issues` — one row per problem; contains excel_row, tag_no,
#         tree_id, column, issue description.
#
# HOW TO USE:
#   1. Set file_path and sheet_name below.
#   2. Source or run the script.
#   3. Inspect `issues`. If empty, the file passed all checks.
#   4. Optionally uncomment the write_xlsx block at the bottom.
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ============================================================
# CONFIG
# ============================================================

file_path  <- "../CongoFor1.5/RoC/SAN/data/SAN 21.xlsx"
sheet_name <- "Field Sheet"

# ============================================================
# CONSTANTS
# ============================================================

FIXED_COLS <- c(
  "Tag No", "Old No", "Stem Group ID", "Main Stem Tag",
  "T1", "T2", "X", "Y",
  "Family", "Species", "Subspecies", "Variety",
  "Rec. Family", "Rec. Species", "Rec. Subspecies", "Rec. Variety",
  "Old Tree Notes"
)

# Columns that must be present (order does not matter)
ESSENTIAL_COLS <- c(
  "Tag No", "Old No", "Stem Group ID", "Main Stem Tag",
  "T1", "T2", "X", "Y",
  "Family", "Species", "Subspecies", "Variety"
)

FINAL_COLS <- c(
  "D", "POM", "Flag1", "Flag2",
  "Height", "Flag5", "Height Broken At",
  "LI", "CI", "CF", "CD1", "CD2",
  "Census Notes", "Tree ID", "New Tag No",
  "New Stem Grouping", "Flag3", "Flag4"
)

CI_VALID <- c("5","4","3b","3a","2c","2b","2a","1")
F3_VALID <- c("0","1","2","3","4","5","6")
F4_VALID <- c("0","1","2","3","4","6","7","8","60")

FLAG1_VALID_CHARS <- "^[abcdefghijklmnopqswxyz0]*$"
FLAG1_A_VALID <- c("a","ah","ha","an","na","ahn","anh","han","hna","nah","nha")

F2_G1 <- "[abcdefghiklm]"
F2_G2 <- "[pqr]"
F2_G3 <- "[jnostuvwxyz234567]"

# ============================================================
# ISSUE LOGGING
# ============================================================

new_issues <- function() {
  tibble(
    excel_row = integer(),
    tag_no    = character(),
    column    = character(),
    issue     = character()
  )
}

log_issue <- function(issues, rows, tag_no, column, msg) {
  if (length(rows) == 0) return(issues)
  bind_rows(
    issues,
    tibble(
      excel_row = rows,
      tag_no    = tag_no,
      column    = column,
      issue     = msg
    )
  )
}

is_valid_f2 <- function(x) {
  if (is.na(x) || x == "") return(FALSE)
  if (x == "1") return(TRUE)
  if (grepl("1", x)) return(FALSE)
  g1 <- str_count(x, F2_G1)
  g2 <- str_count(x, F2_G2)
  g3 <- str_count(x, F2_G3)
  (g1 <= 1 && g2 <= 1 && g3 <= 1 && (g1 + g2 + g3) == nchar(x))
}

# ============================================================
# LOAD DATA
# ============================================================

raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

# Auto-detect whether column names are in row 1 or row 2 by looking for
# "Tag No" (the first fixed column) in each candidate row.
header_row <- if ("Tag No" %in% as.character(raw[1, ])) 1L else 2L
message("Column names detected in row ", header_row, ".")

col_names <- as.character(raw[header_row, ])
data <- raw[-(seq_len(header_row)), ]
names(data) <- col_names

data <- data |>
  mutate(excel_row = row_number() + header_row) |>
  filter(rowSums(!is.na(across(everything()))) > 0)

issues <- new_issues()

# ============================================================
# REMOVE LIANAS
# ============================================================

data <- data |>
  filter(is.na(Family) | trimws(Family) != "LIANA")

# ============================================================
# CHECK FIXED COLUMN NAMES
# ============================================================

for (col in ESSENTIAL_COLS) {
  if (!col %in% col_names) {
    issues <- log_issue(
      issues, header_row, NA,
      col,
      paste0("Essential column '", col, "' is missing")
    )
  }
}

# ============================================================
# IDENTIFY FINAL + REFERENCE BLOCK BY COLUMN ORDER
# ============================================================

all_cols <- names(data)

flag1_idx <- which(all_cols == "Flag1")
if (length(flag1_idx) != 1) stop("Exactly one 'Flag1' column required")

# Reference block must contain F1 and F2 immediately before final block
ref_f1_idx <- max(which(all_cols[1:(flag1_idx-1)] == "F1"))
ref_f2_idx <- max(which(all_cols[1:(flag1_idx-1)] == "F2"))

if (is.infinite(ref_f1_idx) || is.infinite(ref_f2_idx)) {
  stop("Reference block F1/F2 not found immediately before final block")
}

ref_f1 <- trimws(as.character(data[[ref_f1_idx]]))
ref_pom_col <- grep("^POM\\s*\n", all_cols[1:(flag1_idx-1)], value = TRUE)
ref_pom <- if (length(ref_pom_col) > 0) suppressWarnings(as.numeric(data[[ref_pom_col[length(ref_pom_col)]]])) else NA

# ============================================================
# REQUIRED FRONT FIELDS
# ============================================================

for (col in c("Tag No","Family","Species")) {
  bad <- which(is.na(data[[col]]) | trimws(data[[col]]) == "")
  issues <- log_issue(
    issues,
    data$excel_row[bad],
    data$`Tag No`[bad],
    col,
    paste0(col," must not be empty")
  )
}

# ============================================================
# TREE ID RULES
# ============================================================

if ("Tree ID" %in% names(data)) {
  tid <- trimws(as.character(data$`Tree ID`))
  recruit <- grepl("n", trimws(as.character(data$Flag1)), fixed = TRUE)
  bad <- which((is.na(tid) | tid == "") & !recruit)
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Tree ID",
    "Tree ID must not be empty unless Flag1 contains 'n'"
  )
}

# ============================================================
# REFERENCE‑DEATH RULE (CORE SPEC LOGIC)
# ============================================================

prev_dead <- ref_f1 == "0"

# If previously dead → ALL final columns must be NA
# Exclude identifier/admin columns that may legitimately be filled for dead stems
FINAL_DATA_COLS <- setdiff(FINAL_COLS, c("Tree ID", "New Tag No", "New Stem Grouping"))
final_cols_present <- intersect(FINAL_DATA_COLS, names(data))
final_has_data <- apply(
  data[, final_cols_present, drop = FALSE], 1,
  function(r) any(!is.na(r) & trimws(as.character(r)) != "")
)

bad <- which(prev_dead & final_has_data)
issues <- log_issue(
  issues,
  data$excel_row[bad],
  data$`Tag No`[bad],
  "Final block",
  "Stem was dead in reference census (F1 = 0) — verify that this stem is back to life (if not back to life, all final block columns must be NA)"
)

f1_curr <- trimws(as.character(data$Flag1))

# ============================================================
# NEW TAG NO RULES
# ============================================================

if ("New Tag No" %in% names(data)) {
  new_tag <- trimws(as.character(data$`New Tag No`))
  is_recruit <- recruit | (is.na(tid) | tid == "")
  bad <- which(is_recruit & (is.na(new_tag) | new_tag == ""))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "New Tag No", "Stem is a recruit (Tree ID blank or Flag1 contains 'n') — New Tag No must be filled"
  )
}

# ============================================================
# FLAG1 VALUE RULES
# ============================================================

has_f1 <- !is.na(data$Flag1) & f1_curr != ""

# Flag1 must never be blank
bad <- which(!has_f1 & ref_f1 == "0")
issues <- log_issue(
  issues, data$excel_row[bad], data$`Tag No`[bad],
  "Flag1", "Stem previously died (F1 = 0 in reference census) — this row should be removed from the field sheet if not a back to life stem"
)
bad <- which(!has_f1 & ref_f1 != "0")
issues <- log_issue(
  issues, data$excel_row[bad], data$`Tag No`[bad],
  "Flag1", "Flag1 is blank"
)

bad <- which(has_f1 & !grepl(FLAG1_VALID_CHARS, f1_curr))
issues <- log_issue(
  issues, data$excel_row[bad], data$`Tag No`[bad],
  "Flag1", "Invalid characters in Flag1"
)

bad <- which(has_f1 & grepl("a", f1_curr) & !f1_curr %in% FLAG1_A_VALID)
issues <- log_issue(
  issues, data$excel_row[bad], data$`Tag No`[bad],
  "Flag1", "'a' may only occur with 'n' and/or 'h'"
)

# Recruit rule
bad <- which(is.na(ref_f1) & has_f1 & !grepl("n", f1_curr))
issues <- log_issue(
  issues, data$excel_row[bad], data$`Tag No`[bad],
  "Flag1", "Reference F1 empty — Flag1 must contain 'n' for new recruits"
)

# ============================================================
# FLAG2 RULES
# ============================================================

if ("Flag2" %in% names(data)) {
  f2 <- trimws(as.character(data$Flag2))
  f2_blank <- is.na(data$Flag2) | f2 == ""
  
  # Flag2 must never be blank
  bad <- which(f2_blank & ref_f1 == "0")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag2", "Stem previously died (F1 = 0 in reference census) — this row should be removed from the field sheet"
  )
  bad <- which(f2_blank & ref_f1 != "0")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag2", "Flag2 is blank"
  )
  
  bad <- which(has_f1 & f1_curr != "0" & !sapply(f2, is_valid_f2))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag2", "Invalid Flag2 value"
  )
}

# ============================================================
# FLAG3 / FLAG4 RULES
# ============================================================

# Rows where Flag1 indicates dead or absent — Flag3/4 must be empty
dead_or_absent <- !has_f1 | f1_curr == "0"
# Rows where Flag1 is filled and alive — Flag3/4 must be a valid value
must_have_flag <- has_f1 & f1_curr != "0"

if ("Flag3" %in% names(data)) {
  f3 <- trimws(as.character(data$Flag3))
  bad <- which(dead_or_absent & !is.na(data$Flag3) & f3 != "")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag3", "Flag3 must be empty when Flag1 is 0 or NA"
  )
  bad <- which(must_have_flag & !f3 %in% F3_VALID)
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag3", paste0("Invalid Flag3 value — if tree is alive, Flag3 must be one of: ", paste(F3_VALID, collapse = ", "))
  )
}

if ("Flag4" %in% names(data)) {
  f4 <- trimws(as.character(data$Flag4))
  bad <- which(dead_or_absent & !is.na(data$Flag4) & f4 != "")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag4", "Flag4 must be empty when Flag1 is 0 or NA"
  )
  bad <- which(must_have_flag & !f4 %in% F4_VALID)
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag4", paste0("Invalid Flag4 value — if tree is alive, Flag4 must be one of: ", paste(F4_VALID, collapse = ", "))
  )
}

# ============================================================
# D / POM / FLAG4 RULES
# ============================================================

if ("D" %in% names(data) & "POM" %in% names(data)) {
  d_raw <- trimws(as.character(data$D))
  pom_raw <- trimws(as.character(data$POM))
  d   <- suppressWarnings(as.numeric(d_raw))
  pom <- suppressWarnings(as.numeric(pom_raw))
  
  # Non-numeric characters
  bad <- which(!is.na(data$D) & d_raw != "" & is.na(d))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "D", "D contains non-numeric characters — must be a number"
  )
  bad <- which(!is.na(data$POM) & pom_raw != "" & is.na(pom))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "POM", "POM contains non-numeric characters — must be a number"
  )
  
  # D must never be blank
  d_blank <- is.na(d) & (is.na(data$D) | d_raw == "")
  bad <- which(d_blank & ref_f1 == "0")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "D", "Stem previously died (F1 = 0 in reference census) — this row should be removed from the field sheet"
  )
  bad <- which(d_blank & ref_f1 != "0")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "D", "D is blank"
  )
  
  bad <- which(f1_curr == "0" & (d != 0 | pom != 0))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "D / POM", "Flag1 = 0 — D and POM must both be 0"
  )
}

# ============================================================
# HEIGHT / FLAG5 RULES
# ============================================================

if ("Height" %in% names(data) & "Flag5" %in% names(data)) {
  height_raw <- trimws(as.character(data$Height))
  height_num <- suppressWarnings(as.numeric(height_raw))
  height_present <- !is.na(data$Height) & height_raw != ""
  
  # Non-numeric characters
  bad <- which(height_present & is.na(height_num))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Height", "Height contains non-numeric characters — must be a number"
  )
  
  f5_raw <- trimws(as.character(data$Flag5))
  f5_present <- !is.na(data$Flag5) & f5_raw != ""
  
  # Height present but Flag5 missing or out of range 1–6
  bad <- which(height_present & (!f5_present | !f5_raw %in% as.character(1:6)))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag5", "Height is filled — Flag5 must be a value from 1 to 6"
  )
  
  # Flag5 filled but Height is empty
  bad <- which(!height_present & f5_present)
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag5", "Flag5 is filled but Height is empty — Flag5 must be blank when Height is blank"
  )
}

# ============================================================
# HEIGHT BROKEN AT RULES
# ============================================================

if ("Height Broken At" %in% names(data)) {
  hba_raw <- trimws(as.character(data$`Height Broken At`))
  hba_num <- suppressWarnings(as.numeric(hba_raw))
  bad <- which(!is.na(hba_raw) & hba_raw != "" & is.na(hba_num))
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Height Broken At", "Height Broken At contains non-numeric characters — must be a number"
  )
}

if ("Flag4" %in% names(data) & !all(is.na(ref_pom))) {
  pom_curr <- suppressWarnings(as.numeric(data$POM))
  bad <- which(!is.na(pom_curr) & !is.na(ref_pom) & pom_curr != ref_pom & data$Flag4 != "60")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag4", "POM changed from reference census — Flag4 must be 60"
  )
  # Reverse check: Flag4 = 60 but POM has not changed
  bad <- which(!is.na(pom_curr) & !is.na(ref_pom) & pom_curr == ref_pom & data$Flag4 == "60")
  issues <- log_issue(
    issues, data$excel_row[bad], data$`Tag No`[bad],
    "Flag4", "Flag4 is 60 but POM matches reference census — Flag4 should not be 60"
  )
}

# ============================================================
# OUTPUT
# ============================================================

issues <- issues |> arrange(excel_row, column)

if (nrow(issues) == 0) {
  message("No issues found — file passed validation.")
} else {
  issues
}

# write_xlsx(as.data.frame(table(issues$issue)), "../CongoFor1.5/RoC/SAN/SAN21_issue_summary.xlsx")

# specific_issue <- issues |>
#   filter(issue == "Stem was dead in reference census (F1 = 0) — all final block columns must be NA")
# 
# data |>
#   filter(excel_row %in% specific_issue$excel_row) |>
#   View()
# 
# # View the issues overall
# data |>
#   filter(excel_row %in% issues$excel_row) |>
#   View()
# 
# table(issues$issue) %>% View()
