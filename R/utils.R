# R/utils.R
# Shared helper functions used across all check modules.
# Depends on: constants.R (F2_G1, F2_G2, F2_G3, CENSUS_COLS)

# ── Shared helpers ────────────────────────────────────────────────────────────

#' Return TRUE for each element that is NA or blank after trimming whitespace.
#' @param x Any vector.
#' @return Logical vector, same length as x.
is_empty <- function(x) is.na(x) | trimws(as.character(x)) == ""

# ── Issue accumulation ────────────────────────────────────────────────────────

new_issues <- function() list()

#' Append one issue entry to the issues list.
#'
#' @param issues     Current list of issue tibbles.
#' @param rows       Integer vector of Excel row numbers.
#' @param tree_ids   Corresponding TreeID values.
#' @param census     Census label, e.g. "Census 1" (NA for structural checks).
#' @param column     Column name or label where the issue was found.
#' @param description Plain-English description of the problem.
#' @return Updated issues list.
log_issue <- function(issues, rows, tree_ids, census, column, description) {
  if (length(rows) == 0) return(issues)
  issues[[length(issues) + 1]] <- tibble(
    excel_row = as.integer(rows),
    TreeID    = as.character(tree_ids),
    census    = as.character(census),
    column    = as.character(column),
    issue     = as.character(description)
  )
  issues
}

# ── Flag2 validation ──────────────────────────────────────────────────────────

#' Test whether each element of x is a valid Flag2 value.
#'
#' Valid values: "1"  OR  a string containing at most one character from each
#' of three defined groups (F2_G1, F2_G2, F2_G3) and no other characters.
#'
#' @param x Character vector.
#' @return Logical vector, same length as x.
is_valid_f2 <- function(x) {
  out       <- rep(FALSE, length(x))
  non_empty <- !is.na(x) & x != ""
  out[non_empty & x == "1"] <- TRUE
  check <- non_empty & x != "1" & !grepl("1", x)
  if (any(check)) {
    v  <- x[check]
    g1 <- str_count(v, F2_G1)
    g2 <- str_count(v, F2_G2)
    g3 <- str_count(v, F2_G3)
    out[check] <- g1 <= 1 & g2 <= 1 & g3 <= 1 & (g1 + g2 + g3 == nchar(v))
  }
  out
}

# ── Census block occupancy ────────────────────────────────────────────────────

#' Return TRUE for each row where census block c contains at least one value.
#'
#' Used to distinguish genuinely active census records from rows where a stem
#' had not yet recruited or had already died, so required-field checks are not
#' incorrectly raised for empty blocks.
#'
#' @param df Data frame produced by parse_fp_data().
#' @param c  Integer census index.
#' @return Logical vector of length nrow(df).
census_block_has_data <- function(df, c) {
  cols <- paste0(CENSUS_COLS, "_c", c)
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  Reduce("|", lapply(df[cols], function(x) !is.na(x) & trimws(as.character(x)) != ""))
}
