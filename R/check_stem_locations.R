# R/check_stem_locations.R
# Interactive stem-location map for ForestPlots upload files.
#
# Plots X/Y coordinates coloured by subplot (T1 or T2) for any of the three
# supported dataset types, using the same header-detection logic as the
# validation modules.
#
# Usage (after sourcing run_checks.R):
#
#   plot_stem_locations(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1"
#   )
#
#   # Colour by T2 instead of T1
#   plot_stem_locations("single_recensus", "path/to/file.xlsx",
#                       subplot_col = "T2")
#
# Depends on: readxl, dplyr, ggplot2, plotly

library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

# ── Palette ───────────────────────────────────────────────────────────────────

LOCATION_PALETTE <- c(
  "#5A5156", "#F6222E", "#FE00FA", "#16FF32", "#3283FE", "#FEAF16",
  "#B00068", "#1CFFCE", "#90AD1C", "#2ED9FF", "#DEA0FD", "#AA0DFE",
  "#F8A19F", "#325A9B", "#C4451C", "#1C8356", "#85660D", "#B10DA1",
  "#FBE426", "#1CBE4F", "#FC1CBF", "#F7E1A0", "#C075A6", "#782AB6",
  "#AAF400", "#BDCDFF", "#822E1C", "#B5EFB5", "#7ED7D1", "#1C7F93",
  "#D85FF7", "#683B79", "#66B0FF", "#3B00FB"
)

# ── Internal helper ───────────────────────────────────────────────────────────

#' Load X/Y/subplot columns from a ForestPlots upload file.
#'
#' Handles the header-row differences across the three dataset types:
#'   - new_multicensus  : row 1 is the census-date filler row; column names
#'                        are always in row 2.
#'   - single_recensus  : column names are in row 1 or row 2, detected by the
#'                        presence of "Tag No" or "New Tag No" in row 1.
#'   - new_single_census: column names are in row 1 or row 2, detected by the
#'                        presence of "Tag No" in row 1.
#'
#' @param dataset_type One of "new_multicensus", "single_recensus",
#'   "new_single_census".
#' @param file_path   Path to the .xlsx file.
#' @param sheet_name  Sheet name or index passed to readxl.
#' @return A tibble with at minimum columns X, Y, excel_row, and whatever
#'   columns were present in the file (including T1, T2, Tag No, etc.).
load_location_data <- function(dataset_type, file_path, sheet_name) {
  raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

  header_row <- switch(
    dataset_type,
    new_multicensus = 2L,
    single_recensus = {
      if (any(c("Tag No", "New Tag No") %in% as.character(raw[1, ]))) 1L else 2L
    },
    new_single_census = {
      if (any(c("New Tag No", "Tag No") %in% as.character(raw[1, ]))) 1L else 2L
    },
    stop("Unknown dataset_type: '", dataset_type, "'. ",
         "Supported types: 'new_multicensus', 'new_single_census', 'single_recensus'.")
  )
  message("Column names detected in row ", header_row, ".")

  col_names   <- as.character(raw[header_row, ])
  data        <- raw[-(seq_len(header_row)), ]
  names(data) <- col_names

  data |>
    mutate(excel_row = row_number() + header_row) |>
    filter(rowSums(!is.na(across(everything()))) > 0)
}

# ── Main function ─────────────────────────────────────────────────────────────

#' Plot stem X/Y locations for a ForestPlots upload file.
#'
#' Produces an interactive plotly scatter plot of stem coordinates coloured by
#' subplot. Hover text shows the stem's Tag No and its excel row number.
#'
#' @param dataset_type Character. One of:
#'   \describe{
#'     \item{"new_multicensus"}{New plots with two or more census periods.}
#'     \item{"new_single_census"}{New plots with a single census.}
#'     \item{"single_recensus"}{Existing plots with one new census (field sheet).}
#'   }
#' @param file_path   Character. Path to the .xlsx file.
#' @param sheet_name  Sheet name or index passed to readxl. Default: 1.
#' @param subplot_col Character. Column to use for colour grouping. Must be
#'   present in the file. Default: "T1".
#' @param x_breaks    Numeric vector of x-axis gridline positions. If NULL
#'   (default), breaks are chosen automatically with pretty().
#' @param y_breaks    Numeric vector of y-axis gridline positions. If NULL
#'   (default), breaks are chosen automatically with pretty().
#'
#' @return A plotly htmlwidget (printed automatically when called
#'   interactively).
#'
#' @examples
#' \dontrun{
#' source("run_checks.R")
#'
#' # New multi-census plot, colour by T1
#' plot_stem_locations(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "plot001"
#' )
#'
#' # Existing-plot field sheet, colour by T2, fixed axis breaks
#' plot_stem_locations(
#'   dataset_type = "single_recensus",
#'   file_path    = "data/field_sheet.xlsx",
#'   sheet_name   = "Field Sheet",
#'   subplot_col  = "T2",
#'   x_breaks     = seq(0, 100, by = 10),
#'   y_breaks     = seq(0, 100, by = 10)
#' )
#' }
plot_stem_locations <- function(dataset_type,
                                file_path,
                                sheet_name  = 1,
                                subplot_col = "T1",
                                x_breaks    = NULL,
                                y_breaks    = NULL) {

  data <- load_location_data(dataset_type, file_path, sheet_name)

  # Validate required columns
  required <- c("X", "Y", subplot_col)
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Required column(s) not found in the file: ",
         paste(missing, collapse = ", "))
  }

  # Coerce coordinates to numeric
  data <- data |>
    mutate(
      X = as.numeric(X),
      Y = as.numeric(Y)
    )

  n_na_x <- sum(is.na(data$X))
  n_na_y <- sum(is.na(data$Y))
  if (n_na_x > 0) message(n_na_x, " row(s) with non-numeric or missing X dropped from plot.")
  if (n_na_y > 0) message(n_na_y, " row(s) with non-numeric or missing Y dropped from plot.")
  data <- filter(data, !is.na(X), !is.na(Y))

  # Order subplot levels numerically where possible, otherwise alphabetically
  raw_vals <- data[[subplot_col]]
  num_vals <- suppressWarnings(as.numeric(raw_vals))
  subplot_levels <- if (!anyNA(num_vals[!is.na(raw_vals)])) {
    as.character(sort(unique(num_vals)))
  } else {
    sort(unique(as.character(raw_vals)))
  }
  data[[subplot_col]] <- factor(data[[subplot_col]], levels = subplot_levels)

  # Tooltip: Tag No when available, otherwise just the excel row
  tag_col <- intersect(c("Tag No", "New Tag No"), names(data))[1]
  if (!is.na(tag_col)) {
    data <- data |> mutate(.label = paste0("Tag: ", .data[[tag_col]],
                                           "\nRow: ", excel_row))
  } else {
    data <- data |> mutate(.label = paste0("Row: ", excel_row))
  }

  # Default axis breaks
  if (is.null(x_breaks)) x_breaks <- pretty(data$X)
  if (is.null(y_breaks)) y_breaks <- pretty(data$Y)

  ggplotly(
    ggplot(data, aes(x = X, y = Y,
                     color = .data[[subplot_col]],
                     text  = .label)) +
      geom_point() +
      scale_color_manual(values = LOCATION_PALETTE, name = subplot_col) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks) +
      labs(title = paste("Location QC \u2014", subplot_col, "\u2014", dataset_type)),
    tooltip = "text"
  )
}
