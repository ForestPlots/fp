# R/check_stem_locations.R
# Interactive stem-location map for ForestPlots upload files.
#
# Loads X/Y coordinates from any of the three supported dataset types,
# transforms them to plot-level space via fill_coord(), and produces an
# interactive plotly scatter plot coloured by subplot.
#
# Usage (after sourcing run_checks.R):
#
#   # Coordinates already in plot space (default)
#   plot_stem_locations(
#     dataset_type = "new_multicensus",
#     file_path    = "path/to/upload.xlsx",
#     sheet_name   = "Sheet1"
#   )
#
#   # Coordinates recorded with the RAINFOR zig-zag protocol
#   plot_stem_locations(
#     dataset_type = "single_recensus",
#     file_path    = "path/to/file.xlsx",
#     coord_scale  = "rainfor"
#   )
#
#   # Colour by T2, fixed axis breaks
#   plot_stem_locations("new_multicensus", "path/to/upload.xlsx",
#                       subplot_col = "T2",
#                       x_breaks    = seq(0, 100, by = 10),
#                       y_breaks    = seq(0, 100, by = 10))
#
# Depends on: readxl, dplyr, ggplot2, plotly, fill_coord.R

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

#' Load stem records from a ForestPlots upload file.
#'
#' Handles the header-row differences across the three dataset types:
#'   - new_multicensus  : row 1 is the census-date filler; column names always
#'                        in row 2.
#'   - single_recensus  : column names in row 1 or 2, detected by the presence
#'                        of "Tag No" or "New Tag No" in row 1.
#'   - new_single_census: column names in row 1 or 2, detected by the presence
#'                        of "Tag No" in row 1.
#'
#' @param dataset_type One of "new_multicensus", "single_recensus",
#'   "new_single_census".
#' @param file_path   Path to the .xlsx file.
#' @param sheet_name  Sheet name or index passed to readxl.
#' @return A tibble containing all columns from the file plus an excel_row
#'   column recording each stem's original row number in the workbook.
load_location_data <- function(dataset_type, file_path, sheet_name) {
  raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)

  header_row <- switch(
    dataset_type,
    new_multicensus = 2L,
    single_recensus = {
      if (any(c("Tag No", "New Tag No") %in% as.character(raw[1, ]))) 1L else 2L
    },
    new_single_census = {
      if (any(c("Tag No", "New Tag No") %in% as.character(raw[1, ]))) 1L else 2L
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
#' Loads stem records, transforms their coordinates to plot-level space using
#' \code{\link{fill_coord}}, and produces an interactive plotly scatter plot
#' coloured by subplot. Stems with missing XY coordinates are placed on the
#' west edge of the plot (x < 0) rather than silently dropped.
#'
#' @param dataset_type Character. One of:
#'   \describe{
#'     \item{"new_multicensus"}{New plots with two or more census periods.}
#'     \item{"new_single_census"}{New plots with a single census.}
#'     \item{"single_recensus"}{Existing plots with one new census (field sheet).}
#'   }
#' @param file_path   Character. Path to the .xlsx file.
#' @param sheet_name  Sheet name or index passed to readxl. Default: 1.
#' @param subplot_col Character. Column to use for colour grouping (e.g.
#'   \code{"T1"} or \code{"T2"}). Must be present in the file. Default:
#'   \code{"T1"}.
#' @param coord_scale Character. The recording convention for the X/Y
#'   coordinates in the file. Passed directly to \code{\link{fill_coord}}.
#'   \describe{
#'     \item{"plot"}{(Default) Already in full-plot space; no transformation.}
#'     \item{"rainfor"}{RAINFOR zig-zag, column-by-column.}
#'     \item{"rainfor-north"}{Subplot space, always facing north.}
#'     \item{"rainfor-east"}{RAINFOR zig-zag, row-by-row.}
#'   }
#' @param x_breaks    Numeric vector of x-axis gridline positions. If
#'   \code{NULL} (default), breaks are chosen automatically with
#'   \code{pretty()}.
#' @param y_breaks    Numeric vector of y-axis gridline positions. If
#'   \code{NULL} (default), breaks are chosen automatically with
#'   \code{pretty()}.
#'
#' @return A plotly htmlwidget (printed automatically when called
#'   interactively).
#'
#' @examples
#' \dontrun{
#' source("run_checks.R")
#'
#' # Coordinates already in plot space (default)
#' plot_stem_locations(
#'   dataset_type = "new_multicensus",
#'   file_path    = "data/my_upload.xlsx",
#'   sheet_name   = "plot001"
#' )
#'
#' # Coordinates from RAINFOR zig-zag field protocol, colour by T2
#' plot_stem_locations(
#'   dataset_type = "single_recensus",
#'   file_path    = "data/field_sheet.xlsx",
#'   sheet_name   = "Field Sheet",
#'   subplot_col  = "T2",
#'   coord_scale  = "rainfor"
#' )
#'
#' # Fixed axis breaks for a standard 1-ha plot
#' plot_stem_locations(
#'   dataset_type = "new_single_census",
#'   file_path    = "data/my_upload.xlsx",
#'   coord_scale  = "rainfor-north",
#'   x_breaks     = seq(0, 100, by = 20),
#'   y_breaks     = seq(0, 100, by = 20)
#' )
#' }
plot_stem_locations <- function(dataset_type,
                                file_path,
                                sheet_name  = 1,
                                subplot_col = "T1",
                                coord_scale = c("plot", "rainfor", "rainfor-north", "rainfor-east"),
                                x_breaks    = NULL,
                                y_breaks    = NULL) {

  coord_scale <- match.arg(coord_scale)

  data <- load_location_data(dataset_type, file_path, sheet_name)

  # Validate required columns before attempting transformation
  required <- c("X", "Y", subplot_col)
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Required column(s) not found in the file: ",
         paste(missing, collapse = ", "))
  }

  # Identify the stem label column for tooltips and fill_coord's id_col
  tag_col <- intersect(c("Tag No", "New Tag No"), names(data))
  id_col  <- if (length(tag_col) > 0) tag_col[[1L]] else names(data)[[1L]]

  # Warn about missing raw coordinates before transformation
  x_raw <- suppressWarnings(as.numeric(data[["X"]]))
  y_raw <- suppressWarnings(as.numeric(data[["Y"]]))
  
  # Check for subplot-level coordinates mistakenly labelled as plot-level
  if (coord_scale == "plot") {
    max_x <- suppressWarnings(max(x_raw, na.rm = TRUE))
    max_y <- suppressWarnings(max(y_raw, na.rm = TRUE))
    
    if (!is.infinite(max_x) && !is.infinite(max_y) &&
        max_x <= 20 && max_y <= 20) {
      
      warning(
        "Maximum X and Y values are both < 20 while coord_scale = 'plot'. ",
        "Coordinates may have been recorded at subplot level rather than plot level."
      )
    }
  }
  
  n_na  <- sum(is.na(x_raw) | is.na(y_raw))
  if (n_na > 0) {
    message(n_na, " stem(s) with missing X and/or Y will appear on the west ",
            "edge of the plot (x < 0).")
  }

  # Transform coordinates to plot-level space
  data <- fill_coord(
    data        = data,
    coord_scale = coord_scale,
    subplot_col = subplot_col,
    id_col      = id_col,
    x_col       = "X",
    y_col       = "Y"
  )

  # Safety filter: drop any rows still missing plot coordinates after transform
  # (can occur in rainfor-north when subplot number is outside 1–25)
  n_still_na <- sum(is.na(data[["x_plot"]]) | is.na(data[["y_plot"]]))
  if (n_still_na > 0) {
    message(n_still_na, " stem(s) with unresolvable coordinates removed from plot.")
    data <- data[!is.na(data[["x_plot"]]) & !is.na(data[["y_plot"]]), ]
  }

  # Order subplot levels numerically where possible, otherwise alphabetically
  raw_vals       <- data[[subplot_col]]
  num_vals       <- suppressWarnings(as.numeric(raw_vals))
  subplot_levels <- if (!anyNA(num_vals[!is.na(raw_vals)])) {
    as.character(sort(unique(num_vals)))
  } else {
    sort(unique(as.character(raw_vals)))
  }
  data[[subplot_col]] <- factor(data[[subplot_col]], levels = subplot_levels)

  # Tooltip
  data$.label <- if (!is.na(id_col) && id_col %in% names(data)) {
    paste0("Tag: ", data[[id_col]], "\nRow: ", data[["excel_row"]])
  } else {
    paste0("Row: ", data[["excel_row"]])
  }

  if (is.null(x_breaks)) x_breaks <- pretty(data[["x_plot"]])
  if (is.null(y_breaks)) y_breaks <- pretty(data[["y_plot"]])

  ggplotly(
    ggplot(data, aes(x     = x_plot,
                     y     = y_plot,
                     color = .data[[subplot_col]],
                     text  = .label)) +
      geom_point() +
      scale_color_manual(values = LOCATION_PALETTE, name = subplot_col) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks) +
      labs(
        x     = "X (m)",
        y     = "Y (m)",
        title = paste0("Location QC \u2014 ", subplot_col,
                       " \u2014 ", dataset_type,
                       " [", coord_scale, "]")
      ),
    tooltip = "text"
  )
}
