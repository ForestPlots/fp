# R/fill_coord.R
# Coordinate transformation for ForestPlots stem data.
#
# Converts subplot-level XY coordinates to full-plot XY coordinates,
# or passes through coordinates already recorded at plot level.
#
# Only designed for standard 5 × 5 RAINFOR-style 1-ha plots (25 subplots of
# 20 m × 20 m each). For "plot" mode, any plot shape is accepted.
#
# Depends on: data.table

library(data.table)

#' Convert subplot XY coordinates to plot-level XY coordinates.
#'
#' Adds two new columns — \code{x_plot} and \code{y_plot} — representing stem
#' positions in the full-plot coordinate system. Stems with missing XY receive
#' artificial negative-X positions stacked on the west edge so they remain
#' visible when mapped.
#'
#' @param data        A data frame (or data.table) of stem records.
#' @param coord_scale Character. The recording convention used in the field.
#'   \describe{
#'     \item{"plot"}{(Default) Coordinates are already in plot-level space.
#'       X and Y are copied to x_plot and y_plot unchanged.}
#'     \item{"rainfor"}{Traditional RAINFOR zig-zag protocol: the measurer
#'       walks column-by-column through subplots, so the measurement origin
#'       flips in even-numbered subplot columns.}
#'     \item{"rainfor-north"}{Coordinates recorded inside each subplot always
#'       facing north (no zig-zag flip). Subplot offsets are added to place
#'       stems in plot space.}
#'     \item{"rainfor-east"}{Like "rainfor" but the serpentine walk runs
#'       row-by-row (east/west) rather than column-by-column.}
#'   }
#' @param subplot_col Character. Name of the subplot column in \code{data}.
#'   Default: \code{"T1"}.
#' @param id_col      Character. Name of the stem identifier column. Used only
#'   when assigning artificial coordinates to records with missing XY. Default:
#'   \code{"Tag No"}.
#' @param x_col       Character. Name of the raw X column. Default: \code{"X"}.
#' @param y_col       Character. Name of the raw Y column. Default: \code{"Y"}.
#'
#' @return A data.table equal to \code{data} with \code{x_plot} and
#'   \code{y_plot} columns appended.
#'
#' @examples
#' \dontrun{
#' source("run_checks.R")
#'
#' # Coordinates already at plot level (default)
#' filled <- fill_coord(my_data)
#'
#' # Coordinates recorded with the RAINFOR zig-zag protocol
#' filled <- fill_coord(my_data, coord_scale = "rainfor")
#'
#' # Use T2 as the subplot column
#' filled <- fill_coord(my_data, coord_scale = "rainfor-north", subplot_col = "T2")
#' }
#' 
fill_coord <- function(data,
                       coord_scale = c("plot", "rainfor", "rainfor-north", "rainfor-east"),
                       subplot_col = "T1",
                       id_col      = "Tag No",
                       x_col       = "X",
                       y_col       = "Y") {

  coord_scale <- match.arg(coord_scale)

  missing_cols <- setdiff(c(subplot_col, id_col, x_col, y_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  dt           <- data.table::as.data.table(data)
  subplot_size <- 20L
  n_cols       <- 5L

  dt[[subplot_col]] <- as.numeric(dt[[subplot_col]])

  # ── plot ──────────────────────────────────────────────────────────────────
  # Coordinates are already in full-plot space; copy directly.
  if (coord_scale == "plot") {
    dt[, x_plot := as.numeric(dt[[x_col]])]
    dt[, y_plot := as.numeric(dt[[y_col]])]
  }

  # ── rainfor ───────────────────────────────────────────────────────────────
  # Serpentine walk column-by-column.
  # Odd columns measured from lower-left; even columns from upper-right.
  if (coord_scale == "rainfor") {
    sp_v <- dt[[subplot_col]]
    x_v  <- as.numeric(dt[[x_col]])
    y_v  <- as.numeric(dt[[y_col]])

    col_idx <- ((sp_v - 1L) %/% 5L) + 1L
    col_raw <- ((sp_v - 1L) %% n_cols) + 1L
    row_raw <- ((sp_v - 1L) %/% n_cols) + 1L
    # Flip column order on even rows to follow the serpentine walk
    row_idx <- ifelse(row_raw %% 2L == 0L, (n_cols + 1L) - col_raw, col_raw)

    is_even_col <- (col_idx %% 2L == 0L)

    anchor_x <- ifelse(is_even_col, col_idx * subplot_size, (col_idx - 1L) * subplot_size)
    anchor_y <- ifelse(is_even_col, row_idx * subplot_size, (row_idx - 1L) * subplot_size)

    dt[, x_plot := ifelse(is_even_col, anchor_x - x_v, anchor_x + x_v)]
    dt[, y_plot := ifelse(is_even_col, anchor_y - y_v, anchor_y + y_v)]
  }

  # ── rainfor-east ──────────────────────────────────────────────────────────
  # Serpentine walk row-by-row (east/west).
  # Odd rows measured from lower-left; even rows from upper-right.
  if (coord_scale == "rainfor-east") {
    sp_v <- dt[[subplot_col]]
    x_v  <- as.numeric(dt[[x_col]])
    y_v  <- as.numeric(dt[[y_col]])

    col_raw <- ((sp_v - 1L) %% n_cols) + 1L
    row_raw <- ((sp_v - 1L) %/% n_cols) + 1L
    # Flip column order on even rows
    col_idx <- ifelse(row_raw %% 2L == 0L, (n_cols + 1L) - col_raw, col_raw)

    is_even_row <- (row_raw %% 2L == 0L)

    anchor_x <- ifelse(is_even_row, col_idx * subplot_size, (col_idx - 1L) * subplot_size)
    anchor_y <- ifelse(is_even_row, row_raw * subplot_size, (row_raw - 1L) * subplot_size)

    dt[, x_plot := ifelse(is_even_row, anchor_x - x_v, anchor_x + x_v)]
    dt[, y_plot := ifelse(is_even_row, anchor_y - y_v, anchor_y + y_v)]
  }

  # ── rainfor-north ─────────────────────────────────────────────────────────
  # Always facing north; no orientation flip. Subplot column offset added to X;
  # Y offset read from a lookup table that follows the serpentine row structure.
  if (coord_scale == "rainfor-north") {
    sp_v <- dt[[subplot_col]]
    x_v  <- as.numeric(dt[[x_col]])
    y_v  <- as.numeric(dt[[y_col]])

    x_offset <- subplot_size * ((sp_v - 1L) %/% 5L)
    dt[, x_plot := x_v + x_offset]

    y_offsets <- c(
       0, 20, 40, 60, 80,
      80, 60, 40, 20,  0,
       0, 20, 40, 60, 80,
      80, 60, 40, 20,  0,
       0, 20, 40, 60, 80
    )

    valid        <- !is.na(sp_v) & sp_v >= 1L & sp_v <= 25L
    y_plot_vals  <- rep(NA_real_, nrow(dt))
    y_plot_vals[valid] <- y_v[valid] + y_offsets[sp_v[valid]]
    dt[, y_plot := y_plot_vals]
  }

  # ── Artificial coordinates for stems with missing XY ──────────────────────
  # Stems where x_plot is still NA after transformation are placed at negative
  # X positions west of the plot, stacked in groups of 50.
  id_v      <- dt[[id_col]]
  missing_x <- which(is.na(dt[["x_plot"]]))

  if (length(missing_x) > 0) {
    x_less <- sort(id_v[missing_x])
    for (i in seq_along(x_less)) {
      idx <- which(id_v == x_less[[i]])
      dt[idx, x_plot := -10 - 5 * ceiling(i / 50)]
    }

    neg_x_positions <- sort(
      unique(dt[["x_plot"]][!is.na(dt[["x_plot"]]) & dt[["x_plot"]] < 0]),
      decreasing = TRUE
    )
    for (x_pos in neg_x_positions) {
      ids_here <- id_v[!is.na(dt[["x_plot"]]) & dt[["x_plot"]] == x_pos]
      for (j in seq_along(ids_here)) {
        idx <- which(id_v == ids_here[[j]])
        dt[idx, y_plot := j * 2L]
      }
    }
  }

  dt
}
