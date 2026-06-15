# NOTE: this is currently not set up to work with any plot_stem_locations function
# NEED TO ADD A WAY TO USE THE FILL_COORD.R FUNCTIONS IN THE CHECK STEM LOCATIONS SCRIPT; 
# the default for this when used with plot_stem_locations should be plot level
# if the funciton is at plot level and the XY range is under 20, then a warning should appear saying that the XY may be recorded at subplot level. 

# -------------------------------------------------------------------------
# fill_coord()
# Converts subplot-level XY coordinates to full-plot XY depending on protocol.
#
# Supports three modes:
#   - "rainfor"        : Coordinates recorded according to the direction the person faces
#                       (zig-zag walking → orientation flips in even columns)
#   - "rainfor-north"  : Coordinates recorded in subplot space with fixed orientation
#   - "plot"           : Coordinates already in plot-level space
#
# Also assigns artificial coordinates for missing stems on the west edge.
# Only use with standard 5x5 rainfor-style 1-ha plots.
# -------------------------------------------------------------------------

fill_coord <- 
  function(fieldsheet, 
           coord_scale = c("rainfor", "rainfor-north", "plot", "rainfor-east"), 
           subplot = t1, 
           id_tree = tree_id, 
           x_stem = x, 
           y_stem = y){
    
    
    # ---------------------------------------------------------------------
    # Validate coord_scale (forces: "rainfor", "subplot", or "plot")
    # --------------------------------------------------------------
    coord_scale <- match.arg(coord_scale)
    
    
    # ---------------------------------------------------------------------
    # Convert symbol arguments into column names (as strings)
    # ---------------------------------------------------------------------
    subplot <- deparse(substitute(subplot))
    id_tree <- deparse(substitute(id_tree))
    x_stem   <- deparse(substitute(x_stem))
    y_stem   <- deparse(substitute(y_stem))
    
    # Ensure subplot column is numeric
    fieldsheet[[subplot]] <- as.numeric(fieldsheet[[subplot]])
    
    
    # =====================================================================
    # =======================  rainfor MODE  ==============================
    # =====================================================================
    # Person moves serpentine through subplots → orientation flips.
    # Columns 1,3,5 measured from lower-left; columns 2,4 measured from top-right.
    # =====================================================================
    if(coord_scale=="rainfor"){
      
      fieldsheet <- data.table::as.data.table(fieldsheet)
      
      # Constants
      subplot_size <- 20L     # size of each subplot (m)
      n_cols <- 5L
      n_rows <- 5L
      
      # Extract local numeric vectors for convenience
      subplot <- as.numeric(fieldsheet[[subplot]])
      x_stem <- as.numeric(fieldsheet[[x_stem]])
      y_stem <- as.numeric(fieldsheet[[y_stem]])
      
      
      # ------------------------------------------------------------
      # COLUMN INDEX: 1–5 block structure, NOT serpentine
      # Used for horizontal orientation (odd = normal, even = flipped)
      # ------------------------------------------------------------
      col_idx <- ((subplot - 1L) %/% 5L) + 1L # 1..5
      
      
      # ------------------------------------------------------------
      # ROW & COLUMN INDEXES for serpentine Y grouping
      # col_raw = raw 1–5 within row
      # row_raw = row block 1–5
      # row_idx = serpentine-adjusted "column" index per your layout
      # ------------------------------------------------------------
      col_raw <- ((subplot - 1L) %% n_cols) + 1L
      row_raw <- ((subplot - 1L) %/% n_cols) + 1L
      
      # Flip column order for even-numbered rows (serpentine walk)
      row_idx <- ifelse(
        row_raw %% 2L == 0L,            # even rows
        (n_cols + 1L) - col_raw,        # flip left ↔ right
        col_raw             
      )
      
      
      # ------------------------------------------------------------
      # X COORDINATE (orientation depends on column)
      # odd columns  → measure from left edge
      # even columns → measure from right edge
      # ----------------------------------------------------------
      is_even <- (col_idx %% 2L == 0L)
      
      # Left or right subplot boundarie
      anchor_x <- ifelse(
        is_even, 
        col_idx * 20L,         # right boundary (40,
        (col_idx - 1L) * 20L   # left boundary (0, 40, 80)
      )
      
      # Final X coordinate: add or subtract depending on orien
      x_plot <- ifelse(is_even, anchor_x - x_stem, anchor_x + x_stem)
      
      
      # ------------------------------------------------------------
      # Y COORDINATE (simple row offset; no orientation flip)
      # row 1 → 0m, row 2 → 20m, ..., row 5 → 80m
      # -----------------------------------------------------------
      anchor_y <- ifelse(is_even, row_idx * 20L, (row_idx - 1L) * 20L)
      y_plot <- ifelse(is_even, anchor_y - y_stem, anchor_y + y_stem)
      
      # Attach coordinates
      fieldsheet[, x_plot := x_plot]
      fieldsheet[, y_plot := y_plot]
      
    } 
    
    
    # =====================================================================
    # =======================  RAINFOR-EAST MODE  ==========================
    # =====================================================================
    # Layout: 5 × 5 grid of 20 m subplots
    #
    # Traversal pattern (serpentine by ROW):
    #   - Row 1, 3, 5 → left → right
    #   - Row 2, 4   → right → left
    #   - Start at bottom-left, move across row, then snake upward
    #
    # Measurement origin:
    #   - Orientation alternates with row direction
    #   - Odd rows  → measured from lower-left corner
    #   - Even rows → measured from upper-right corner
    # =====================================================================
    
    if(coord_scale == "rainfor-east"){
      
      fieldsheet <- data.table::as.data.table(fieldsheet)
      
      # Constants
      subplot_size <- 20L
      n_cols <- 5L
      n_rows <- 5L
      
      # Extract numeric vectors
      subplot <- as.numeric(fieldsheet[[subplot]])
      x_stem  <- as.numeric(fieldsheet[[x_stem]])
      y_stem  <- as.numeric(fieldsheet[[y_stem]])
      
      # ------------------------------------------------------------
      # RAW GRID POSITION (no serpentine yet)
      # ------------------------------------------------------------
      col_raw <- ((subplot - 1L) %% n_cols) + 1L   # 1..5 left→right
      row_raw <- ((subplot - 1L) %/% n_cols) + 1L  # 1..5 bottom→top
      
      # ------------------------------------------------------------
      # SERPENTINE COLUMN INDEX
      # Flip column order for even-numbered rows
      # ------------------------------------------------------------
      col_idx <- ifelse(
        row_raw %% 2L == 0L,            # even rows: right → left
        (n_cols + 1L) - col_raw,
        col_raw                         # odd rows: left → right
      )
      
      # ------------------------------------------------------------
      # ORIENTATION FLAG (based on row direction)
      # odd rows  → normal (from lower-left)
      # even rows → flipped (from upper-right)
      # ------------------------------------------------------------
      is_even_row <- (row_raw %% 2L == 0L)
      
      # ------------------------------------------------------------
      # X COORDINATE
      # ------------------------------------------------------------
      anchor_x <- ifelse(
        is_even_row,
        col_idx * subplot_size,           # right boundary
        (col_idx - 1L) * subplot_size     # left boundary
      )
      
      x_plot <- ifelse(
        is_even_row,
        anchor_x - x_stem,                # measure from right
        anchor_x + x_stem                 # measure from left
      )
      
      # ------------------------------------------------------------
      # Y COORDINATE
      # ------------------------------------------------------------
      anchor_y <- ifelse(
        is_even_row,
        row_raw * subplot_size,           # top boundary
        (row_raw - 1L) * subplot_size     # bottom boundary
      )
      
      y_plot <- ifelse(
        is_even_row,
        anchor_y - y_stem,                # measure from top
        anchor_y + y_stem                 # measure from bottom
      )
      
      # Attach coordinates
      fieldsheet[, x_plot := x_plot]
      fieldsheet[, y_plot := y_plot]
    }
    
    
    # =====================================================================
    # ========================== RAINFOR-NORTH ============================
    # =====================================================================
    # XY recorded inside subplot space but always facing north (no zig-zag)
    # =====================================================================
    if (coord_scale == "rainfor-north") {
      
      fieldsheet <- data.table::as.data.table(fieldsheet)
      
      # X offsets per block of 5 subplots
      x_offset <- 20 * ((fieldsheet[[subplot]] - 1L) %/% 5L)
      fieldsheet[, x_plot := fieldsheet[[x_stem]] + x_offset]
      
      # Predefined rainfor Y offsets for serpentine structur
      y_offsets <- c(
        0,20,40,60,80,
        80,60,40,20,0,
        0,20,40,60,80,
        80,60,40,20,0,
        0,20,40,60,80
      )
      
      y_plot_vals <- fieldsheet[[y_stem]] + y_offsets[fieldsheet[[subplot]]]
      
      # Only valid for subplot 1–25
      fieldsheet[
        data.table::between(get(subplot), 1, 25), 
        y_plot := y_plot_vals
      ]
      
      fieldsheet[
        !data.table::between(get(subplot), 1, 25), 
        y_plot := NA_real_
      ]
    }
    
    
    # =====================================================================
    # ============================ PLOT MODE ===============================
    # =====================================================================
    # XY already recorded in global plot coordinates → copy as-is
    # ==================================================================
    if(coord_scale=="plot"){
      fieldsheet[["x_plot"]] <- fieldsheet[[x_stem]]
      fieldsheet[["y_plot"]] <- fieldsheet[[y_stem]]
    }
    
    
    # =====================================================================
    # ========= ASSIGN ARTIFICIAL COORDINATES FOR MISSING XY ==============
    # =====================================================================
    # Trees with missing coordinates placed on west side of plot
    # ====================================================================
    
    # X missing: stack them westwards in groups o
    x_less <- sort(fieldsheet[is.na(x_plot)][[id_tree]])
    for(i in seq_along(x_less)){
      fieldsheet[get(id_tree)==x_less[i]][["x_plot"]] <- -10-5*ceiling(i/50)
    };rm(i)
    
    # Y positions for these artificial X positio
    for(i in sort(unique(fieldsheet[x_plot<0][["x_plot"]]), decreasing = TRUE)){
      y_less <- fieldsheet[x_plot==i][[id_tree]]
      for(j in seq_along(y_less)){
        fieldsheet[get(id_tree)==y_less[j], y_plot := j * 2]
      }
    }
    return(fieldsheet)
  }
