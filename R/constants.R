# R/constants.R
# Column definitions and valid-value sets for ForestPlots upload validation.

FIXED_COLS <- c(
  "TreeID", "Stem Grouping", "Tag No", "T1", "T2", "X", "Y",
  "Family", "original identification", "Species", "Subspecies",
  "Variety", "Tree Notes"
)

CENSUS_COLS <- c(
  "New Tag Number", "D0", "D", "DPOMtminus1", "POM0", "POM",
  "Extra D0", "Extra D", "ExtraDPOMtminus1", "Extra POM0", "Extra POM",
  "Flag1", "Flag2", "Flag3", "Flag4", "Extra Flag3", "Extra Flag4",
  "LI", "CI", "CF", "CD1", "CD2", "Height", "Flag5",
  "Height Broken At", "census_notes", "voucher code", "voucher collected"
)

N_FIXED      <- length(FIXED_COLS)   # 13
N_CENSUS     <- length(CENSUS_COLS)  # 28
CENSUS_START <- 14L                  # first census block starts at column 14

CI_VALID <- c("5", "4", "3b", "3a", "2c", "2b", "2a", "1")
F3_VALID <- as.character(0:6)
F4_VALID <- c("0", "1", "2", "3", "4", "6", "7", "8", "60")

FLAG1_VALID_CHARS <- "^[abcdefghijklmnopqswxyz0]*$"

# 'a' can only appear with 'n', 'h', or both (all permutations)
FLAG1_A_VALID <- c("a", "ah", "ha", "an", "na",
                   "ahn", "anh", "han", "hna", "nah", "nha")

# Flag2 character group definitions
F2_G1 <- "[abcdefghiklm]"
F2_G2 <- "[pqr]"
F2_G3 <- "[jnostuvwxyz234567]"
