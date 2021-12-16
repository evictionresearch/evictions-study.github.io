# ==========================================================================
# Various functions for eviction analysis
# ==========================================================================
if(!require(pacman)) install.packages('pacman')

options(
  scipen=10, # avoid scientific notation
  width=system("tput cols", intern=TRUE), # set width to terminal window
  tigris_use_cache = TRUE
  )

#
# Name abbreviations
# --------------------------------------------------------------------------

name_patterns <-
  c(' [A-Z]. | [A-Z] |JR.|JR|.SR|SR|DOE.*|,$|, $| \"[^>]+\" |\\s*\\([^\\)]+\\)|\\s\\s| AKA.*| AND.*| III| II| I|ESTATE|DEL |OCCUPANT|ABU')

corp_patterns <- c('CAFE|LAB|SECURITY|SERVICE|PHARMACY|COMMERCIAL|LLC|PRINTING|VENTURE|CARRY|CARRYOUT| INC|FOOD|GROUP|SALOON|COMMUNITY|KINGDOM|COVER ME|TATTOO|ROOM|BOAT|TACKLE')

