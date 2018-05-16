# Author: Lucy Lee
# Updated: 5/16/2018
# This script creates a new table that combines watershed annual summary
# tables found in ArcGIS SWAT outputs (.std). A year column is added
# to the new table and monthly and annual total rows are removed.
# The output is saved as a CSV.
#
# Notes: The leap_year function from lubridate used here is an older
# version (current documentation is for leap.year). If R is ever updated,
# the syntax of this function may need to be changed to the newer version.

library(lubridate)

# Read in file
fn <- 'D:/SWAT/output.std'
con <- file(description = fn, open = 'r')
lines <- readLines(con)

# Index lines that indicate the start of a watershed annual summary table
title_lines <- grep('Annual Summary for Watershed in year', lines, value = FALSE)

# Index lines that begin actual data rows in each watershed table
start_table <- title_lines + 5

# Column names from the input file
input_col_names <- c('day', 'prec', 'surq', 'latq', 'gwq', 'percolate',
                     'tileq', 'sw', 'et', 'pet', 'water_yield', 'sed_yield',
                     'no3_surq', 'no3_latq', 'no3_perc', 'no3_crop',
                     'n_organic', 'p_soluble', 'p_organic', 'tileno3')


# Iterate over file using the known indices of table beginnings
annual_tables <- lapply(start_table, function(x) {
  # Check to see if table has data ('1' means there is no data in the table)
  if (lines[x] != '1') {
    # Read in the data at table_start index for 379 lines -- this will capture leap and non-leap years
    table <- read.table(fn, skip = x - 1, nrows = 379, fill = TRUE, col.names = input_col_names, stringsAsFactors = FALSE)
    
    # Coerce all columns to numeric while retaining DF data type
    table <- as.data.frame(sapply(table, as.numeric))
    
    # Retain only the complete cases (ie, get rid of extra rows that may have been included in the 379 rows)
    table <- table[complete.cases(table), ]
    
    # Add year column to df (get year from last row of table$day)
    table$year <- tail(table$day, 1)
    
    # Remove monthly and annual total rows
    if (leap_year(as.numeric(tail(table$day, 1))) == FALSE) {
      table <- table[-c(32, 61, 93, 124, 156, 187, 219, 251, 282, 314, 345, 377, 378), ]
    }
    else if (leap_year(as.numeric(tail(table$day, 1))) == TRUE) {
      table <- table[-c(32, 62, 94, 125, 157, 188, 220, 252, 283, 315, 346, 378, 379), ]
    }
  }
})


# Combine yearly dataframes into one
df_all <- do.call(rbind, annual_tables)
#row.names(df_all) <- NULL

# Write output as CSV
write.csv(x = df_all, file = "D:/SWAT/clean_swat_output.csv", quote = FALSE, row.names = FALSE)
