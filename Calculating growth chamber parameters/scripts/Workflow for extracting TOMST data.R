#############################################
## Workflow extracting raw TOMST data
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-14
##############################################

#install.packages("myClim")
library (myClim)
library (tidyverse)
library (lubridate)

####Import new TOMST data####
fpath<- list.files("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2025/2025_raw", full.names = TRUE, pattern = "\\.csv$")


###Identify which part of the file name should be the locality_ID###
# This regex looks for: QHI (Group 1), TOMST (Group 2), and 39 (Group 3)
# based on my file name format: ..._QHI_TOMST_39.csv
extracted_ids <- str_replace(
  basename(fpath), 
  ".*_([A-Z]+)_(TOMST)_(\\d+)\\.csv", 
  "\\2\\3_\\1"
)

# 3. Create the files_table required by myClim
my_files_table <- data.frame(
  path = fpath,
  locality_id = extracted_ids,
  data_format = "TOMST"
)

# 4. Import the data using the custom locality names
tomst <- mc_read_data(files_table = my_files_table)

#tomst25 <- mc_read_files(data_path, dataformat_name = "TOMST")

#clean and aggregate using myClim
tomst <- mc_prep_clean(tomst)
tomst_daily <- mc_agg(tomst, fun = c("mean", "max", "min"), period = "day")

# Convert to long format
tomst_daily <- mc_reshape_long(tomst_daily)
head(tomst_daily)

#### Doing some checks ####
# Does every TOMST sensor have all the year data?
check_years <- tomst_daily %>%
  #Extract the year and create a temporary 'presence' column
  mutate(year = year(as.Date(datetime)), 
         present = 1) %>%
  #Keep only unique locality/year combinations
  distinct(locality_id, year, present) %>%
  # 3. Pivot the years into columns
  pivot_wider(
    names_from = year, 
    values_from = present, 
    values_fill = 0
  )

unique(tomst_daily$locality_id) 

###Only 33 labelled TOMST (missing 02,08,11,14,27,29,34), plus 3 unknown datasets with only 2025 data...
###How many TOMST are there actually?? 

#### write as csv ####
write_csv(tomst_daily, "agg_daily_tomst_data_2025.csv")
getwd()
