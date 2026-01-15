#############################################
## Workflow extracting raw TOMST data
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-14
##############################################

#install.packages("myClim")
library (myClim)

####Import new TOMST data####
fpath<- list.files("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2025_raw", full.names = TRUE, pattern = "\\.csv$")


###Identify which part of the file name should be the locality_ID###
# This regex looks for: QHI (Group 1), TOMST (Group 2), and 39 (Group 3)
# based on your specific format: ..._QHI_TOMST_39.csv
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
tomst25 <- mc_read_data(files_table = my_files_table)

#tomst25 <- mc_read_files(data_path, dataformat_name = "TOMST")

#clean and aggregate using myClim
tomst25 <- mc_prep_clean(tomst25)
tomst25_daily <- mc_agg(tomst25, fun = c("mean", "max", "min"), period = "day")

# Convert to long format
tomst25_daily <- mc_reshape_long(tomst25_daily)
head(tomst25_daily)

## something is wrong with the file names, such that 
## the locality_ID is not downloading correctly. Ask 
## Hana what she did. Also, I am not sure how these files 
## somehow have the full data since 2021???
