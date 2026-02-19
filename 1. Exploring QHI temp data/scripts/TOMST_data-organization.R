#############################################
## Reorganizing TOMST data 2021-2025
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-14
##############################################

#### Download Packages####
#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(readr)


#### Import and correct 2021-2024 TOMST data from Hana ####
tomst24<- read.csv("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2021-2024/TOMST_2021-2024_fromHana.csv")
#make it so single digit locality ids have a zero in front of the number (i.e. TOMST01)
tomst24$locality_id <- sub("TOMST([0-9]_)", "TOMST0\\1", tomst24$locality_id)
#rearrange locality ID so it counts up from 01-39
tomst24 <- tomst24 %>% arrange(as.numeric(str_extract(locality_id, "[0-9]+"))) 
tomst24$datetime <- as.Date(tomst24$datetime)
head(tomst24)


#### Import and prepare metadata from Hana ####
metadata<-read.csv("Calculating growth chamber parameters/data//QHI temperature data/TOMST data/2021-2024/station_metadata.csv") %>%
  rename_with(tolower) %>% # make the column headers lowercase
  mutate(
    field_id = str_to_upper(field_id),   #make the values in the field ID capitalized
    field_id = str_replace(field_id, "TOMST([0-9])(_|$)", "TOMST0\\1\\2") #add a 0 before single digit TOMST
  )

# CHECK: Does this data include every TOMST sensor and all years of data?
#check_years <- tomst24 %>%
#  #Extract the year and create a temporary 'presence' column
#  mutate(year = year(as.Date(datetime)), 
#         present = 1) %>%
#  #Keep only unique locality/year combinations
#  distinct(locality_id, year, present) %>%
#  # 3. Pivot the years into columns
#  pivot_wider(
#    names_from = year, 
#    values_from = present, 
#    values_fill = 0
#  )
#This dataset seems to be missing TOMSTs 2,5,11 and 37


#### Organize 2021-2024 TOMST data ####
tomst24clean <- tomst24 %>%
  #seperate QHI as the location, and the TOMST ID out into seperate columns
  separate_wider_delim(locality_id, 
                       delim = "_", 
                       names = c("id", "location")) %>%
  
  #remove TMS_ from sensor_name
  mutate(sensor_name = str_remove(sensor_name, "TMS_"))%>%

  # seperate sensor and measurement
  separate_wider_delim(cols = sensor_name, 
                     delim = "_", 
                     names = c("sensor", "measurement"))%>%
  
  ##seperate out the sensor and the measurement, create description and unite with measurement immediately
  mutate(descrip = case_match(sensor, 
            "T1" ~ "soil_temp", "T2" ~ "nearsoil_temp", 
            "T3" ~ "air_temp", "moist" ~ "soil_moisture"),
    measurement = paste(descrip, measurement, sep = "_"))%>%

  # add year column and calculate day of year (doy)
  mutate(datetime = as.POSIXct(datetime)) %>%
  mutate(
    year  = year(datetime),
    month = month(datetime),
    day   = day(datetime),
    doy   = yday(datetime)) %>%
  
  #Rearrange the headings to your exact specification
  select(location, id, sensor, datetime, 
         year, month, day, doy, measurement, value)
  
  
  

#### Join 2021-2024 dataset with metadata ####

  # join tomst and metadata where 'id' in tomst matches 'field id' in the metdata set
  tomst24meta <- tomst24clean %>%
       left_join(metadata, by = c("id" = "field_id")) %>%
  
  # reorganize columns
       select(location, serial_no, id, lat, lon, elevation,
              nearest.phenocam, datetime, year, 
              month, day, doy, 
         measurement, value) %>%
  
  #rearrange so measurement is spread and values are underneath
       pivot_wider(
       # define id columns
        id_cols = c(location, serial_no, id, lat, lon, elevation, 
                    nearest.phenocam, datetime, year,
                    month, day, doy,), 
       # use values in 'measurement' for new headers
        names_from = measurement, 
       # fill measurement columns with values from the 'value' column
        values_from = value
        ) 


#### write as csv ####
  #write_csv(tomst_full, "cleaned_tomst_data_2021-2024.csv")
  #getwd()
  

###############################Adding 2025 TOMST data ###################################################

####Import and correct 2025 tomst data####
tomst25<- read.csv("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2025/agg_daily_tomst_data_2025.csv")
tomst25$datetime <- as.Date(tomst25$datetime)

# CHECK: Does this data include every TOMST sensor and all years of data?
#check_years <- tomst25 %>%
#  #Extract the year and create a temporary 'presence' column
#  mutate(year = year(as.Date(datetime)), 
#         present = 1) %>%
# #Keep only unique locality/year combinations
#  distinct(locality_id, year, present) %>%
#  # 3. Pivot the years into columns
#  pivot_wider(
#    names_from = year, 
#    values_from = present, 
#    values_fill = 0) %>%
#  #rearrange locality ID so it counts up from 01-39
#  arrange(as.numeric(str_extract(locality_id, "[0-9]+"))) 


#### join to the original 2021-2024 tomst for recleaning.
tomstALL <- bind_rows(tomst24, tomst25)%>%
  select(locality_id, sensor_name, height, datetime, value) %>%
  distinct()

### check for duplicates
tomstALL %>%
  group_by(locality_id, sensor_name, height, datetime, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
### There are duplicates of the ID variables, but not the values???

#filter out 1 TOMST and 1 sensor to inspect and double check
tomst1day <- tomstALL %>%
  filter(locality_id == "TOMST01_QHI" & sensor_name == "TMS_T1_mean")

########WHY ARE THE VALUES DIFFERENT???#############

#######################CHECK TO MAKE SURE ALL DATA IS THERE#########################

# CHECK: Does this data include every TOMST sensor and all years of data?
check_years <- tomstALL %>%
  #Extract the year and create a temporary 'presence' column
  mutate(year = year(as.Date(datetime)), 
         present = 1) %>%
  #Keep only unique locality/year combinations
  distinct(locality_id, year, present) %>%
  # 3. Pivot the years into columns
  pivot_wider(
    names_from = year, 
    values_from = present, 
    values_fill = 0) %>%
  #rearrange locality ID so it counts up from 01-39
  arrange(as.numeric(str_extract(locality_id, "[0-9]+"))) 

#CHECK: Which data sets are bringing unique values?
# Which values are in tomst25 but missing from tomst24
only_in_tomst25 <- setdiff(tomst25$locality_id, tomst24$locality_id)
print(only_in_tomst25)
#Only in TOMSST25 include: "TOMST05_QHI" "TOMST37_QHI" and three unknown datasets: "data_96641225_2025_08_05_0.csv" "data_96641225_2025_08_05_1.csv"
#"data_96641225_2025_08_05_2.csv"

# Which values are in tomst24 but missing from tomst25
only_in_tomst24 <- setdiff(tomst24$locality_id, tomst25$locality_id)
print(only_in_tomst24)
#Only in TOMST24 include: "TOMST08_QHI" "TOMST14_QHI" "TOMST27_QHI" "TOMST29_QHI" "TOMST34_QHI". i.e. we are missing 2025 data for these. 

##################################################################################################
### MISSING DATA: With both datasets together we are missing all data for TOMST02 and TOMST11,
### and 2025 data for TOMST08, TOMST14, TOMST27, TOMST29 and TOMST34
### There is also some unknown data from 2025 that we need to remove, since it doesn't have a locality_ID
#########################################################################################################

#remove data with unknown locality
tomstALL <- tomstALL %>%
  filter(str_starts(locality_id, "TOMST"))


#### Organize TOMST data ####
tomst <- tomstALL %>%
  #seperate QHI as the location, and the TOMST ID out into seperate columns
  separate_wider_delim(locality_id, 
                       delim = "_", 
                       names = c("id", "location")) %>%
  
  #remove TMS_ from sensor_name
  mutate(sensor_name = str_remove(sensor_name, "TMS_"))%>%
  
  # seperate sensor and measurement
  separate_wider_delim(cols = sensor_name, 
                       delim = "_", 
                       names = c("sensor", "measurement"))%>%
  
  ##seperate out the sensor and the measurement, create description and unite with measurement immediately
  mutate(descrip = case_match(sensor, 
                              "T1" ~ "soil_temp", "T2" ~ "nearsoil_temp", 
                              "T3" ~ "air_temp", "moist" ~ "soil_moisture"),
         measurement = paste(descrip, measurement, sep = "_"))%>%
  
  # add year column and calculate day of year (doy)
  mutate(datetime = as.POSIXct(datetime)) %>%
  mutate(
    year  = year(datetime),
    month = month(datetime),
    day   = day(datetime),
    doy   = yday(datetime)) %>%
  
  #Rearrange the headings to your exact specification
  select(location, id, sensor, datetime, 
         year, month, day, doy, measurement, value)

#### Prepare metadata for joining ####
metadata <- metadata %>%
  rename_with(tolower) %>% # make the column headers lowercase
  mutate(
    field_id = str_to_upper(field_id),   #make the values in the field ID capitalized
    field_id = str_replace(field_id, "TOMST([0-9])(_|$)", "TOMST0\\1\\2") #add a 0 before single digit TOMST
  )

#### Join both datasets ####

# join tomst and metadata where 'id' in tomst matches 'field id' in the metdata set
tomst_full <- tomst %>%
  left_join(metadata, by = c("id" = "field_id")) %>%
  
  # reorganize columns
  select(location, id, lat, lon, elevation,
         nearest.phenocam, datetime, year, 
         month, day, doy, 
         measurement, value) 
  
  #rearrange so measurement is spread and values are underneath
tomst_full <- tomst_full %>% pivot_wider(
    # define id columns
    id_cols = c(location, id, lat, lon, elevation, 
                nearest.phenocam, datetime, year,
                month, day, doy,), 
    # use values in 'measurement' for new headers
    names_from = measurement, 
    # fill measurement columns with values from the 'value' column
    values_from = value
  ) 


