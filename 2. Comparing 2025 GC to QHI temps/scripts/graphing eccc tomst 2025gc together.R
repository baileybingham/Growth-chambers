######################################################
########## Comparing 2025 Growth Chamber     #########
##########    temps to TOMST to ECCC Data    #########
##########    By Bailey Bingham             ##########
##########    Feb 19th, 2026                ##########
######################################################

library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)


##### IMPORT ALL DATA #####
### Import ECCC data
eccc<-read.csv("1. Exploring QHI temp data/data/ECCC temp data/combined_ECCC_dailymeantemp_1996-2025.csv") %>%
  drop_na()%>%
  # Read datetime as a date
  mutate(dummydate = ymd(dummydate)) %>% 
  # change column headers
  select(dummydate,Mean_Temp, Max_Temp, Min_Temp)  %>%
  rename(
    eccc_mean_temp = Mean_Temp,
    eccc_max_temp = Max_Temp,
    eccc_min_temp = Min_Temp
  )
### Import aggregated daily tomst data
tomst<-read.csv("1. Exploring QHI temp data/data/TOMST data/2025_TOMSTdata_preprocessed_daily.csv") %>%
  # Read datetime as a date
  mutate(datetime = ymd(datetime))%>%
  #seperate QHI as the location, and the TOMST ID out into seperate columns
  separate_wider_regex( #using regex so that we can account for there being two underscores in locality_id
    locality_id,
    patterns = c(
      id = ".*",      # Greedily matches everything until...
      "_",      # ...the last underscore (discarded)
      location = ".*") # Everything after that last underscore
  ) %>%
  # remove TMS for the sensors that include it
  mutate(sensor = str_remove(sensor_name, "^TMS_"), .keep = "unused") %>%
  # remove all sensors other than the air temperature data (T3)
  filter(sensor %in% c("T3_mean", "T3_min", "T3_max")) %>%
  # filter to only include 2023 data
  filter(year %in% ("2023")) %>%
  #Rearrange the headings 
  select(location, id, sensor, datetime, 
         year, month, week, day, doy, value)  %>%
  #pivot to wide format
  pivot_wider(
    # define id columns
    id_cols = c(location, id, datetime, year,
                month, week, day, doy,), 
    # use values in 'sensor' for new headers
    names_from = sensor, 
    # fill measurement columns with values from the 'value' column
    values_from = value)

### Import 2025 Growth Chamber Settings
gc25 <- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025.csv") 
