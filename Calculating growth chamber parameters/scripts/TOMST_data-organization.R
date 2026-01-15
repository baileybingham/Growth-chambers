#############################################
## Reorganizing TOMST data 2021-2024
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-14
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(readr)

####Import and join tomst data####
tomst<- read.csv("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2021-2024/TOMST_2021-2024_fromHana.csv")
metadata<-read.csv("Calculating growth chamber parameters/data//QHI temperature data/TOMST data/2021-2024/station_metadata.csv")
head(tomst)


#### Organize TOMST data ####
tomst <- tomst %>%
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
        mutate(field_id = str_to_upper(field_id)) #make the values in the field ID capitalized
  
  #### Join both datasets ####

  # join tomst and metadata where 'id' in tomst matches 'field id' in the metdata set
  tomst_full <- tomst %>%
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
        ) %>%
        drop_na()

  #### write as csv ####
  #write_csv(tomst_full, "cleaned_tomst_data_2021-2024.csv")
  getwd()
  