#############################################
## 2024 TOMST pre-processing from Hana 
## 2025-01-14
##############################################

library(myClim)
library(stringr)
library(data.table)
library(lubridate)
library(arrow)
library(dplyr)
library(terra)

list_path <- list.files("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2021-2024/2024_raw", full.names = TRUE, pattern = "\\.csv$")
list_files <- list.files("Calculating growth chamber parameters/data/QHI temperature data/TOMST data/2021-2024/2024_raw", full.names = FALSE, pattern = "\\.csv$")

locality_name <-  str_extract(list_files, "TOMST[:digit:]+_QHI") ## other way of naming it, if we have a metada file for example

files_table <- data.table(path = list_path,
                          locality_id = locality_name ,  
                          data_format = "TOMST")

locality_metadata <-  data.table(locality_id = locality_name ,  
                                 tz_offset  = -7*60)

tms <- mc_read_data(files_table,locality_metadata)

mc_info_count(tms) #which returns the number of localities, loggers and sensors in myClim object
mc_info(tms)# returning data frame with summary per sensor
mc_info_meta(tms)# returning the data frame with locality metadata
mc_info_clean(tms) #returning the data frame with cleaning log

## let's check the most appropriate  soiltype, default to universal now
tms.calc = mc_calc_vwc(tms, soiltype = "universal")
###Error in names[[i]] : subscript out of bounds### <- You are here. 

## virtual sensor with growing and freezing degree days
tms.calc = mc_calc_gdd(tms.calc, sensor = "TMS_T3",)
tms.calc = mc_calc_fdd(tms.calc, sensor = "TMS_T3")

## virtual sensor to estimate snow presence from 2 cm air temperature 
tms.calc = mc_calc_snow(tms.calc, sensor = "TMS_T2")

## aggregates all those sensors to daily (monthly also possible) values # choose between minium percentile
daily.tms = mc_agg(tms.calc,fun=c("mean","min","max"),period = "day",min_coverage=1,use_utc = F)

## export the object out of the MyClim framework
export_dt = data.table(mc_reshape_long(daily.tms),use_utc = F)
export_dt[, serial_number:=NULL] ##removing useless col
export_dt[, datetime := ymd(datetime)] ## :=  creates or update a column in data.table, here we switch to a lubridate format with ymd
export_dt[, month := month(datetime)] ## extracting the month
export_dt[, day := day(datetime)] ## extracting the day
export_dt[, week := week(datetime)] ## extracting the week

# remove sensor with different names and filter to just TMS data
export_dt = export_dt %>% filter(locality_id != "TOMST37_QHI") %>% filter(grepl("TMS", sensor_name)) 

daily_values = export_dt[,.(mean_value = mean(value,na.rm=T)),
                          by=.(month,day,sensor_name,height,week)] # na.rm=T remove incomplete days 

write_parquet(export_dt, "micro_climate_sensors")
write_parquet(daily_values, "micro_climate_sensors_daily")

######## Hana making a shape file with the locations of the TOMST on the island ######
## Arctic DEM
dem = rast("C:/Users/hanats/Downloads/42_18_10m_v4.1.tar/42_18_10m_v4.1/42_18_10m_v4.1_dem.tif")
island = vect("D:/TreelineShapefiles/Herschel.shp") %>% project(., dem)

dem.clip = crop(dem, island, mask = T)
dem.clip = project(dem.clip, "epsg:32607", res = 10)
dem.clip

writeRaster(dem.clip, "H:/coding_challenge/arctic_dem_10m.tif")

## stations
points = read.csv("H:/coding_challenge/team_shrub_coding_challenge/station_metadata.csv")
points = vect(points, geom = c("Lon", "Lat"), crs = "epsg:4326")
points.proj = project(points, dem.clip)

plot(dem.clip)
plot(points.proj, add=T)
