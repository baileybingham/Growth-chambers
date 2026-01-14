#############################################
## ECCC QHI weather station temperature curves since 2010
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-13
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tdyr, dplyr, etc. 

####Import and join climate data####
data_path <- "Calculating growth chamber parameters/data/ECCC temp data/"
desired_cols <- c(
  "Station.Name", "Date.Time", "Year", "Month", "Day",
  "Max.Temp...C.", "Min.Temp...C.", "Mean.Temp...C.") #define columns I want to keep
# Load all files, select desired columns, and combine into one data frame
climALL <- list.files(data_path, pattern = "*.csv", full.names = TRUE) %>%
  map_df(~read.csv(.x) %>% 
           select(all_of(desired_cols)), 
         .id = "source_file")

###above is a simpler way of loading all 30 files, but they could also have been loaded as below:
#clim10 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2010_climate_daily_YT_QHI.csv")
#clim11 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2011_climate_daily_YT_QHI.csv")
#clim12 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2012_climate_daily_YT_QHI.csv")

#remove all months but June-September
climALL <- climALL %>% filter (between (Month, 6,9))

# create graph
climALL$Date.Time <- as.Date(climALL$Date.Time)

ggplot(climALL, aes(x = Date.Time, y = Mean.Temp...C.)) +
  geom_point()+
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "solid") +
  facet_wrap(~Year, scales = "free_x")+
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "1 months") + 
  labs(
    title = "Daily Mean Temperature (2020-2025)",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )



