#############################################
## ECCC QHI weather station temperature curves since 2010
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-13
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tdyr, dplyr, etc. 

####Import and join climate data####
clim10 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2010_climate_daily_YT_QHI.csv")
clim11 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2011_climate_daily_YT_QHI.csv")
clim12 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2012_climate_daily_YT_QHI.csv")
clim13 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2013_climate_daily_YT_QHI.csv")
clim14 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2014_climate_daily_YT_QHI.csv")
clim15 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2015_climate_daily_YT_QHI.csv")
clim16 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2016_climate_daily_YT_QHI.csv")
clim17 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2017_climate_daily_YT_QHI.csv")
clim18 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2018_climate_daily_YT_QHI.csv")
clim19 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2019_climate_daily_YT_QHI.csv")
clim20 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2020_climate_daily_YT_QHI.csv")
clim21 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2021_climate_daily_YT_QHI.csv")
clim22 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2022_climate_daily_YT_QHI.csv")
clim23 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2023_climate_daily_YT_QHI.csv")
clim24 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2024_climate_daily_YT_QHI.csv")
clim25 <- read.csv("Calculating growth chamber parameters/data/ECCC temp data/2025_climate_daily_YT_QHI.csv")

joined <- bind_rows(clim10, clim11, clim12, clim13, clim14, clim15, clim16, clim17,
                    clim18, clim19, clim20, clim21, clim22, clim23, clim24, clim25) ##bind all data together

## getting an error that Spd of max gust includes characters and integers, so we have to remove that.
# Put all data frames in a vector of names
df_names <- paste0("clim", 10:25)

# Use lapply to remove the unwanted column, then bind_rows
joined <- bind_rows(
  lapply(df_names, function(x) {
    df <- get(x)                     # get the data frame by name
    df %>% select(-Spd.of.Max.Gust..km.h.)  # drop the column
  })
)
climALL<- joined %>% select(Station.Name, Date.Time, Year, Month, Day, Max.Temp...C.,Min.Temp...C.,Mean.Temp...C.)

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



