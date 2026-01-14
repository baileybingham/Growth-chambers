#############################################
## Organizing climate data into one file
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-13
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tdyr, dplyr, etc. 

####Import and join climate data####
clim20<- read.csv("Calculating growth chamber parameters/data/2020_climate_daily_YT_QHI.csv")
clim21<- read.csv("Calculating growth chamber parameters/data/2021_climate_daily_YT_QHI.csv")
clim22<- read.csv("Calculating growth chamber parameters/data/2022_climate_daily_YT_QHI.csv")
clim23<- read.csv("Calculating growth chamber parameters/data/2023_climate_daily_YT_QHI.csv")
clim24<- read.csv("Calculating growth chamber parameters/data/2024_climate_daily_YT_QHI.csv")
clim25<- read.csv("Calculating growth chamber parameters/data/2025_climate_daily_YT_QHI.csv")

joined<-bind_rows(clim20,clim21,clim22,clim23,clim24,clim25) ##bind all data together
climALL<- joined %>% select(Station.Name, Date.Time, Year, Month, Day, Max.Temp...C.,Min.Temp...C.,Mean.Temp...C.)

#remove all months but June-September
climALL <- climALL %>% filter (between (Month, 6,9))

# create graph
climALL$Date.Time <- as.Date(climALL$Date.Time)

ggplot(climALL, aes(x = Date.Time, y = Mean.Temp...C.)) +
  geom_point()+
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, linetype = "solid") +
  facet_wrap(~Year, scales = "free_x")+
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "1 months") + 
  labs(
    title = "Daily Mean Temperature (2020-2025)",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

#group day and month for averaging
climALL <- climALL %>% unite("Date", Month, Day, sep = "-")

