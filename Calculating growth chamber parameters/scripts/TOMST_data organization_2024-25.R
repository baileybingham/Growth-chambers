
#############################################
## Reviewing TOMST temp curves
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-14
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tdyr, dplyr, etc. 

####Import and join tomst data####
tomst24<- read.csv(("Calculating growth chamber parameters/data/TOMST data/QHI_location_temperature_daily.csv"))
tomst25<- read.csv(("Calculating growth chamber parameters/data/TOMST data/QHI_location_temperature_daily_2025.csv"))
tomst <- bind_rows(tomst24,tomst25)

tomst <- tomst %>% filter (between (month, 6,9))

# create graph
tomst$datetime <- as.Date(tomst$datetime)

ggplot(tomst, aes(x = datetime, y = value)) +
  geom_point()+
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, linetype = "solid") +
  facet_wrap(~year, scales = "free_x")+
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "2 week") + 
  labs(
    title = "TOMST Data",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

