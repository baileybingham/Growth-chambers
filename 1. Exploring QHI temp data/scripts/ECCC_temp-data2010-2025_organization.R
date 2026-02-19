#############################################
## ECCC QHI weather station temperature curves since 2010
## Bailey E. Bingham
## bbingh02@student.ubc.ca
## 2025-01-13
##############################################

#install.packages("tidyverse")
library(tidyverse) #includes ggplot, tdyr, dplyr, etc. 

####Import and join climate data####
data_path <- "Calculating growth chamber parameters/data/QHI temperature data/ECCC temp data/"
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
#climALL <- climALL %>% filter (between (Month, 6,9))

# create graph
climALL$Date.Time <- as.Date(climALL$Date.Time)

ggplot(climALL, aes(x = Date.Time, y = Mean.Temp...C.)) +
  geom_point()+
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  facet_wrap(~Year, scales = "free_x")+
  theme_minimal() +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_x_date(date_labels = "%b %d", date_breaks = "1 months") + 
  labs(
    title = "Daily Mean Temperature (1996-2025)",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

####Create a 30 year average plot####
library(lubridate)
climALL_rev <-climALL %>%
  # Create dummy column: force all years to 2026
  mutate(datetime_rev = update(as.Date(Date.Time), year = 2026)) 

ggplot(climALL_rev, aes(x = datetime_rev, y = Mean.Temp...C.)) +
  geom_point(alpha = 0.1, color = "black", size = 1)+
  #geom_smooth(aes(group = Year, color = Year),  se = FALSE, linewidth = 0.7) + #show individual years
  geom_smooth(color = "blue", linewidth = 1.5, se = FALSE) + #show trendline
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_color_viridis_c() + 
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") + 
  labs(
    title = "Daily Mean Temperature (1996-2025)",
    subtitle = "Blue line represents the 30-year average trend",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )


#### Create a dataframe with the 30 year average
QHI_30avg <- climALL_rev %>%
  group_by(Month, Day) %>%
  summarise(
    Mean_Temp = mean(`Mean.Temp...C.`, na.rm = TRUE),
    Max_Temp = max(`Max.Temp...C.`, na.rm = TRUE),
    Min_Temp = min(`Min.Temp...C.`, na.rm = TRUE),
    n_years    = sum(!is.na(Mean.Temp...C.)),
    .groups = "drop")%>%
    ungroup()

# Add a 'dummy' date for easier graphing (uses a non-leap year by default)
QHI_30avg$dummydate <- as.Date(paste(2023, QHI_30avg$Month, QHI_30avg$Day, sep = "-"))

## graph 30 year average
ggplot(QHI_30avg, aes(x = dummydate, y = Mean_Temp)) +
  geom_line(color = "grey", size = 1)+
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), alpha = 0.2, fill = "grey") +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_color_viridis_c() + 
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") + 
  labs(
    title = "QHI- ECCC Daily Mean Temperature (1996-2025)",
    subtitle = "Ribbon represents temperature extremes. Data collected by ECCC. Note that there is a lot of missing data.",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

#dir.create("export_data")
write.csv(QHI_30avg,"export_data/ECCC_dailymeantemp_1996-2025.csv" , row.names = FALSE)

