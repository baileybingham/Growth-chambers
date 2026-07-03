#######################################################
########## Comparing TOMST to ECCC Data ##############
##########    By Bailey Bingham            ############
######################################################

library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)

###################
#### TOMST ########
###################

### IMPORT AGGREGATED TOMST DATA (it includes all years) ###
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
names(tomst) <- tolower(gsub(".", "", names(tomst), fixed = TRUE))


### Graph an average of all TOMST years onto one graph, to get an idea of the range
ggplot(tomst, aes(x = datetime, y = t3_mean, color = id)) +
  geom_line(alpha = 0.5) + 
  theme_minimal() +
  facet_wrap(~id)+
  labs(title = "Time Series by Sensor ID",
       x = "Date",
       y = "Mean Temperature (T3)") 

### TOMST_21 and _22 look kinda weird, but they are right near each other on 
### the island so the data is probably accurate.

### Average all stations to get one daily average for the whole island ###
tomst_daily_avg <- tomst %>%
  group_by(datetime) %>%
  summarise(
    # Average of the mean temperatures across all stations
    qhi_mean = mean(t3_mean, na.rm = TRUE),
    # The absolute lowest temperature recorded by ANY station that week
    qhi_min  = min(t3_min, na.rm = TRUE),
    # The absolute highest temperature recorded by ANY station that week
    qhi_max  = max(t3_max, na.rm = TRUE))

tomst_daily_avg  <- tomst_daily_avg %>% 
  mutate (year = year(datetime))

### Graph the average to see if it makes sense
ggplot(tomst_daily_avg, aes(x = datetime, y = qhi_mean)) +
  geom_line(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), alpha = 0.2, fill = "darkgrey") +
  scale_x_date(
    date_breaks = "1 month",   # Set marks at every month
    date_labels = "%b %Y"      # Format: %b = Abbr Month, %y = year (e.g., Jan 22)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "QHI average temperature (all years)",
       subtitle = "Grey shaded area shows the extreme temperatures for each day",
       x = "Date",
       y = "Mean Daily Air Temperature") 

# facet grid by year
tomst_daily_avg <- tomst_daily_avg %>%
  mutate(dummydate = as.Date(format(datetime, "2023-%m-%d")))
ggplot(tomst_daily_avg, aes(x = dummydate, y = qhi_mean)) +
  geom_point()+
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  facet_wrap(~year)+
  theme_minimal() +
  scale_y_continuous(breaks = seq(-40, 40, by = 10)) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "TOMST Daily Mean Temps (July 2022 - August 2025)",
    x = "Month",
    y = "Mean Temperature (°C)"
  )

### Calculate the average tomst measurements over all years 
tomst_daily_avg <-tomst_daily_avg %>%
  # Create dummy column: force all years to 2026
  mutate(dummydate = update(as.Date(datetime), year = 2026)) 

ggplot(tomst_daily_avg, aes(x = dummydate, y = qhi_mean)) +
  geom_point(alpha = 0.1, color = "black", size = 1)+
  geom_smooth(color = "blue", linewidth = 1.5, se = FALSE) + #show trendline
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week", expand = expansion(add = 0.2)) + 
  labs(
    title = "Daily Mean Temperature TOMST (July 2022- August 2025)",
    subtitle = "Blue line represents the average trend since the TOMST were installed in July 2022",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

#### TOMST: Max, min, mean graph with ribbon ###
# create data set with averaged tomst values
tomstavg <- tomst_daily_avg %>%
  group_by(dummydate) %>%
  summarise(
    # Average of the mean temperatures across all stations
    qhi_mean = mean(qhi_mean, na.rm = TRUE),
    # The absolute lowest temperature recorded by ANY station that week
    qhi_min  = min(qhi_min, na.rm = TRUE),
    # The absolute highest temperature recorded by ANY station that week
    qhi_max  = max(qhi_max, na.rm = TRUE))

ggplot(tomstavg, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), fill = "grey", alpha = 0.3) +
  geom_line(aes(y = qhi_mean), color = "grey20", size = 0.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "All TOMSTs averaged with max and min values (July 2022- August 2025)",
       x = "Date",
       y = "Mean Daily Air Temperature (°C)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#### Lets do it again, but with rolling averages over 3 days
rollavg <-tomstavg %>%
  mutate(
    # Rolling Average: Smoothes data, but lowers peaks
    rolling_avg = rollmean(qhi_mean, k = 6, fill = NA, align = "center"))

ggplot(rollavg, aes(x = dummydate, y = rolling_avg)) +
  geom_line(size = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), alpha = 0.2, fill = "darkgrey") +
  scale_x_date(
    date_breaks = "1 month",   # Set marks at every month
    date_labels = "%b %Y"      # Format: %b = Abbr Month, %y = year (e.g., Jan 22)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "QHI Rolling Average, avg over 3 days",
       x = "Date",
       y = "Mean Daily Air Temperature (°C)")

################
##### ECCC #####
################

### IMPORT AND JOIN ECCC DATA ###
data_path <- "1. Exploring QHI temp data/data/ECCC temp data/raw/"
desired_cols <- c(
  "Station.Name", "Date.Time", "Year", "Month", "Day",
  "Max.Temp...C.", "Min.Temp...C.", "Mean.Temp...C.") #define columns I want to keep
# Load all files, select desired columns, and combine into one data frame
eccc <- list.files(data_path, pattern = "*.csv", full.names = TRUE) %>%
  map_df(~read.csv(.x) %>% 
           select(all_of(desired_cols)), 
         .id = "source_file")
names(eccc) <- tolower(gsub(".", "", names(eccc), fixed = TRUE))
eccc <- eccc %>% rename(qhi_mean = meantempc, qhi_max = maxtempc, qhi_min = mintempc)

# Graph the ECCC data by year so we can see each year individually
eccc$datetime <- as.Date(eccc$datetime)

ggplot(eccc, aes(x = datetime, y = qhi_mean)) +
  geom_point()+
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  facet_wrap(~year, scales = "free_x")+
  theme_minimal() +
  scale_y_continuous(breaks = seq(-40, 40, by = 10)) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    title = "Daily Mean Temperature (1996-2025)",
    x = "Month",
    y = "Mean Temperature (°C)"
  )

### graph daily ECCC data together to get a trendline for all 30 years
eccc_rev <-eccc %>%
  # Create dummy column: force all years to 2026
  mutate(dummydate = update(as.Date(datetime), year = 2026)) 

ggplot(eccc_rev, aes(x = dummydate, y = qhi_mean)) +
  geom_point(alpha = 0.1, color = "black", size = 1)+
  geom_smooth(color = "blue", linewidth = 1.5, se = FALSE) + #show trendline
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) + 
  scale_color_viridis_c() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") + 
  labs(
    title = "Daily Mean Temperature (1996-2025)",
    subtitle = "Blue line represents the 30-year average trend",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

#### ECCC: Max, min, mean graph with ribbon ###
# create data set with averaged tomst values
eccc_avg <- eccc_rev %>%
  group_by(dummydate) %>%
  summarise(
    # Average of the mean temperatures across all stations
    eccc_qhi_mean = mean(qhi_mean, na.rm = TRUE),
    # The absolute lowest temperature recorded by ANY station that week
    eccc_qhi_min  = min(qhi_min, na.rm = TRUE),
    # The absolute highest temperature recorded by ANY station that week
    eccc_qhi_max  = max(qhi_max, na.rm = TRUE))

ggplot(eccc_avg, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin =  eccc_qhi_min, ymax =  eccc_qhi_max), fill = "grey", alpha = 0.3) +
  geom_line(aes(y =  eccc_qhi_mean), color = "grey20", size = 0.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "30 year ECCC avg with max and min values",
       x = "Date",
       y = "Mean Daily Air Temperature (°C)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


####################
#### COMBINED ######
####################

#### Combine original TOMST and ECCC data (before averaging) to graph trend with real data

#Select ECCC columns needed
eccc_com <- eccc_rev %>%
  select (dummydate, qhi_mean, qhi_min, qhi_max, year) %>%
  mutate (source = "ECCC" )
#Select TOMST columns needed
tomst_com <- tomst_daily_avg %>%
   mutate (year = year(datetime), source = "TOMST" ) %>%
   select (dummydate, qhi_mean, qhi_min, qhi_max, year, source) 
#combine them
cdata <- bind_rows(eccc_com, tomst_com)
## graph with data
ggplot(cdata, aes(x = dummydate, y = qhi_mean)) +
  geom_point(aes(color =source), alpha = 0.1, size = 1)+
  geom_smooth(aes(group = source, color = source),  se = FALSE, linewidth = 0.9) + 
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-40, 40, by = 5), limits = c(-40, 40))+ 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(
    date_labels = "%b %d", 
    date_breaks = "7 days",
    expand = c(0, 0),
    limits = c(as.Date("2026-01-01"), NA)
  ) + 
  labs(
    title = "Average Temperature (°C) on QHI",
    subtitle = "ECCC includes data since 1996 from the weather station
TOMST includes daily average from 40 sensors since July 2022",
    x = "Month and Day",
    y = "Mean Temperature (°C)"
  )

# graph with mins and max ribbons
ddata<-left_join(eccc_avg, rollavg, by = "dummydate") 
# graph it
ggplot(ddata, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  #ECCC Ribbon (Grey Background)
  geom_ribbon(aes(ymin = eccc_qhi_min, ymax = eccc_qhi_max),fill = "darkgrey", alpha = 0.3) +
  #ECCC Mean Line
  geom_line(aes(y = eccc_qhi_mean), color = "grey20", size = 0.8) +
  #TOMST Ribbon 
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), fill = "darkred", alpha = 0.3) +
  #TOMST Mean Line
  geom_line(aes(y = qhi_mean), color = "darkred", size = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(
    date_labels = "%b %d", 
    date_breaks = "7 days",
    expand = c(0, 0),
    limits = c(as.Date("2026-01-01"), NA)
  ) + 
  labs(title = "QHI ECCC weather station VS. TOMST average, max and min temps",
       subtitle = "Grey: ECCC (30-yr) | Red: TOMST (2022-2025)",
       x = "Date",
       y = "Temperature (°C)")


# what about with rolling 2023 averages?
ggplot(ddata, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  #ECCC Ribbon (Grey Background)
  geom_ribbon(aes(ymin = eccc_qhi_min, ymax = eccc_qhi_max),fill = "darkgrey", alpha = 0.3) +
  #ECCC Mean Line
  geom_line(aes(y = eccc_qhi_mean), color = "grey20", size = 0.8) +
  #TOMST Ribbon 
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), fill = "darkred", alpha = 0.3) +
  #TOMST rolling average Line
  geom_line(aes(y = rolling_avg), color = "darkred", size = 0.8) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "QHI ECCC weather station historical temps compared to TOMST rolling average and max and min temps",
       subtitle = "Grey: ECCC (30-yr) | Red: rolling average and actual max and mins of the TOMST (2022-2025)",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()

# what about with rolling 2023 averages over just the growing season
ggplot(ddata, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  #ECCC Mean Line
  geom_line(aes(y = eccc_qhi_mean), color = "blue", size = 0.8) +
  #TOMST Ribbon 
  geom_ribbon(aes(ymin = qhi_min, ymax = qhi_max), fill = "darkred", alpha = 0.2) +
  #TOMST rolling average Line
  geom_line(aes(y = rolling_avg), color = "darkred", size = 0.8) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "QHI ECCC weather station historical temps compared to TOMST rolling average and max and min temps",
       subtitle = "Grey: ECCC (30-yr) | Red: rolling average and actual max and mins of the TOMST (2022-2025)",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()+
  coord_cartesian(ylim = c(-15, 35),  xlim = c(as.Date("2026-05-15"), as.Date("2026-10-10")))




