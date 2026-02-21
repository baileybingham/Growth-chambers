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
    values_from = value) %>%
  drop_na()

### Average all stations to get one daily average for the whole island ###
tomst_daily_avg <- tomst %>%
  group_by(datetime, year, month, day, doy) %>%
  summarise(
    # Average of the mean temperatures across all stations
    QHI_mean = mean(T3_mean, na.rm = TRUE),
    # The min low temperature recorded by any stations
    QHI_min  = min (T3_min, na.rm = TRUE),
    # The max high temperature recorded by any stations
    QHI_max  = max(T3_max, na.rm = TRUE))

### Average all years under a 2023 dummy date (averaged mins and maxes as well, but you could also use absolute min or max)
tomst_4y_avg <- tomst_daily_avg %>%
  group_by(month, day) %>%
  summarise(
    QHI_4y_mean = mean(QHI_mean, na.rm = TRUE),
    QHI_4y_min = mean(QHI_min, na.rm = TRUE),
    QHI_4y_max = mean(QHI_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # create the dummy 2023 date for plotting
  mutate(dummydate = dmy(paste(day, month, "2023", sep = "-"))) %>%
  arrange(dummydate)%>%
  drop_na()%>%
  #Rearrange the headings 
  select(dummydate, QHI_4y_mean,QHI_4y_min,QHI_4y_max)

### Import 2025 Growth Chamber Settings
gc25 <- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025.csv") %>%
  # Remove columns where all values are NA
  select(where(~ !all(is.na(.)))) %>%
  # Remove rows where all values are NA
  filter(if_any(everything(), ~ !is.na(.))) %>%
  # add column with a dummy date, where the year is 2023, to match the ECCC dummy date
  mutate(dummydate = dmy(paste(sim_date, "2023")))

gc25_freezer <- gc25 %>%
  # Force the dataset to include every day from Apr 1 to Nov 30
  complete(dummydate = seq.Date(as.Date("2023-04-01"), 
                                 as.Date("2023-11-30"), 
                                 by = "day")) %>%
  # if a temp is missing, set it to -20, because the plants were in the freezer
  mutate(
    set_mean = replace_na(set_mean, -20),
    set_max  = replace_na(set_max, -20),
    set_min  = replace_na(set_min, -20)
  )


###### Bind all datasets so they can be graphed together #####

forplot <- bind_rows(
  gc25_freezer %>% select(dummydate, mean = set_mean, min = set_min, max = set_max) %>% mutate(Source = "Growth Chamber Settings"),
  tomst_4y_avg %>% select(dummydate, mean = QHI_4y_mean, min = QHI_4y_min, max = QHI_4y_max) %>% mutate(Source = "TOMST 4yr avg"),
  eccc %>% select(dummydate, mean = eccc_mean_temp, min = eccc_min_temp, max = eccc_max_temp) %>% mutate(Source = "ECCC-QHI 30yr avg")
)

#### Plot comparison ####
ggplot(forplot, aes(x = dummydate, fill = Source, color = Source)) +
  # Shading Ribbon (alpha makes it transparent)
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Temperature Comparison",
       subtitle = "Comparing our 2025 growth chamber settings to the last 4 years of TOMST data
and all the ECCC data available from QHI over the last 30 years.",
       x = "Month",
       y = "Temperature (°C)",
       fill = "Dataset",
       color = "Dataset") +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25"))) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()


####################################################################
### Using ECCC 30yr AVG to recommend growth chamber settings ######
###################################################################
eccc <- eccc %>% mutate(dummydate = ymd(dummydate))
eccc_rec <- eccc %>%
  mutate(dummydate_clean = ymd(dummydate)) %>% 
  filter(month(dummydate_clean) >= 6& month(dummydate_clean) <= 9) %>%
  mutate(
    # Create 7-day 'steps' for the protocol
    step_period = as.Date(cut(dummydate_clean, breaks = "7 days"))
  )

#calculate the mean temps for your chamber protocol
chamber_protocol <- eccc_rec %>%
  group_by(step_period) %>%
  summarize(
    recommended_temp = round(mean(eccc_mean_temp, na.rm = TRUE),0)
  ) %>%
  # Apply chamber constraints (-2C to 30C)
  mutate(recommended_temp = pmax(-2, pmin(30, recommended_temp)))

print(chamber_protocol,n=30)
#A tibble: 27 × 2
#step_period recommended_temp ACTUAL 
#<date>                    <Rec_mean_temp>
#1 2023-05-01                -2
#2 2023-05-08                -2
#3 2023-05-15                -2
#4 2023-05-22                 0
#5 2023-05-29                 0
#6 2023-06-05                 3
#7 2023-06-12                 4
#8 2023-06-19                 6
#9 2023-06-26                 8
#10 2023-07-03                9
#11 2023-07-10                9
#12 2023-07-17                10
#13 2023-07-24                10
#14 2023-07-31                10
#15 2023-08-07                9
#16 2023-08-14                7
#17 2023-08-21                6
#18 2023-08-28                5
#19 2023-09-04                5
#20 2023-09-11                3
#21 2023-09-18                2
#22 2023-09-25                0
#23 2023-10-02               -2
#24 2023-10-09               -2
#25 2023-10-16               -2
#26 2023-10-23               -2
#27 2023-10-30               -2


ggplot(forplot, aes(x = dummydate, fill = Source, color = Source)) +
  # Original Ribbon and Lines
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  
  # NEW: Evidence-Based Step-Up Protocol Layer
  # We use 'inherit.aes = FALSE' so it doesn't look for 'Source' in the protocol data
  geom_step(data = chamber_protocol, 
            aes(x = step_period, y = recommended_temp), 
            color = "black", 
            linewidth = 1.2, 
            inherit.aes = FALSE) +
  
  # Formatting
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Temperature Comparison",
       subtitle = "Comparing 2025 chamber settings and field data to our New Recommended Protocol (Black Line)",
       x = "Month",
       y = "Temperature (°C)",
       fill = "Dataset",
       color = "Dataset") +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25"))) +
  theme_minimal()

#### I want to update the reccommended temps, so that there are less temperature changes. 

protocol <- tribble(
  ~step_period, ~recommended_temp,
  "2023-05-04",  -20,
  "2023-05-11",  -20,
  "2023-05-18",   -1,
  "2023-05-25",   -1,
  "2023-06-01",    1,
  "2023-06-08",    3,
  "2023-06-15",    5,
  "2023-06-22",    7,
  "2023-06-29",    9,
  "2023-07-06",   10,
  "2023-07-13",   10,
  "2023-07-20",   10,
  "2023-07-27",   10,
  "2023-08-03",   10,
  "2023-08-10",    9,
  "2023-08-17",    7,
  "2023-08-24",    7,
  "2023-08-31",    5,
  "2023-09-07",    5,
  "2023-09-14",    3,
  "2023-09-21",    1,
  "2023-09-28",   -1,
  "2023-10-05",   -1,
  "2023-10-12",  -20,
  "2023-10-19",  -20,
  "2023-10-26",  -20,
  ) %>%
  mutate(step_period = ymd(step_period))

ggplot(forplot, aes(x = dummydate, fill = Source, color = Source)) +
  #geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  
  # Add the manual protocol as black line
  geom_step(data = protocol, 
            aes(x = step_period, y = recommended_temp), 
            color = "black", 
            linewidth = 1, 
            inherit.aes = FALSE) +
  
  geom_point(data = protocol, 
             aes(x = step_period, y = recommended_temp), 
             color = "black", 
             size = 1.5, 
             inherit.aes = FALSE) +
  
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Temperature Comparison",
       subtitle = "Black line shows an updated mean temp regime for the growth chambers, to better match field temp averages.",
       x = "Month",
       y = "Temperature (°C)") +
  coord_cartesian(xlim = c(as.Date("2023-05-01"), as.Date("2023-10-30")),
                  ylim = c(-25, 20)) + 
  theme_minimal()

