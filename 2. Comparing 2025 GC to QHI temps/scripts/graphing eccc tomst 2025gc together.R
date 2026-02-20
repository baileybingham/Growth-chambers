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
    # The mean low temperature recorded by all stations
    QHI_min  = mean (T3_min, na.rm = TRUE),
    # The mean high temperature recorded by all stations
    QHI_max  = mean(T3_max, na.rm = TRUE))

### Average all years under a 2023 dummy date
tomst_4y_avg <- tomst_daily_avg %>%
  group_by(month, day) %>%
  summarise(
    QHI_4y_mean = mean(QHI_mean, na.rm = TRUE),
    QHI_4y_min = mean (QHI_min, na.rm = TRUE),
    QHI_4y_max = mean (QHI_max, na.rm = TRUE),
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
  # Force the dataset to include every day from May 1 to Nov 30
  complete(dummydate = seq.Date(as.Date("2023-05-01"), 
                                 as.Date("2023-11-30"), 
                                 by = "day")) %>%
  # 2. Fill in the gaps: if a temp is NA (missing day), set it to -20
  mutate(
    set_mean = replace_na(set_mean, -20),
    set_max  = replace_na(set_max, -20),
    set_min  = replace_na(set_min, -20),
    # Optional: Fill the ID column so the line doesn't break in the plot
    gc_id = replace_na(gc_id, "control") 
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
  #geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
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
  coord_cartesian(xlim = c(as.Date("2023-05-01"), as.Date("2023-11-01"))) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()
