#########################################################
##########    Comparing 2025 Growth Chamber     #########
##########     Settings to Actual Temps         #########
##########         By Bailey Bingham            #########
##########           May 4, 2026                #########
#########################################################

library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)


##### IMPORT ALL DATA #####
#import large chamber actual data
lrg_act <-read_csv("2. Comparing 2025 GC to QHI temps/data/2025_largechamber_hobodata_raw.csv")

#import settings 
set<- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025.csv")[, 1:10][1:115,] 


#### CALCULATE DAILY HOBO SUMMARY DATA #####
lrg_sum <- lrg_act %>%
  # Convert DateTime to POSIXct
  mutate(datetime_pdt = mdy_hms(datetime_pdt),
             date = as.Date(datetime_pdt)) %>%
  group_by(date) %>%
  # Calculate metrics, removing NA values
  summarise(
    mean = mean(temp_c, na.rm = TRUE),
    max = max(temp_c, na.rm = TRUE),
    min = min(temp_c, na.rm = TRUE), 
    # Count rows with light and multiply by 1/6 (10 mins / 60 mins)
    hrs_light = sum(light_intensity_lux > 0, na.rm = TRUE) * (10/60),
    .groups = "drop") %>%
    select (date, mean, max, min, hrs_light)

lrg_sum <- lrg_sum [1:115,] 

#### FILTER SET SO IT ONLY INCLUDES DATE, MIN, MAX, MEAN ####
set_sum <- set %>%
  mutate(
    act_date = dmy(act_date)) %>%
  rename(date = act_date, mean = set_mean, max = set_max, min = set_min) %>%
  select (date, mean, max, min, hrs_light)


#### JOIN DATASETS ####
forplot <- bind_rows(lrg_sum %>% mutate(source = "Actual"), set_sum %>% mutate(source = "Settings"))


#### GRAPHING ####
ggplot(forplot, aes(x = date)) +
  # Shading Ribbon (alpha makes it transparent)
  geom_ribbon(aes(ymin = min, ymax = max, fill= source), alpha = 0.2, color = NA) +
  geom_line(aes(y = mean, color = source), linewidth = 1) +
  geom_line(aes(y = hrs_light * 0.625, linetype = source), color = "midnightblue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . / 0.625, name = "Daylight Hours", breaks = seq(0, 24, 4))
  ) +
  labs(title = "Actual vs set GC temps °C",
       subtitle = "",
       x = "Month",
       y = "Temperature (°C)",
       fill = "Temp",
       color = "Temp",
       linetype = "Light Hrs") +
  coord_cartesian(ylim = c(-3, 15)) +
  scale_x_date(date_labels = "%b%d", date_breaks = "7 days") +
  theme_minimal()
