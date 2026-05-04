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
des<- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025.csv")[, 1:10][1:115,] 

set<- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025_3.csv")[1:115, c(2, 10, 11, 12)]

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

#### FILTER DESIRED SO IT ONLY INCLUDES DATE, MIN, MAX, MEAN ####
des_sum <- des %>%
  mutate(
    act_date = dmy(act_date)) %>%
  rename(date = act_date, mean = set_mean, max = set_max, min = set_min) %>%
  select (date, mean, max, min, hrs_light)

#### FILTER SET SO IT ONLY INCLUDES DATE, MIN, MAX, MEAN ####
set_sum <- set %>%
  mutate(
    act_date = dmy(act_date)) %>%
  rename(date = act_date, mean = set_mean, max = set_max, min = set_min) %>%
  select (date, mean, max, min)

#### JOIN DATASETS ####
forplot <- bind_rows(lrg_sum %>% mutate(source = "Actual"), des_sum %>% mutate(source = "Desired"))


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


#### JOIN DATASETS (including settings) ####
forplot2 <- bind_rows(lrg_sum %>% mutate(source = "Actual"), des_sum %>% mutate(source = "Desired"), set_sum %>% mutate(source = "Settings"))


#### GRAPHING ####
ggplot(forplot2, aes(x = date)) +
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

#### CREATING A NEW DATATABLE FOR COMPARISON ####
all_wide <- forplot2 %>%
  pivot_wider(
    names_from = source,
    values_from = c(mean, max, min, hrs_light),
    names_glue = "{tolower(substring(source, 1, 3))}_{.value}"
  )

#### Let's model the error to see if we can make a tend line for how off 
#### the actual temps are at different settings. 
analysis <- all_wide %>%
  mutate(
    # Error: How far off was the result from the target?
   error = act_mean - set_mean)

model <- lm(error ~ set_mean, data = analysis)
summary(model)
# At zero C set_mean, the predicted error is: 3.6C
# for every 1 degree increase in temp, error decreases by 0.19C

# Graph it
ggplot(analysis, aes(x = set_mean, y = error)) +
  geom_point(alpha = 0.5, color = "steelblue") + # Actual data points
  geom_smooth(method = "lm", color = "firebrick") + # The trend line
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Growth chamber error trend",
    subtitle = "At 0°C for the set mean temp, the predicted error is: 3.6°C. 
For every 1°C increase in temp, error decreases by 0.19°C",
    x = "Set temp",
    y = "Error (actual - setting)"
  ) +
  theme_minimal()
