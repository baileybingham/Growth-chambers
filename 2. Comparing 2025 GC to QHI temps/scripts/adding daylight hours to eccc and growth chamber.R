######################################################
##########   All Growth Chamber Settings     #########
##########   against ECCC 30y avg            #########
##########     By Bailey Bingham            ##########
##########     Mar 01, 2026                 ##########
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

### Import 2025 Growth Chamber Settings
gc25 <- read_csv("2. Comparing 2025 GC to QHI temps/data/GC_settings_2025_2.csv") %>%
  # Remove columns where all values are NA
  select(where(~ !all(is.na(.)))) %>%
  # Remove rows where all values are NA
  filter(if_any(everything(), ~ !is.na(.))) %>%
  # add column with a dummy date, where the year is 2023, to match the ECCC dummy date
  mutate(dummydate = dmy(paste(sim_date, "2023")))

gc25_freezer <- gc25 %>%
  # Force the dataset to include every day from Apr 1 to Nov 30 for all treatments
  complete(
    gc_id = c("control", "heatwave", "extended"), 
    dummydate = seq.Date(as.Date("2023-04-01"), 
                         as.Date("2023-11-30"), 
                         by = "day")
  ) %>%
  # if a temp is missing, set it to -20, because the plants were in the freezer
  mutate(
    set_mean = replace_na(set_mean, -20),
    set_max  = replace_na(set_max, -20),
    set_min  = replace_na(set_min, -20)
  )


###### Bind all datasets so they can be graphed together #####

forplot <- bind_rows(
  gc25_freezer %>% select(dummydate, gc_id,  mean = set_mean, min = set_min, max = set_max) %>% mutate(Source = "Growth Chamber Settings", Group = gc_id),
  eccc %>% select(dummydate, mean = eccc_mean_temp, min = eccc_min_temp, max = eccc_max_temp) %>% mutate(Source = "Field Data", Group = "ECCC-QHI 30yr avg")
)

forplot <- forplot %>%
  mutate(Group = fct_relevel(Group, "ECCC-QHI 30yr avg", "extended", "heatwave", "control"))

#### Plot comparison ####
ggplot(forplot, aes(x = dummydate, fill = Group, color = Group)) +
  # Shading Ribbon (one for each Group)
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.15, color = NA) +
    # Mean lines
  geom_line(aes(y = mean), linewidth = 1) +
    # Reference line at 0°C
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  # Reference line at August 17th
  geom_vline(xintercept = as.Date("2023-08-17"), color = "gold", linewidth = 1, linetype = "dotted") +
    # Date formatting
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    # Custom Colors (optional but recommended for clarity)
  scale_color_manual(values = c("control" = "blue", "heatwave" = "red", 
                                "extended" = "orange", "ECCC-QHI 30yr avg" = "black")) +
  scale_fill_manual(values = c("control" = "blue", "heatwave" = "red", 
                               "extended" = "orange", "ECCC-QHI 30yr avg" = "black")) +
    labs(title = "Comparison of growth chamber settings vs. 30 year average temp on Qikiqtaruk",
       subtitle = "Ribbons show min/max range. Lines show mean temperature.
Note that daylight hours began to decrease from 24hrs of daylight on August 17th, in line with light conditions in the field.",
       x = "Month/ Simulated date",
       y = "Temperature (°C)")+
       #fill = "Growth Chamber Setting",
      # color = "Growth Chamber Setting") 
    # Focus on the active period (zoom without removing data)
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25")),
                  ylim = c(-30, 30)) + 
  theme_minimal()


###############################################################################
####### Add light settings and field data #####################################
###############################################################################

#Import QHI field daylight hours
QHI_light<-read_csv("2. Comparing 2025 GC to QHI temps/data/QHI_daylight_hours.csv") %>%
  mutate(
    dummydate = dmy(date),
    # Convert time object to seconds, then to decimal hours
    decimal_hrs = as.numeric(hms(act_daylight_hrs)) / 3600,
    Source = "Field Data", 
    Group = "QHI"
  ) %>%
  select(dummydate, decimal_hrs, Source, Group)


# get growth chamber light settings and convert to decimal Hours
gc25_light <- gc25 %>%
  # Filter for just one treatment since settings are identical
  filter(gc_id == "control") %>%
  complete(
    dummydate = seq.Date(as.Date("2023-04-01"), as.Date("2023-11-30"), by = "day"),
    fill = list(set_hrs_light = 0)
  ) %>%
  transmute(
    dummydate, 
    decimal_hrs = set_hrs_light, 
    Source = "Growth Chamber Settings", 
    Group = "Growth Chamber"
  )

# Combine datasets
light_plot_data <- bind_rows(QHI_light, gc25_light) 

####################################################################
####### Graph field light vs GC light settings #####################
####################################################################

#reference for creating shading for field data
field_ref <- light_plot_data %>%
  filter(Source == "Field Data") 

# Plot
ggplot(light_plot_data, aes(x = dummydate, y = decimal_hrs)) +
  geom_ribbon(data = field_ref, 
              aes(ymin = 0, ymax = decimal_hrs, 
                  fill = "QHI Photoperiod", color = "QHI Photoperiod"), 
              alpha = 0.2) + 
  geom_ribbon(data = field_ref, 
              aes(ymin = decimal_hrs, ymax = 24), 
              fill = "midnightblue", alpha = 0.2, color = NA) +
  geom_line(data = filter(light_plot_data, Source == "Growth Chamber Settings"), 
            aes(color = "Growth Chamber Settings", fill = "Growth Chamber Settings"), 
            linewidth = 1) + 
  scale_fill_manual(values = c("QHI Photoperiod" = "gold", 
                               "Growth Chamber Settings" = "transparent")) +
  scale_color_manual(values = c("QHI Photoperiod" = "transparent", 
                                "Growth Chamber Settings" = "orange")) +
    labs(title = "Comparing Growth Chamber Settings to QHI Photoperiod",
       x = "Month", y = "Daylight Hours",
       fill = "Data Source", color = "Data Source") + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4), expand = c(0,0)) +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25"))) +
  theme_minimal() +
  # This override cleans up the legend boxes to show only the line/fill as needed
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

##############################################################################
####### Graph field temps and photoperiod together ###########################
##############################################################################

#Combine QHI light and temp data together
qhi_combined <- eccc %>%
  left_join(QHI_light_avg, by = "dummydate")

#plot field light and temp data on the same graph
ggplot(qhi_combined, aes(x = dummydate)) +
  # QHI photoperiod (y-axis scaled and shifted so 0 to 24 matches -30 to 30)
  geom_line(aes(y = (decimal_hrs * 2.5) - 30, color = "Daylight Hours"), 
            linewidth = 1, linetype = "solid") +
  # ECCC Temperature 
  geom_ribbon(aes(ymin = eccc_min_temp, ymax = eccc_max_temp), fill = "grey", alpha = 0.2) +
  geom_line(aes(y = eccc_mean_temp, color = "Temperature"), linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
     # Axis Configuration
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(-40, 30),
    # Reverse the transformation for labels: (y + 30) / 2.5
    sec.axis = sec_axis(~ (. + 30) / 2.5, name = "Daylight Hours", breaks = seq(0, 24, 4))
  ) +
  scale_color_manual(values = c("Temperature" = "black", "Daylight Hours" = "gold")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25")), ylim = c(-30, 30)) +
  theme_minimal() +
  labs(title = "ECCC Temperature vs. QHI Photoperiod",
       subtitle = "Light scale adjusted so that 0 to 24hrs = -30 to 30°C",
       color = "Data")

#### With shading
ggplot(qhi_combined, aes(x = dummydate)) +
  # Light Yellow below the light boundary
  geom_ribbon(aes(ymin = -30, ymax = (decimal_hrs * 2.5) - 30), 
              fill = "gold", alpha = 0.1) + 
  # Dark Blue above the light boundary
  geom_ribbon(aes(ymin = (decimal_hrs * 2.5) - 30, ymax = 30), 
              fill = "midnightblue", alpha = 0.4) +
    # Plot temp data
  geom_ribbon(aes(ymin = eccc_min_temp, ymax = eccc_max_temp), fill = "gray80", alpha=0.8) +
  geom_line(aes(y = eccc_mean_temp), color = "black", linewidth = 1) +
  # Reference line at 0°C
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  # scaling both axes
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(-40, 30),
    sec.axis = sec_axis(~ (. + 30) / 2.5, name = "Daylight Hours", breaks = seq(0, 24, 4))
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25")), 
                  ylim = c(-30, 30)) +
  theme_minimal() +
  labs(title = "ECCC 30 year average temperature on QHI compared to daylight hours",
       subtitle = "Yellow = daylight and civil twilight | blue = night and all other twilight")


###############################################################
#### Make graph of growth chamber temp and light settings  ####
###############################################################
gc25_light_temp <- gc25 %>%
  # add 0 hours of daylight to all the days the plants were in the freezer
  complete(
    gc_id = c("control", "heatwave", "extended"), 
    dummydate = seq.Date(as.Date("2023-04-01"), 
                         as.Date("2023-11-30"), 
                         by = "day"),
    # This automatically sets light to 0 for all newly created 'missing' rows
    fill = list(set_hrs_light = 0)
  ) %>%
  # Fill in the freezer temperatures and ensure light is numeric
  mutate(
    set_mean = replace_na(set_mean, -20),
    set_max  = replace_na(set_max, -20),
    set_min  = replace_na(set_min, -20),
    light_decimal = as.numeric(set_hrs_light)
  )

# Reorder gc_id so 'control' is on top
gc25_light_temp <- gc25_light_temp %>%
  mutate(gc_id = fct_relevel(gc_id, "extended", "heatwave", "control"))

# Extract one treatment's worth of data to avoid triple-stacking the background
light_ref <- gc25_light_temp %>% 
  filter(gc_id == "extended")

### Plot GC ligth and temp
ggplot(gc25_light_temp, aes(x = dummydate)) +
  # blue from the light line to the top (35)
  geom_ribbon(data = light_ref, 
              aes(ymin = light_decimal * 1.25, ymax = 30, fill = "night"), 
              alpha = 0.4)  +
  # yellow from the bottom (-25) to the light line
  geom_ribbon(data = light_ref, 
              aes(ymin = 0, ymax = light_decimal * 1.25, fill = "day"), 
              alpha = 0.2) +
  # Ribbons for temperature ranges
  geom_ribbon(aes(ymin = set_min, ymax = set_max, fill = gc_id), 
              alpha = 0.3) +
  # Reference line at 0°C
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  # Temperature means 
  geom_line(aes(y = set_mean, color = gc_id), linewidth = 1) +
  # scaling axis
  scale_y_continuous(
    name = "Temperature (°C)",
    # sec_axis uses the inverse: divide by 1.25 to get back to 0-24
    sec.axis = sec_axis(~ . / 1.25, name = "Daylight Hours", breaks = seq(0, 24, 4))
  ) +
  scale_color_manual(
    name = "Chamber Settings",
    breaks = c("control", "heatwave", "extended", "Daylight Hours"),
    values = c("control" = "blue", "heatwave" = "red", 
               "extended" = "orange")
  ) +
  scale_fill_manual(
    name = "Chamber Settings",
    breaks = c("control", "heatwave", "extended", "Daylight Hours"),
    values = c("control" = "blue", "heatwave" = "red", "extended" = "orange", 
               "day" = "gold", "night" = "midnightblue")
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  coord_cartesian(xlim = c(as.Date("2023-05-05"), as.Date("2023-10-25")), 
                  ylim = c(-2, 30)) +
  theme_minimal() +
  labs(title = "Chamber Settings: temperature and light",
       subtitle = " Dark blue is darkness and yellow is light",
       x = "Month")


###############################################################
####  Make graph of GC settings and field light and temp   ####
###############################################################

wide_light <- light_plot_data %>%
  pivot_wider(
    id_cols = dummydate,           # Keep dummydate as the unique row identifier
    names_from = Group,            # Use QHI and GC values for new column names
    values_from = decimal_hrs,     # Fill those columns with the hour values
    names_glue = "{Group}_hrs"     # Automatically appends '_hrs' to the headers
  )

combined_plot_data <- forplot %>%
  left_join(wide_light, by = "dummydate")

# Create dual axis plot with temperature on one side and light on the other
ggplot(combined_plot_data, aes(x = dummydate)) +
  # Field light conditions
  geom_ribbon(data = filter(plot_df, gc_id == "extended"),
              aes(ymin = QHI_hrs * 1.25, ymax = 30, fill = "night"), 
              alpha = 0.4) +
  # Temp ribbons and lines
  geom_ribbon(aes(ymin = min, ymax = max, fill = Group), alpha = 0.2) +
  geom_line(aes(y = mean, color = Group), linewidth = 1) +
  # GC light lines
  geom_line(aes(y = `Growth Chamber_hrs` * 1.25, color = "GC Light"), 
            linewidth = 0.8, linetype = "solid") +
  # Reference line at 0°C
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  # Define the Scales
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . / 1.25, name = "Daylight Hours", breaks = seq(0, 24, 4))
  ) +
  # Formatting
  scale_color_manual(
    name = "Legend",
    breaks = c("control", "heatwave", "extended", "QHI Light", "GC Light"),
    values = c("control" = "blue", "heatwave" = "red", "extended" = "orange",
               "QHI Light" = "black", "GC Light" = "gold")
  ) +
  scale_fill_manual(
    name = "Legend",
    breaks = c("control", "heatwave", "extended", "day", "night"),
    values = c("control" = "blue", "heatwave" = "red", "extended" = "orange", 
               "day" = "gold", "night" = "midnightblue")
  ) +
  coord_cartesian(xlim = c(as.Date("2023-05-10"), as.Date("2023-10-05")), 
                  ylim = c(-10, 35)) +
  theme_minimal() +
  labs(title = "Comparing all GC settings to field light and temp conditions on QHI",
       x = "Month")




