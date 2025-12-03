## 2025-11-18
## Francis Joyce
## This script is for combining compensated water level data files and converting to water surface elevation


## 0. load libraries ----
library(tidyverse)
library(janitor)
library(cowplot)
library(scales)
library(measurements)

# 1. logger elevations ----

#note that these elevations are in feet

logger_elev <- read_csv("data/leveloggers/logger_elevations_2025wy.csv")

# Pier ----



# Venoco bridge ----

#note level is in meters and temp is in C
#not sure what ms column is
venoco_2025_wy_a <- read_csv("data/leveloggers/Venoco_Bridge/Venoco_11.13.24_05.10.25_Compensated.csv", skip = 11)

venoco_2025_wy_b <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_05.20.25_08.29.25_Compensated.csv", skip = 11)

venoco_2024_11_13_2025_08_29 <- bind_rows(venoco_2025_wy_a, venoco_2025_wy_b) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time,
    #adjust water level by logger elevation
    wse = level_ft + 2.838) 

#test plot

Venoco_wse_fig <- ggplot(data = venoco_2024_11_13_2025_08_29, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(title = "Venoco Bridge (north side)") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
               date_minor_breaks = "1 month",
               date_labels = "%b %Y")


Venoco_wse_fig 

ggsave(filename = "figures/Venoco_Bridge_wse_wy25_DRAFT.png", plot = Venoco_wse_fig)


# East Bridge ----

# we have data up to 11/13/24 due to a levelogger malfunction. instrument was
# replaced on 10/22/25. data collection at this location continues from then on.

# Phelps Creek - Marymount Bridge  ----

phelps_wy_25_a <- read_csv(file = "data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_03.19.24_05.10.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    #convert level from m to ft
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time,
    #adjust water level by logger elevation
    wse = level + 9.99) 

  
Phelps_wse_fig <- ggplot(data = phelps_wy_25_a, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water elevation (ft)") +
  xlab("Date") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y")

Phelps_wse_fig
