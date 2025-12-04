# =============================================================================
# Name:           venoco_levelogger_combine.R
# Description:    combines compensated water level data files and converts to water surface elevation.

# Originally written 2025-11-18
# Author(s):      Francis Joyce, Claire WS

# Inputs:         
# Outputs:        
# 
# Notes:          
#                 
# =============================================================================

## 0. load libraries ----
library(tidyverse)
library(janitor)
library(cowplot)
library(scales)
library(measurements)

#interactive plots for exploration
library(plotly)

# define start and end of 2025 water year

wy_2025_start <- as.POSIXct("2024-10-01 00:00:00")
wy_2025_end <- as.POSIXct("2025-09-30 23:59:59")


# 1. logger elevations ----

#ordered from "downstream" (lower) to upstream (higher)

#note that these elevations are in feet
logger_elev <- read_csv("data/leveloggers/logger_elevations_2025wy.csv")


# Pier ----

pier_2025_wy_a <- read_csv(file = "data/leveloggers/Pier/PIER_03.19.24_05.10.25_Compensated.csv", skip = 13) %>% 
  clean_names() %>% 
  select(-ms)

pier_2025_wy_b <- read_csv(file = "data/leveloggers/Pier/PIER_05.10.25_05.20.25_Compensated.csv") %>% 
  clean_names() %>% 
  select(-x1)

pier_2025_wy_c <- read_csv(file = "data/leveloggers/Pier/PIER_05.20.24_08.29.25_Compensated.csv", skip = 13) %>% 
  clean_names() %>% 
  select(-ms)

pier_2025_wy_d <- read_csv(file = "data/leveloggers/Pier/PIER_08.29.25_11.13.25_Compensated.csv") %>% 
  clean_names()  %>% 
  select(-x1)

pier_20240319_20251113 <- bind_rows(pier_2025_wy_a, pier_2025_wy_b, pier_2025_wy_c, pier_2025_wy_d) %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time,
    #adjust water level by logger elevation
    wse = level_ft + 3.4) 

#declutter
remove(pier_2025_wy_a, pier_2025_wy_b, pier_2025_wy_c, pier_2025_wy_d)

#plot Pier water levels over 2025 water year
Pier_wse_fig <- ggplot(data = pier_20240319_20251113, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(title = "Devereux Slough Pier") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
                   date_minor_breaks = "1 month",
                   date_labels = "%b %Y",
                   limits = c(wy_2025_start, wy_2025_end))

Pier_wse_fig

ggsave(filename = "figures/Pier_wse_wy25_DRAFT.png", plot = Pier_wse_fig)


# Venoco bridge ----

#note level is in meters and temp is in C
#not sure what ms column is
venoco_2025_wy_a <- read_csv("data/leveloggers/Venoco_Bridge/Venoco_11.13.24_05.10.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>%
  select(-ms) %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = conv_unit(level, "m", "ft")
)
  
# already in feet  
venoco_2025_wy_b <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_05.10.25_05.20.25_Compensated.csv") %>% 
  clean_names() %>%
  select(datetime:temperature)
  
venoco_2025_wy_c <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_05.20.25_08.29.25_Compensated.csv", skip = 11) %>% 
  clean_names() %>% 
  select(-ms) %>% 
  mutate(date = mdy(date),
         datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
         comp_level_ft = conv_unit(level, "m", "ft")
  )

#already in feet
venoco_2025_wy_d <- read_csv(file = "data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Compensated.csv") %>% 
  clean_names() %>% 
  select(datetime:temperature)

# combine four different data frames
venoco_2024_11_13_2025_10_22 <- bind_rows(venoco_2025_wy_a, venoco_2025_wy_b, venoco_2025_wy_c, venoco_2025_wy_d) %>% 
  clean_names() %>% 
  mutate(
    #adjust water level by logger elevation
    wse = comp_level_ft + 2.838) 

#declutter
remove(venoco_2025_wy_a, venoco_2025_wy_b, venoco_2025_wy_c, venoco_2025_wy_d)

#test plot
Venoco_wse_fig <- ggplot(data = venoco_2024_11_13_2025_10_22, aes(x = datetime, y = wse)) +
  geom_line() +
  #theme_cowplot() +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(title = "Venoco Bridge (north side)") +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_datetime(date_breaks =  "2 month", 
               date_minor_breaks = "1 month",
               date_labels = "%b %Y",
               limits = c(wy_2025_start, wy_2025_end)
               #expand = c(0,0)
               )


Venoco_wse_fig 

#make interactive
ggplotly(Venoco_wse_fig)

ggsave(filename = "figures/Venoco_Bridge_wse_wy25_DRAFT_revised.pdf", plot = Venoco_wse_fig)


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
                   date_labels = "%b %Y",
                   limits = c(wy_2025_start, wy_2025_end))

Phelps_wse_fig

#combine into one figure


