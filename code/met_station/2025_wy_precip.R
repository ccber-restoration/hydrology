# =============================================================================
# Name:           2025_wy_precip.R
# Description:    summarizes rainfall data from the COPR weather station for the 2025 water year

# Author(s):      Francis Joyce

# Inputs:         csv in data/NOAA_weather_station        
# Outputs:        daily rainfall figure, summaries for text     
# 
# Notes:         
#                 
# =============================================================================

library(tidyverse)
library(janitor)
library(cowplot)
library(ggrepel)
library(scales)
library(plotly)

# load functions to assign water year (standard and local)
source("code/met_station/function_assign_water_years.R")

#read in NOAA daily summaries
daily_summaries <- read_csv(file = "data/NOAA_weather_station/NOAA_daily_summaries_USW00053152_full.csv") %>%
  clean_names() %>%
  #create columns for month and year
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    #create column for "day of water year"
    dowy = dowy(date),
    dolwy = dolwy(date)
  ) %>%
  #create column for wateryears
  mutate(
    wy = case_when(month >= 10 ~ year + 1, .default = year),
    lwy = case_when(month >= 9 ~ year + 1, .default = year)
  )

water_year_2025 <- daily_summaries %>% 
  filter(lwy == 2025) %>% 
  # calculate cumulative precipitation
  mutate(cumulative_prcp = cumsum(prcp))

# why only 364 days??
start_date <- as.Date("2024-09-01")
end_date <- as.Date("2025-08-31")
all_expected_dates <- seq(from = start_date, to = end_date, by = "day")

missing_date <- setdiff(all_expected_dates, water_year_2025$date)

as.Date(missing_date)
# 2025-04-28 is missing

#how many days with more than 0.1 inch of precip

rainy_days <- water_year_2025 %>% 
  filter(prcp >= 0.1)

# 15 days with more than 0.1 inch of rain
very_wet_days <- water_year_2025 %>% 
  filter(prcp >= 1)

# 3 days with more than 1 inch of rain

# single rainiest day was January 26 2025 with 2.47 inches

# total precip
sum(water_year_2025$prcp)

# total rainfall for water year = 10.98 inches
fig_precip_2025 <- ggplot(data = water_year_2025, aes(x = date, y = prcp)) +
  geom_col(color = "darkblue") +
  ylab("Daily rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 months", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = scales::breaks_width(0.25)) +
  theme_cowplot()

fig_precip_2025

ggsave(fig_precip_2025, 
       filename = paste0("figures/rainfall/year_8_report/2024_2025_wy_precipitation_",
                         format(Sys.time(), "%Y-%m-%d"),
                         ".png"),
       width = 6,
       height = 4.5,
       units = "in",
       bg = "white"
       )
