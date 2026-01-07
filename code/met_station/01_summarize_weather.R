# =============================================================================
# Name:           01_summarize_weather.R
# Description:    summarizes rainfall data from the Coal Oil Point Reserve NOAA weather station

# Author(s):      Francis Joyce

# Inputs: csv in data/NOAA_weather_station        
# Outputs:        figures of cumulative annual rainfall        
# 
# Notes:         This script is intended to summarize all years, not a single year 
#                 
# =============================================================================

library(tidyverse)
library(janitor)
library(cowplot)
library(ggrepel)
library(scales)


#define function for calculating "day of water year"
# (from here: https://stackoverflow.com/questions/55525965/is-there-a-r-function-to-calculate-day-of-water-year)
dowy = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

#test
dowy(as.Date("2025-09-30"))

#read in NOAA daily summaries
daily_summaries <- read_csv(file = "data/NOAA_weather_station/NOAA_daily_summaries_USW00053152_full_2025-12-09.csv") %>% 
  clean_names() %>% 
  #create columns for month and year
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         #create column for "day of water year"
         dowy = dowy(date)) %>% 
  #create column for wateryear
  mutate(wy = case_when(
    month >= 10 ~ year + 1,
    .default = year
  ))

# cumulative rainfall plots ----

#create data frame with cumulative annual rainfall by year
ytd <- daily_summaries %>%
  group_by(wy, .drop = FALSE) %>%
  mutate(cumulative_rain = cumsum(prcp)) %>% 
  ungroup() %>%
  # create column with "year" label that will be plotted on last day of wy
  mutate(year_label = case_when(
    month == 9 & day == 30 ~ year
  )) %>% 
  filter(wy >= 2018)


ytd_all_time <- daily_summaries %>%
  group_by(wy, .drop = FALSE) %>%
  mutate(cumulative_rain = cumsum(prcp)) %>% 
  ungroup() %>%
  # create column with "year" label that will be plotted on last day of wy
  mutate(year_label = case_when(
    month == 9 & day == 30 ~ year
  )) %>% 
  #filter out 2008 since that was a partial year, missed rainy season
  filter(wy > 2008)

#create data frame of dates for first day in each month to use as tick marks & labels. 
#Note that 2016 is a leap year.
tickmarks <- date(
  c(
    "2015-10-01",
    "2015-11-01",
    "2015-12-01",
    "2016-01-01",
    "2016-02-01",
    "2016-03-01",
    "2016-04-01",
    "2016-05-01",
    "2016-06-01",
    "2016-07-01",
    "2016-08-01",
    "2016-09-01"
  )
)

#extract day of year (doy) using yday and also extract month for tick mark labels
doy <- tibble(mon = month(tickmarks, label = T),
              dowy = dowy(as.Date(tickmarks)))


#create plot
cumulative_curves <-  ggplot(data = ytd,
                             aes(
                               x = dowy,
                               y = cumulative_rain,
                               group = wy,
                               color = wy
                             )) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = doy$dowy,
                     labels = doy$mon,
                     limits = c(0, 400)) +
  scale_y_continuous(breaks = seq(0,30, by = 5), expand = c(0,1)) +
  #  geom_text(aes(x = JD + 5, y = cum_rain, label = year_label), hjust = 0) +
  geom_text_repel(aes(x = dowy + 4, y = cumulative_rain, label = year_label), direction = c("y"), hjust = 0, size = 5) +
  #scale_colour_viridis_c() +
  #scale_color_manual(values = cal_palette("kelp1")) +
  #scale_x_date(date_labels = "%b", date_breaks = "months") +
  labs(
    #title = "Cumulative Annual Rainfall (2018-2025 water years)",
    caption = "Coal Oil Point Reserve \n Lat: 34.41386, Lon: -119.8802",
    y = "Cumulative rainfall (in)",
    x = "Date"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5,
                               hjust = -0.2),
    panel.grid = element_line(color = "white"), 
    legend.position = "none"
  )
#scale_y_continuous(limits = c(0,300))

cumulative_curves

ggsave(cumulative_curves, 
       filename = paste("figures/rainfall/cumulative_annual_rainfall",
                        format(Sys.time(), "%Y-%m-%d"),
                        ".pdf"),
       
       
       width = 6.5,
       height = 5, 
       units = "in")


#ggplotly(cumulative_curves)

# all years - cumulative curves
cumulative_curves_all <-  ggplot(data = ytd_all_time,
                             aes(
                               x = dowy,
                               y = cumulative_rain,
                               group = wy,
                               color = wy
                             )) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = doy$dowy,
                     labels = doy$mon,
                     limits = c(0, 400)) +
  scale_y_continuous(breaks = seq(0,30, by = 5), expand = c(0,1)) +
  #  geom_text(aes(x = JD + 5, y = cum_rain, label = year_label), hjust = 0) +
  geom_text_repel(aes(x = dowy + 4, y = cumulative_rain, label = year_label), direction = c("y"), hjust = 0, size = 5) +
  #scale_colour_viridis_c() +
  #scale_color_manual(values = cal_palette("kelp1")) +
  #scale_x_date(date_labels = "%b", date_breaks = "months") +
  labs(
    #title = "Cumulative Annual Rainfall (2018-2025 water years)",
    caption = "Coal Oil Point Reserve \n Lat: 34.41386, Lon: -119.8802",
    y = "Cumulative rainfall (in)",
    x = "Date"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(vjust = 5.5,
                               hjust = -0.2),
    panel.grid = element_line(color = "white"), 
    legend.position = "none"
  )
#scale_y_continuous(limits = c(0,300))

cumulative_curves_all

ggplotly(cumulative_curves_all)


# simple annual bar graph ----

annual_rainfall <- daily_summaries %>% 
  group_by(wy) %>% 
  summarize(total_rainfall = sum(prcp),
            ndays = n()) %>%   
  #filter years
  filter(wy >2017 & wy <2026) 

annual_rainfall_all <- daily_summaries %>% 
  group_by(wy) %>% 
  summarize(total_rainfall = sum(prcp),
            ndays = n()) %>%   
  #filter years
  filter(wy >2008) 

#create bar plot
fig_annual_rain <- ggplot(data = annual_rainfall, aes(x = wy, y = total_rainfall)) +
  geom_col(fill = "darkblue") +
  #geom_line() +
  scale_y_continuous(expand = c(0,NA), breaks = breaks_width(5)) +
  scale_x_continuous(breaks = breaks_width(1), expand = c(0,0)) +
  theme_cowplot() +
  ylab("Total rainfall (in)") +
  xlab("Water year")

fig_annual_rain

#save to file
#ggsave(plot = fig_annual_rain, filename = "figures/annual_rainfall.png")


fig_annual_rain_all <- ggplot(data = annual_rainfall_all, aes(x = wy, y = total_rainfall)) +
  geom_col(fill = "darkblue") +
  #geom_line() +
  scale_y_continuous(expand = c(0,NA), breaks = breaks_width(5)) +
  scale_x_continuous(breaks = breaks_width(1), expand = c(0,0)) +
  theme_cowplot() +
  ylab("Total rainfall (in)") +
  xlab("Water year")

fig_annual_rain_all
