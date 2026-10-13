# ~~~~~~~~~~~~~~~~~~~~~~~~~~----
# Name:           01_summarize_weather.R
# Description:    summarizes rainfall data from the Coal Oil Point Reserve NOAA weather station

# Author(s):      Francis Joyce

# Inputs: csv in data/NOAA_weather_station        
# Outputs:        figures of cumulative annual rainfall        
# 
# Notes:         This script is intended to summarize all years, not a single year 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~----                 

# 0. Setup ----
library(tidyverse)
library(janitor)
library(cowplot)
library(ggrepel)
library(scales)
library(plotly)

# define function for calculating "day of water year"
# (from here: https://stackoverflow.com/questions/55525965/is-there-a-r-function-to-calculate-day-of-water-year)
dowy = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}


# test function
dowy(as.Date("2025-09-30"))

# define function for day of local water year (starting in September; dolwy)
dolwy = function(x, start.month = 9L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# test
dolwy(as.Date("2025-09-30"))

# 1. read in NOAA daily summaries ----
daily_rain_2025_11_30 <- read_csv(file = "data/NOAA_weather_station/NOAA_daily_summaries_USW00053152_full_2025-12-09.csv") |> 
  clean_names()

daily_rain_2026_01 <- read_csv(file = "data/NOAA_weather_station/NOAA_daily_summaries_2026-01-26.csv") |>  
  clean_names() 

# combine data frames and add new date columns
daily_rain <- bind_rows(daily_rain_2025_11_30, daily_rain_2026_01) |> 
  # create additional date columns
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         # create column for "day of water year"
         dowy = dowy(date),
         dolwy = dolwy(date)) |>  
  # create column for water year (standard definition)
  mutate(wy = case_when(
    month >= 10 ~ year + 1,
    .default = year
  ),
  # column for local water year
  lwy = case_when(
    month >= 9 ~ year + 1,
    .default = year
  ) ) |>  
  filter(!(year == 2025 & month == 12))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~----

# 2. cumulative rainfall plots ----

# create data frames with cumulative annual rainfall by (local water) year

##  all available years from weather station (pre-dating NCOS) ----
ytd_all_time <- daily_rain |> 
  group_by(lwy, .drop = FALSE) |> 
  mutate(cumulative_rain = cumsum(prcp)) |>  
  ungroup() %>%
  # create column with "year" label that will be plotted on last day of wy, including the total for the year
  mutate(year_label = case_when(
    month == 8 & day == 31 ~ paste0(year, " (", round(cumulative_rain, 1), ")")
    )) |>  
  # filter out 2008 since that was a partial year, missed rainy season
  filter(wy > 2008)

## all available years after 2018 ----
ytd <- ytd_all_time |> 
  filter(wy >= 2018)

## Monitoring report subsets ----

# filter to years 1-8 (wy 2018-2025) for year 8 monitoring report
ytd_y8_report <- ytd |>  
  # filter to 2025 water year as final year
  filter(lwy < 2026)

# create subset for year 9
ytd_y9_report <- ytd |> 
  # filter to 2026 water year as final year
  filter(lwy < 2027)

# create data frame of dates for first day in each month to use as tick marks & labels. 
# Note that 2016 is a leap year.
tickmarks <- date(
  c(
    "2016-09-01",
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
    "2016-08-01"
  )
)

# extract day of year (doy) using yday and also extract month for tick mark labels
dolwy <- tibble(mon = month(tickmarks, label = T),
              dolwy = dolwy(as.Date(tickmarks)))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~----

## Create figs ----


## 3. Cumulative annual rainfall ----

### All years - cumulative curves ----
cumulative_curves_all <-  ggplot(data = ytd_all_time,
                                 aes(
                                   x = dolwy,
                                   y = cumulative_rain,
                                   group = wy,
                                   color = wy
                                 )) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = dolwy$dolwy,
                     labels = dolwy$mon,
                     limits = c(0, 400)) +
  scale_y_continuous(breaks = breaks_width(5) , expand = c(0,1)) +
  #  geom_text(aes(x = JD + 5, y = cum_rain, label = year_label), hjust = 0) +
  geom_text_repel(aes(x = dolwy + 4, y = cumulative_rain, label = year_label), direction = c("y"), hjust = 0, size = 5) +
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

# scale_y_continuous(limits = c(0,300))
cumulative_curves_all

ggplotly(cumulative_curves_all)

# save as pdf
ggsave(cumulative_curves_all, 
       filename = paste0("figures/rainfall/cumulative_annual_rainfall_all_yrs_",
                        format(Sys.time(), "%Y-%m-%d"),
                        ".pdf"),
       
       
       width = 6.5,
       height = 5, 
       units = "in")


### Year 8 report: 2018-2025 cumulative curves----
cumulative_curves_y8 <-   ggplot(data = ytd_y8_report,
                                 aes(
                                   x = dolwy,
                                   y = cumulative_rain,
                                   group = wy,
                                   color = wy
                                 )) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = dolwy$dolwy,
                     labels = dolwy$mon,
                     limits = c(0, 430)) +
  scale_y_continuous(breaks = breaks_width(5), expand = c(0,1)) +
  #  geom_text(aes(x = JD + 5, y = cum_rain, label = year_label), hjust = 0) +
  geom_text_repel(aes(x = dolwy + 4, y = cumulative_rain, label = year_label), direction = c("y"), hjust = 0, size = 5) +
  #scale_colour_viridis_c() +
  #scale_color_manual(values = cal_palette("kelp1")) +
  #scale_x_date(date_labels = "%b", date_breaks = "months") +
  labs(
    # title = "Cumulative annual rainfall (2018-2025 water years)",
    #caption = "Coal Oil Point Reserve \n Lat: 34.41386, Lon: -119.8802",
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


cumulative_curves_y8
# ggplotly(cumulative_curves)

ggsave(cumulative_curves_y8, file = paste0("figures/rainfall/year_8_report/rainfall_curves_y8_", 
                                           format(Sys.time(), "%Y-%m-%d"),
                                           ".png"),
       width = 180,
       height = 160,
       units = "mm",
       bg = "white")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~----

# 4. annual bar graph ----


# all available full years
annual_rainfall_all <- daily_rain %>% 
  group_by(lwy) %>% 
  summarize(total_rainfall = sum(prcp),
            ndays = n()) %>%   
  # filter years
  filter(lwy >2008) %>% 
  filter(lwy <2027)

# all years relevant to NCOS
annual_rainfall_NCOS <- daily_rain %>% 
  group_by(lwy) %>% 
  summarize(total_rainfall = round(sum(prcp),1),
            ndays = n()) %>%   
  # filter years
  filter(lwy >2017 & lwy <2027) 

annual_rainfall_NCOS_y1_8 <- annual_rainfall_NCOS |> 
  filter(lwy <2026)

annual_rainfall_NCOS_y1_9 <- annual_rainfall_NCOS |> 
  filter(lwy <2027)


# create bar plots

# all available full local water years
# don't label tops of bars (too crowded)
fig_annual_rain_all <- ggplot(data = annual_rainfall_all, aes(x = lwy, y = total_rainfall)) +
  geom_col(fill = "darkblue") +
  #geom_line() +
  scale_y_continuous(expand = c(0,NA), breaks = breaks_width(5)) +
  scale_x_continuous(breaks = breaks_width(1), expand = c(0,0)) +
  theme_cowplot() +
  ylab("Total rainfall (in)") +
  xlab("Water year")

fig_annual_rain_all


## y8 annual rainfall at NCOS ----
fig_annual_rain_y8 <- ggplot(data = annual_rainfall_NCOS_y1_8, aes(x = lwy, y = total_rainfall)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = total_rainfall), vjust = -0.5, size = 4) +
  # geom_line() +
  scale_y_continuous(expand = c(0,NA), limits = c(0,31), breaks = breaks_width(5)) +
  scale_x_continuous(breaks = breaks_width(1), expand = c(0,0)) +
  theme_cowplot() +
  ylab("Total rainfall (in)") +
  xlab("Water year")

fig_annual_rain_y8

# save to file
ggsave(plot = fig_annual_rain_y8, 
       filename = paste0("figures/rainfall/year_8_report/total_annual_rainfall_y1_y8_",
                         format(Sys.time(), "%Y-%m-%d"),
                         ".png"),
       width = 6,
       height = 4.5,
       units = "in",
       bg = "white")

## y9 annual rainfall at NCOS ----
fig_annual_rain_y9 <- ggplot(data = annual_rainfall_NCOS_y1_9, aes(x = lwy, y = total_rainfall)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = total_rainfall), vjust = -0.5, size = 4) +
  # geom_line() +
  scale_y_continuous(expand = c(0,NA), limits = c(0,31), breaks = breaks_width(5)) +
  scale_x_continuous(breaks = breaks_width(1), expand = c(0,0)) +
  theme_cowplot() +
  ylab("Total rainfall (in)") +
  xlab("Water year")

fig_annual_rain_y9



# save to file
ggsave(plot = fig_annual_rain_y9, 
        filename = paste0("figures/rainfall/year_9_report/total_annual_rainfall_y1_y9_",
                          format(Sys.time(), "%Y-%m-%d"),
                          ".png"),
        width = 180,
        height = 140,
        units = "mm",
        bg = "white")



# histogram (distribution) of annual rainfall
fig_annual_rain_hist <- ggplot(data = annual_rainfall_all, aes(x = total_rainfall)) +
  geom_histogram(binwidth = 1) +
  xlab("Total rainfall (in)") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = breaks_width(1)) +
  theme_cowplot() +
  labs(title = "Histogram of annual rainfall (2009-2026 water years)")


fig_annual_rain_hist


mean(annual_rainfall_all$total_rainfall)

median(annual_rainfall_all$total_rainfall)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~----

# 5. monthly bar graphs ----
monthly_rainfall <- daily_rain |> 
  group_by(lwy, year, month) |> 
  summarize(monthly_total = sum(prcp)) |> 
  ungroup() |> 
  mutate(time_id = make_date(year, as.numeric(month), 1) + 14) # Create a date for time series plotting

# filter to 2018-2025 water years
monthly_rainfall_y8 <- monthly_rainfall |> 
  filter(lwy >2017 & lwy < 2026) |> 
  # filter to date range of mouth state data
  filter(time_id >= as.Date("2019-01-15"))

# date range for figure:
# 2019-01-07 to 2026-10-04 to 2025-08-31


fig_monthly_rain_y8 <- ggplot(data = monthly_rainfall_y8, aes(x = time_id, y = monthly_total)) +
  geom_col(fill = "darkblue") +
  scale_x_date(date_breaks = "1 year",                  # Set tick intervals
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), breaks = breaks_width(2)) +
  theme_cowplot() +
  xlab("Date") +
  ylab("Monthly rainfall (in)")

fig_monthly_rain_y8  

# use this object to combine with mouth state timeline
