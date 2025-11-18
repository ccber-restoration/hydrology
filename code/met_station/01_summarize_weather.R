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
daily_summaries <- read_csv(file = "data/NOAA_weather_station/NOAA_daily_summaries_USW00053152_full.csv") %>% 
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
  # create column with "year" label
  mutate(year_label = case_when(
    month == 9 & day == 30 ~ year
  )) %>% 
  filter(wy >= 2018)


#create data frame of dates for first day in each month to use as tick marks & labels. Note that 2016 is a leap year.
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
    title = "Cumulative Annual Rainfall (2018-2025 water years)",
    caption = "Coal Oil Point Reserve \n Lat: 34.41386, Lon: -119.8802",
    y = "Cumulative rainfall (in)",
    x = ""
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

ggsave(cumulative_curves, filename = "figures/cumulative_annual_rainfall.png" )
#ggplotly(cumulative_curves)

# simple annual bar graph ----

annual_rainfall <- daily_summaries %>% 
  group_by(wy) %>% 
  summarize(total_rainfall = sum(prcp),
            ndays = n()) %>%   
  #filter years
  filter(wy >2017 & wy <2026) 

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

ggsave(plot = fig_annual_rain, filename = "figures/annual_rainfall.png")

## 2025-only ----
water_year_2025 <- daily_summaries %>% 
  filter(date > "2024-09-30" & date < "2025-10-01") %>% 
  mutate(cumulative_prcp = cumsum(prcp))

#why only 364 days??
start_date <- as.Date("2024-10-01")
end_date <- as.Date("2025-09-30")
all_expected_dates <- seq(from = start_date, to = end_date, by = "day")

missing_date <- setdiff(all_expected_dates, water_year_2025$date)

as.Date(missing_date)
#2025-04-38 is missing

#how many days with more than 0.1 inch of precip

rainy_days <- water_year_2025 %>% 
  filter(prcp >= 0.1)

#16 days with more than 0.1 inch of rain

very_wet_days <- water_year_2025 %>% 
  filter(prcp >= 1)

#3 days with more than 1 inch of rain

#single rainiest day was January 26 2025 with 2.47 inches


precip_fig <- ggplot(data = water_year_2025, aes(x = date, y = prcp)) +
  geom_col(color = "darkblue") +
  ylab("Daily rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 months", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_cowplot()
  
precip_fig

ggsave(filename = "figures/2025wy_daily_precip.png", plot = precip_fig)

cumulative_precip_fig <- ggplot(data = water_year_2025, aes(x = date, y = cumulative_prcp)) +
  geom_line(color = "darkblue") +
  ylab("Cumulative rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 month", date_labels = "%b %Y") 
  #theme_cowplot()


cumulative_precip_fig
