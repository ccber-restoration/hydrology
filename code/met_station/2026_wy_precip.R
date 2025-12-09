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

#test day of water year (dowy) function
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

#create data frame for 2026 water year
#(first 67 days of 2026 water year)
water_year_2026 <- daily_summaries %>% 
  filter(date > "2025-09-30" & date < "2026-10-01") %>% 
  mutate(cumulative_prcp = cumsum(prcp))

#check for missing dates 
start_date <- as.Date("2025-10-01")
end_date <- as.Date("2026-09-30")
all_expected_dates <- seq(from = start_date, to = end_date, by = "day")

missing_date <- setdiff(all_expected_dates, water_year_2026$date)

as.Date(missing_date)

#how many days with more than 0.1 inch of precip
rainy_days <- water_year_2026 %>% 
  filter(prcp >= 0.1)

#[8] days with more than 0.1 inch of rain

very_wet_days <- water_year_2026 %>% 
  filter(prcp >= 1)

#[3] days with more than 1 inch of rain

#single rainiest day was [2025-11-14] with [2.57] inches of rain

#total precip (inches)
sum(water_year_2026$prcp)

#total rainfall for water year = [8.27] inches

precip_fig <- ggplot(data = water_year_2026, aes(x = date, y = prcp)) +
  geom_col(color = "darkblue") +
  ylab("Daily rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 months", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_cowplot()

precip_fig
