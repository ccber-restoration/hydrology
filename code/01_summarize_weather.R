library(tidyverse)
library(janitor)
library(cowplot)

daily_summaries <- read_csv(file = "data/NOAA_daily_summaries_USW00053152.csv") %>% 
  clean_names()

water_year_2025 <- daily_summaries %>% 
  filter(date > "2024-09-30" & date < "2025-10-01") %>% 
  mutate(cumulative_prcp = cumsum(prcp))

#why only 364 days??

#how many days with more than 0.1 inches of precip

rainy_days <- water_year_2025 %>% 
  filter(prcp >= 0.1)

#16 days with more than 0.1 inches of rain

very_wet_days <- water_year_2025 %>% 
  filter(prcp >= 1)

#3 days with more than 1 inch of rain

#single rainiest day was January 26 2025 with 2.47 inches

temp_fig <- ggplot(data = water_year_2025, aes(x = date, y = tmin)) +
  geom_line()

temp_fig  

precip_fig <- ggplot(data = water_year_2025, aes(x = date, y = prcp)) +
  geom_col(color = "darkblue") +
  ylab("Daily rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 months", date_labels = "%b", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_cowplot()
  
precip_fig

cumulative_precip_fig <- ggplot(data = water_year_2025, aes(x = date, y = cumulative_prcp)) +
  geom_line(color = "darkblue") +
  ylab("Cumulative rainfall (in)") +
  xlab("Date") +
  scale_x_date(breaks = "1 month", date_labels = "%b %Y") 
  #theme_cowplot()


cumulative_precip_fig
