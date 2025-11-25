# 

library(tidyverse)
library(janitor)

ysi_parent <- read_csv("data/YSI/NCOS_YSI_Water_Quality_Monitoring_2025-11-18/NCOS_YSI_Water_Quality_Monitoring_0.csv") %>% 
  clean_names()

ysi_data <- read_csv("data/YSI/NCOS_YSI_Water_Quality_Monitoring_2025-11-18/YSI_Data_Begin_1.csv") %>% 
  clean_names()

#check depth codes

hist(ysi_data$depth_code)

unique(ysi_data$depth_code)


#join the survey metadata (parent) with the data themselves
ysi_joined <- ysi_data %>% 
  left_join(ysi_parent, by = join_by(parent_global_id == global_id)) %>% 
  #format date/time correctly
  mutate(date_time = mdy_hms(monitoring_date)) %>%
  mutate(monitoring_date = date(date_time)) %>% 
  mutate(monitoring_time = (time_of_wq_measurement)) %>% 
  mutate(year = year(date_time)) %>% 
  mutate(wy_25 = case_when(
    monitoring_date >= "2024-10-01" & monitoring_date < "2025-10-01" ~ 2025
  )) %>% 
  filter(wy_25 == 2025)

temp_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = temperature_c, color = site_name)) +
  geom_line()

temp_fig

temp_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = temperature_c, color = site_name)) +
  geom_line() +
  facet_wrap(vars(depth_code))

temp_fig

cond_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = conductivity_u_s_cm, color = site_name)) +
  geom_line() +
  facet_wrap(vars(depth_code))

cond_fig  


salinity_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = salinity_ppt, color = site_name)) +
  geom_line() +
  facet_wrap(vars(depth_code))

salinity_fig


do_sat_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = do_percent_sat, color = site_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(depth_code), nrow = 5)

do_sat_fig

