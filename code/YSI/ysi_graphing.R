# 

library(tidyverse)
library(janitor)
library(patchwork)
library(plotly)

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
    monitoring_date >= "2024-09-01" & monitoring_date < "2025-09-01" ~ 2025
  )) %>% 
  filter(wy_25 == 2025) |> 
  # rename site codes nicely
  mutate(site_name = case_when(
    site_name == "east_channel" ~ "East Channel",
    site_name == "phelps_bridge" ~ "Phelps Bridge",
    site_name == "venoco_bridge" ~ "Venoco Bridge"
  )) |> 
  # put sites in sensible order (downstream to upstream)
  mutate(site_name = fct_relevel(site_name, "Venoco Bridge", "East Channel", "Phelps Bridge"))

# define breaks for start and end dates ----
start_date <- as.Date("2024-09-01")
end_date   <- as.Date("2025-09-01")

# Create custom breaks every 2 weeks (or your preferred interval)
my_breaks <- seq(from = start_date, to = end_date, by = "3 months")


# Average across depths ----

# average across depths
ysi_averaged <- ysi_joined |> 
  group_by(monitoring_date, site_name, flow) |> 
  summarize(time = mean(time_of_wq_measurement),
            wse_ft = max(water_surface_elevation, na.rm = TRUE),
            do_mg_l_mean = mean(do_mg_l, na.rm = TRUE),
            do_percent_sat_mean = mean(do_percent_sat, na.rm = TRUE),
            conductivity_u_s_cm_mean = mean(conductivity_u_s_cm, na.rm = TRUE),
            salinity_ppt_mean = mean(salinity_ppt, na.rm = TRUE),
            temperature_c_mean = mean(temperature_c, na.rm = TRUE),
            n_measurements = n()
  ) |> 
  # correct issue where zeroes were mistakenly entered in the case of no measurement
  mutate(conductivity_u_s_cm_mean = case_when(
    conductivity_u_s_cm_mean == 0 & monitoring_date >= as.Date("2025-08-01") ~ NA,
    .default = conductivity_u_s_cm_mean),
    salinity_ppt_mean = case_when(
      salinity_ppt_mean == 0 & monitoring_date >= as.Date("2025-07-25") ~ NA,
      .default = salinity_ppt_mean),
    do_mg_l_mean = case_when(
      do_mg_l_mean == 0 & monitoring_date >= as.Date("2025-07-25") ~ NA,
      .default = do_mg_l_mean
    )
  )


# Use facet_wrap as a quick preview ----

# pivot long, with a column identifying the response variable
ysi_averaged_long <- ysi_averaged |> 
  pivot_longer(
    cols = do_mg_l_mean:temperature_c_mean, 
    names_to = "response_var", 
    values_to = "response_value"
  )

# faceted figure
fig_water_quality_faceted <- ggplot(data = ysi_averaged_long, aes(x = monitoring_date, y = response_value, group = site_name, color = site_name)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  facet_wrap(~ response_var, scales = "free_y", ncol = 2)

fig_water_quality_faceted

# Patchwork approach (to have individual y-axis labels) ----

# define shared x axis (date) properties, to reduce redundancy
date_scale <-   scale_x_date(
  breaks = my_breaks,
  limits = c(start_date, end_date + 15),
  date_labels = "%b-%Y",
  expand = c(0, 0)
)


pal <- scale_color_brewer(palette = "Set2")


# Build individual plots with custom y-axis labels
p1_ec <- ggplot(ysi_averaged, aes(x = monitoring_date, y = conductivity_u_s_cm_mean, color = site_name)) +
  geom_line() +
  labs(y = bquote("EC (" ~ mu * "S/cm)"), x = NULL) + # Remove x-axis label for top plots
  theme_bw() +
  labs(color = "Site") +
  date_scale +
  pal
  
p1_ec

p2_salinity <- ggplot(ysi_averaged, aes(x = monitoring_date, y = salinity_ppt_mean, color = site_name)) +
  geom_line() +
  labs(y = "Salinity (ppt)", x = NULL) +
  theme_bw() + 
  labs(color = "Site") +
  date_scale +
  pal

p2_salinity

p3_temp <-  ggplot(ysi_averaged, aes(x = monitoring_date, y = temperature_c_mean, color = site_name)) +
  geom_line() +
  labs(y = "Temperature (°C) \n", x = NULL) +
  theme_bw() +
  labs(color = "Site") +
  date_scale +
  pal

p3_temp


p4_do_perc <-  ggplot(ysi_averaged, aes(x = monitoring_date, y = do_percent_sat_mean, color = site_name)) +
  geom_line() +
  labs(y = "Dissolved Oxygen \n(% saturation)", x = NULL) +
  labs(color = "Site") +
  theme_bw() +
  date_scale +
  pal

p4_do_perc

p5_do_mg_l <- ggplot(ysi_averaged, aes(x = monitoring_date, y = do_mg_l_mean, color = site_name)) +
  geom_line() +
  scale_x_date() +
  labs(y = "Dissolved Oxygen \n(mg/L)", x = NULL) +
  theme_bw() + 
  labs(color = "Site") +
  date_scale +
  pal

p5_do_mg_l


## simplify existing precipitation figure ----
fig_precip_2025_simplified <-  fig_precip_2025 +
  theme_bw() +
  date_scale +
  scale_y_continuous(breaks = breaks_width(0.5), limits = c(0,NA), expand = c(0,NA))

fig_precip_2025_simplified

##  Combine using patchwork into small multiples ----
# 'guides = "collect"' merges the legends into one

# decided to exclude the DO percent saturation figure
wq_y8 <- (p3_temp / p5_do_mg_l / p1_ec / p2_salinity / fig_precip_2025_simplified) + 
  plot_layout(guides = "collect",
              axes = "collect",
              heights = c(1, 1, 1, 1, 0.5)) & 
  theme(legend.position = "right")

wq_y8

## save to file ----
ggsave(wq_y8, file = "figures/water_quality/year_8/water_quality_y8.pdf",
       width = 6,
       height = 9)

# code dump ----
temp_fig <- ggplot(data = ysi_joined, aes(x = monitoring_date, y = temperature_c, color = site_name, group = depth_code)) +
  geom_line() +
  geom_point()

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

