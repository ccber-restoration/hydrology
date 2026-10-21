
# define breaks for start and end dates ----
start_date <- as.Date("2024-09-01")
end_date   <- as.Date("2025-09-01")

# Create custom breaks every 2 weeks (or your preferred interval)
my_breaks <- seq(from = start_date, to = end_date, by = "3 months")


# uses inputs from other scripts

date_scale <-   scale_x_date(
  breaks = my_breaks,
  limits = c(start_date, end_date + 15),
  date_labels = "%b-%Y",
  expand = c(0, 0)
)

# use pier levelogger data, which also has temp and conductivity

pier_20240319_20251113

pier_y8_daily <- pier_20240319_20251113 |> 
  filter(date >= as.Date("2024-09-01")) |> 
  filter(date <= as.Date("2025-08-31")) |> 
  group_by(date) |> 
  summarize(temp_daily_C = mean(temperature),
            cond_daily = mean(conductivity)
            ) |> 
  ungroup()

# temperature figure
fig_pier_temp_y8 <- ggplot(data = pier_y8_daily, aes(x = date, y = temp_daily_C)) +
  geom_line() +
  date_scale +
  theme_bw() +
  labs(y = "Temperature (C)", x = NULL) +
  scale_y_continuous(limits = c(0,NA))

fig_pier_temp_y8

# conductivity figure 
fig_pier_cond_y8 <- ggplot(data = pier_y8_daily, aes(x = date, y = cond_daily)) +
  geom_line() +
  date_scale +
  theme_bw() +
  labs(y = bquote("EC (" ~ mu * "S/cm)"), x = NULL) 

fig_pier_cond_y8


# add this object from  ysi_graphing.R for precip at bottom
fig_precip_2025_simplified

# save to file ----
pier_wq_y8 <- (fig_pier_temp_y8 / fig_pier_cond_y8 / fig_precip_2025_simplified) + 
  plot_layout(guides = "collect",
              axes = "collect",
              heights = c(1, 1, 0.5)) 

pier_wq_y8

## save to file ----
ggsave(pier_wq_y8, file = "figures/water_quality/year_8/pier_water_quality_y8.png",
       bg = "white",
       width = 6,
       height = 6)

