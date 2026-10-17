# =============================================================================
# Name:           2025_wy_tidal_period.R
# Description:    plots wse and tide predictions during the period in Feb 2025 when the slough had breached

# Author(s):      Francis Joyce

# Inputs:         
# Outputs:        
# 
# Notes:          
#                 
# =============================================================================

# use objects from 02_combine_levelogger_data.R

# also uses objects from 2025_wy_tides.R
source("code/tides/2025_wy_tides.R")

# filter levelogger data to relevant date range
tidal_2025_wy_wse <- wse_2025_combined %>% 
  filter(date >= as.Date("2025-01-31")) %>% 
  filter(date <= as.Date("2025-03-22"))


# and also uses objects from 2025_wy_precip.R
source("code/met_station/2025_wy_precip.R")

# filter rainfall to relevant range of dates
tidal_2025_wy_rain <- water_year_2025 %>% 
  filter(date >= as.Date("2025-01-31.5")) %>% 
  filter(date <= as.Date("2025-03-22")) |> 
  mutate(datetime = as_datetime(date))

# plot water levels with tides ----  
tidal_2025_wy_fig <- ggplot(data = tidal_2025_wy_wse, aes(x = datetime, y = wse)) +
  # add line for tidal prediction
  geom_line(data = tides_2025, aes(x = datetime, y = pred), color = "darkgray") +
  ylab("Water surface elevation (ft)") +
  xlab("Date") +
  labs(color = "Levelogger Station") +
  scale_x_datetime(
    #date_breaks = "10 days",
                   date_labels = "%b %d",
                   expand = c(0,0)
  ) +
  # add lines for levelogger data
  geom_line(aes(color = station)) +
  theme(legend.position = "top")
 

tidal_2025_wy_fig  

ggplotly(tidal_2025_wy_fig)

# note- there is more of a lag (offset) 
# between the tidal predictions and Pier water elevations than I would have expected
# maybe the times are offset by 1 hr bc of PDT vs. PST tz? Something to check

start_tides <- as_datetime("2025-01-31 00:09:08")
end_tides <- as_datetime("2025-03-23 06:45:49")

# plot rainfall for same date range ----
tidal_2025_wy_rain_fig <- ggplot(data = tidal_2025_wy_rain, aes(x = date, y = prcp)) +
  geom_col(fill = "darkblue") +
  xlab("Date") +
  ylab("Rainfall (in)") +
  scale_x_datetime(
    #date_breaks = "10 days",
                   date_labels = "%b %d",
                   limits = c(start_tides, end_tides),
                   expand = c(0,0)
  ) 
                                  
tidal_2025_wy_rain_fig

# assemble water levels, tides, rainfall ----
tidal_2025_fig <- plot_grid(
  tidal_2025_wy_fig + theme_bw(),
  tidal_2025_wy_rain_fig + theme_bw(),
  nrow = 2,
  align = "v"
)

tidal_2025_fig

# write multi-panel figure to file
ggsave(filename = paste0("figures/water_levels/y8/2025_wy_tidal_period_",
                        format(Sys.time(), "%Y-%m-%d"),
                        ".png"),
       bg = "white",
       plot = tidal_2025_fig,
       width = 140,
       height = 90,
       units = "mm")




