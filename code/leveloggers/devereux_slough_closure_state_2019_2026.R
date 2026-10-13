library(tidyverse)
library(readxl)
library(cowplot)

# define color palette
pal_mouth_closure <- c(closed = "#6d6b35", open = "#285c77")


# read in data
key_dates <- read_xlsx("data/leveloggers/Devereux_Slough_mouth_state.xlsx") |> 
  filter(date > as.Date("2019-01-06"))

# create full sequence of dates within date range
date_seq <- seq(from = as.Date("2019-01-07"), to = as.Date("2026-10-04"), by = "day") |> 
  enframe(name = NULL) |> 
  rename(date = value)

# join key dates into full date range, fill blank values, also create year column
dates_w_state <- date_seq |> 
  left_join(key_dates) |> 
  # fill blank values with preceding value
  fill(mouth_status, .direction = "down") |> 
  mutate(year = year(date))


# visualize

fig_closure <- ggplot(data = dates_w_state, aes(x = date, y = 10, color = mouth_status, fill = mouth_status)) +
  geom_col() +
  scale_x_date(
    date_breaks = "1 year",        # Major ticks and labels every year
    date_labels = "%Y",            # Format labels as 4-digit year
    date_minor_breaks = "1 month"  # Minor ticks every month
  ) +
  scale_fill_manual(values = pal_mouth_closure) +
  scale_color_manual(values = pal_mouth_closure) +
  theme_cowplot() +
  theme(
    axis.line.y = element_blank(), # This removes the y-axis line
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "Devereux Slough Mouth State ") +
  labs(fill = "Mouth state",
       color = "Mouth state")

fig_closure

ggsave(paste0("figures/mouth/dev_slough_mouth_closure_timeline_2019_2026_",
              format(Sys.time(), "%Y-%m-%d"),
              ".png"),
       fig_closure,
       width = 6.5,
       height = 2.5)

# create version only through 2025, for Year 8 report ----

date_w_state_2025 <- dates_w_state |> 
  filter(date < as.Date("2025-09-01"))

fig_closure_2025 <-  ggplot(data = date_w_state_2025, aes(x = date, y = 10, color = mouth_status, fill = mouth_status)) +
  geom_col() +
  scale_x_date(
    date_breaks = "1 year",        # Major ticks and labels every year
    date_labels = "%Y",            # Format labels as 4-digit year
    date_minor_breaks = "1 month"  # Minor ticks every month
  ) +
  scale_fill_manual(values = pal_mouth_closure) +
  scale_color_manual(values = pal_mouth_closure) +
  scale_y_continuous(expand = c(0,0)) +
  theme_cowplot() +
  theme(
    axis.line.y = element_blank(), # This removes the y-axis line
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(title = "Devereux Slough Mouth State ") +
  xlab("Date") +
  labs(fill = "Mouth state",
       color = "Mouth state")

fig_closure_2025

# combine with monthly rainfall data

# need to run the 01_summarize_weather.R script in order to generate the fig_monthly_rain_y8 ggplot object

fig_mouth_and_rain_y8 <- plot_grid(fig_closure_2025 +  theme(legend.position = "top"),
                                   fig_monthly_rain_y8,
                                   nrow = 2,
                                   align = "v",
                                   rel_heights = c(1, 1.5))

fig_mouth_and_rain_y8

ggsave(paste0("figures/mouth/year8/dev_slough_mouth_closure_timeline_2019_2025_",
              format(Sys.time(), "%Y-%m-%d"),
              ".png"),
       fig_mouth_and_rain_y8,
       bg = "white",
       width = 7,
       height = 5)


# try single panel, with mouth state as background color

fig_mouth_closure_comb <-  ggplot(data = monthly_rainfall_y8, aes(x = time_id, y = monthly_total)) +
  geom_col(data = date_w_state_2025, aes(x = date, y = 12, fill = mouth_status)) +
  geom_col(fill = "darkblue", color = "darkblue") +
  scale_fill_manual(values = pal_mouth_closure) +
  scale_x_date(date_breaks = "1 year",                  # Set tick intervals
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), breaks = breaks_width(2)) +
  theme_cowplot() +
  xlab("Date") +
  ylab("Monthly rainfall (in)") +
  labs(fill = "Mouth state")
  
fig_mouth_closure_comb

ggsave(paste0("figures/mouth/year8/dev_slough_mouth_closure_timeline_2019_2025_mon_rain_",
              format(Sys.time(), "%Y-%m-%d"),
              ".pdf"),
       fig_mouth_closure_comb,
       bg = "white",
       width = 7,
       height = 5)

