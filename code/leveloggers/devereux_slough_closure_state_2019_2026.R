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

ggsave("figures/dev_slough_mouth_closure_timeline_2019_2026.png",
       fig_closure,
       width = 9,
       height = 4)