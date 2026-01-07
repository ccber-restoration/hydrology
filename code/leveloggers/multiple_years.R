library(tidyverse)
library(janitor)

Venoco_18_23 <- read_csv(file = "data/leveloggers/dryad/doi_10_25349_D9RP7X__v20240417/Veneco_Bridge_PTdata_2018-2023wy.csv") %>% 
  clean_names() %>% 
  #create columns for month and year
  mutate(date = mdy(date),
         year = year(date),
         month = month(date),
         day = day(date),
         #create column for "day of water year"
         dowy = dowy(date)) %>% 
  #create column for wateryear
  mutate(wy = case_when(
    month >= 10 ~ year + 1,
    .default = year)) %>% 
  filter(wse_ft < 15 & wse_ft > 0)

Venoco_18_23_avg <- Venoco_18_23 %>% 
  group_by(date, dowy, wy) %>% 
  summarize(wse_ft_avg = mean(wse_ft)) %>% 
  #filter out anomalous values that are > 12 ft
  filter(wse_ft_avg < 15)


Venoco_18_23_fig <- ggplot(data = Venoco_18_23, aes(x = dowy, y = wse_ft, color = wy, group = wy)) +
  geom_line() +
  labs(title = "Venoco")

Venoco_18_23_fig
