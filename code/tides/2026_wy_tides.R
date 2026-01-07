# =============================================================================
# Name:           2026_wy_tides.R
# Description:    preps & plots NOAA tide predictions, to use with levelogger viz

# Author(s):      Francis Joyce

# Inputs:         csv in data/NOAA_tide_predictions      
# Outputs:        plots of tidal predictions      
# 
# Notes:         This script is for use with a specific date range (2026 water year) 
#                 
# =============================================================================

#read in tide data

#note that there are 13 lines of metadata at the top of the txt file
#Uses MLLW datum (mean lower low water), not NAVD88! 

tides_2025_11 <- read_delim(file = "data/NOAA_tide_predictions/NOAA_tide_preds_20251110_20251201.txt",
           skip = 14,
           delim = "\t",
           col_names = c("date", "day", "time", "pred")) %>% 
  select(date:pred) %>% 
  mutate(datetime = as.POSIXct(date) + time)


#quick plot

tide_plot_2025_11 <- ggplot(data = tides_2025_11, aes(x = datetime, y = pred)) +
  geom_line()

tide_plot_2025_11



