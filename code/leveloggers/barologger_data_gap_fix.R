library(dplyr)
library(janitor)
library(tidyverse)
library(lubridate)
library(hms)
library(measurements)

# 1. Santa Barbara Airport pressure data ----

## Read in data from Santa Barbara Airport weather station. We are missing
## barologger data from 5/10 through 5/20 and from 8/29 through the rest of the WY.
## source: https://mesonet.agron.iastate.edu/request/download.phtml?network=CA_ASOS
sba<-read_csv("data/NOAA_weather_station/SantaBarbaraAirport_altimeter_05.10.2025_11.26.2025.csv")
sba$valid<-mdy_hm(sba$valid)
sba <- sba %>%
  mutate(
    Date = date(valid),
    Time = as_hms(valid)
  )
sba

## We only care about the 15 and 10 minute interval data. Add a column called 
## interval and omit NA values.
sba<-sba %>%
  mutate(interval = case_when(
    grepl(pattern = "00:00", x = Time) ~ "00",
    grepl(pattern = "10:00", x = Time) ~ "10",
    grepl(pattern = "15:00", x = Time) ~ "15",
    grepl(pattern = "20:00", x = Time) ~ "20",
    grepl(pattern = "30:00", x = Time) ~ "30",
    grepl(pattern = "40:00", x = Time) ~ "40",
    grepl(pattern = "45:00", x = Time) ~ "45",
    grepl(pattern = "50:00", x = Time) ~ "50",
  ))

sba <- sba %>%
  filter(!is.na(interval))

## Altimeter is measured in inHg and is not equal to barometric pressure. We
## need to calculate baropressure using the elevation of the station (3 m
## according to the station metadata) and convert to kPa. Multiply altimeter by
## a constant equal to ((288 - 0.0065 x h)/288)^2 where h is the elevation in m.

sba$baropressure_inHg<-sba$altimeter*0.9996441697
sba

## Since the station is at sea level, the difference is negligible.
## Convert from inches of mercury (inHg) to kilopascals (kPa).
sba$baropressure_kPa<-sba$baropressure_inHg / 10 * 33.8639
sba

## Levellogger manual provides conversion factor for kPa to water column equivalent
## feet and meters.
sba$equivalent_ft<-sba$baropressure_kPa*0.334553
sba$equivalent_m<-sba$baropressure_kPa*0.101972
sba

write.csv(sba,"data/NOAA_weather_station/SBA_baropressure_05.10.2025_11.26.2025.csv")

# 2. Manual Barometric Compensation ----

## Conduct manual compensation for data gaps. Step 1: Calculate elevation
## difference between elevation of levelogger and elevation of weather station.
## According to Solinst guide: (elevation of levelogger - elevation of weather
## station) divided by 826. This is because as elevation increases, barometric 
## pressure decreases at a rate of approximately 1.21/1000 ft or meters.

logger_elev<-read_csv("data/leveloggers/logger_elevations_2025wy.csv")
logger_elev$elev_diff_ft <- (logger_elev$elevation_ft-9.84252)/826
logger_elev

# 3. Venoco Bridge -----

## Read in Venoco Bridge data from 8/29-10/22. Level is in meters.
venoco<-read_csv("data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Uncompensated.csv", skip = 11)
venoco<-venoco %>% 
clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) 

## merge with Santa Barbara Airport baropressure df
venoco_comp<-left_join(venoco,sba,by=join_by(datetime == valid))

## subtract elevation difference coefficient (see logger_elev) and water column equivalent
venoco_comp$comp_level_ft<-venoco_comp$level_ft-0.0084800484-venoco_comp$equivalent_ft
venoco_comp<-venoco_comp %>% 
  select(datetime,comp_level_ft,temperature)
  
write.csv(venoco_comp,"data/leveloggers/Venoco_Bridge/Venoco_08.29.25_10.22.25_Compensated.csv")

## Read in uncompensated Venoco Bridge data from 5/10-5/20. Level is in meters.
venoco_may<-read_csv("data/leveloggers/Venoco_Bridge/Venoco_11.13.24_8.29.25_Uncompensated.csv", skip = 11)
venoco_may<-venoco_may %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time)

## Filter for missing data between 5/10/25 at 3:45 am and 5/20/25 at 12:45 pm.
venoco_may$datetime<-floor_date(venoco_may$datetime,unit="minute")
venoco_may<-venoco_may%>% 
  filter(datetime > ymd_hms("2025-05-10 03:45:00") & datetime < ymd_hms("2025-05-20 12:45:00"))

## merge with Santa Barbara Airport baropressure df
vnc_may_comp<-left_join(venoco_may,sba,by=join_by(datetime == valid))

## subtract elevation difference coefficient (see logger_elev) and water column equivalent
vnc_may_comp$comp_level_ft<-vnc_may_comp$level_ft-0.0084800484-vnc_may_comp$equivalent_ft
vnc_may_comp<-vnc_may_comp %>% 
  select(datetime,comp_level_ft,temperature)

write.csv(vnc_may_comp,"data/leveloggers/Venoco_Bridge/Venoco_05.10.25_05.20.25_Compensated.csv")

# 4. Phelps Creek ----

## Read in Phelps data from 8/29-11/13. Level is in meters.
phlp<-read_csv("data/leveloggers/Phelps_Creek_Marymount_Bridge/Phelps_08.29.25_11.13.25_Uncompensated.csv", skip = 11)
phlp<-phlp %>% 
  clean_names() %>% 
  mutate(
    #parse date from character to date format
    date = mdy(date),
    level_ft = conv_unit(level, "m", "ft"),
    #create datetime variable, first converting date to POSIXct
    datetime = as.POSIXct(date) + time) 

## continue from here, same pattern as above
