library(dplyr)
library(tidyverse)
library(lubridate)
library(hms)

## Read in data from Santa Barbara Airport weather station
## source: https://mesonet.agron.iastate.edu/request/download.phtml?network=CA_ASOS
sba<-read_csv("data/NOAA_weather_station/SantaBarbaraAirport_altimeter_05.10.2025_11.26.2025.csv")
sba$valid<-mdy_hm(sba$valid)
sba <- sba %>%
  mutate(
    Date = date(valid),
    Time = as_hms(valid)
  )
sba

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

## To do: write or adapt code for manual compensation. Use Solinst software to
## compensate up to 5/9/2025. New barologger started running in East Storke
## Wetland on 11/18/2025.

write.csv(sba,"data/NOAA_weather_station/SBA_baropressure_05.10.2025_11.26.2025.csv")

