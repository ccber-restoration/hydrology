# Hydrology and water quality

The purpose of this repository is to work with local hydrological data 

## Weather data

-  Precipitation (and temperature and soil moisture) are available for the NOAA Weather station at Coal Oil Point Reserve (station ID 53152): https://www.ncei.noaa.gov/access/crn/sensors.htm?stationId=1529#precip


- Tried using the 'noaa' R package to query NOAA weather data from RStudio, but ran into issues.
- Instead downloaded data ("daily summaries" data set) from https://www.ncdc.noaa.gov/cdo-web/search (station ID 53152)

## Water level monitoring

-  Water stage is monitored every 15 minutes by Sonlinst LT leveloggers deployed at the following locations (primary locations for monitoring slough in **bold**):
    -  **Devereux Slough Pier**
    -  **East Arm Trail Bridge**
    -  **Phelps Creek - Marymount Bridge**
    -  **Venoco Bridge - north side (YSI EXO1 sonde)**
    -  West Arm - Devereux Creek
    -  Whittier Storm drain
    -  Whittier Pond
 
-  Recorded water levels are compensated using barometric pressure recorded at the ROOST, and then converted to water surface elevation based on the logger elevations.

## Tidal data

- Use NOAA tidal predictions (for station 9411340 Santa Barbara, CA) to compare against slough water levels for periods when the slough breached and was under tidal influence: https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9411340
- **Important**: when downloading and using tidal predictions, make sure to use the appropriate datum. The default is mean lower low water (MLLW), but for comparisons to levelogger water surface elevation, need to select NAVD.

## Vernal Pool hydrology
- Water level in vernal pools is manually monitored.

## Water quality 
- From annual report "we collect dissolved oxygen, conductivity/salinity, and temperature data at three locations on a weekly basis (figures 33-35) using a portable YSI Pro2030 at the three bridges that cross the upper slough: the Marsh trail bridges over the Phelps Creek outlet and “Dilling’s Link” across the east channel, and the Venoco access road bridge"
- YSI data are collected in a Survey123 Form ("NCOS YSI Water Quality Monitoring")

  












