##

#load package
library(noaa)

#check API code

api_token <- Sys.getenv("NOAA_API_TOKEN")

#station ID seems to be 53152 or some variation
#or 1529??

COPR_precip <- get_climate_data(noaa_token = api_token,
                           datasetid = "PRECIP_HLY", #hourly precipitation data
                           #stationid = "USW00053152",
                           startdate = "2024-10-01",
                           enddate = "2025-09-30")

#locations <- get_locationid(noaa_token = api_token, category_id = "ZIP", n_results = INF)
