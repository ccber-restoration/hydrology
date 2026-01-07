library(sf)
library(mapview)

gdb_path <- "data/berm_profiles/2024_Copr_geodatabase/COPR Berm.gdb"

#list the layers within the gdb
st_layers(gdb_path)

#read in specific layers

berm_points_2024 <- st_read(dsn = gdb_path, layer = "c2024berm_points")

mapview(berm_points_2024)

#this is just a square bounding box
berm_polygon_2024 <- st_read(dsn = gdb_path, layer = "bermelevation2024")

#this is just a bounding box
berm_elevation_2024 <- st_read(dsn = gdb_path, layer = "bermelevation2024_realelevation")  