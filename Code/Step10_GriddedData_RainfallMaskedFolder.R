
cat("\014") 
rm(list=ls())
library(dataRetrieval)
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(base64enc)
library(lubridate)  
library(patchwork)
library(ggplot2)
library(grid)
library(stringr)
library(wql)
library(ggplot2)
library(grid)
library(lubridate)
library(measurements)
library(dplyr)
library(tidyverse)
library(openair)
library(heatwaveR)
library(data.table)
library(ggplot2)
library(grid)
library(lubridate)
library(measurements)
library(dplyr)
library(tidyverse)
library(openair)
library(heatwaveR)
library(data.table)
require(methods)
library(investr)
require(gridExtra)
library(Metrics)
library(scales)
library(purrr)
library(dplyr)
library(lubridate)
library(zoo) 
library(writexl)
library(readxl)
library(terra)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"

#these are the annual rainfall…. the names of the raster files 1= 1904, 2=1905, ….119=2022
desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)

file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"

tiff_folder_path <- file.path(file_Path_Variable_I, "Prism/masked_monthly_to_annual")

tiff_files <- list.files(tiff_folder_path, pattern = "\\.tif$", full.names = TRUE)

rainfall_tibble <- tibble(
  file_name = character(),
  rainfall_data = list()
)

for (tiff_file in tiff_files) {
  rainfall_raster <- rast(tiff_file)
  
  rainfall_df <- as.data.frame(rainfall_raster, xy = TRUE)
  
  rainfall_tibble <- rainfall_tibble %>%
    add_row(
      file_name = basename(tiff_file),  
      rainfall_data = list(rainfall_df)  
    )
}


rainfall_tibble <- rainfall_tibble %>%
  mutate(Year = as.numeric(gsub("\\D", "", file_name)))

rainfall_tibble <- rainfall_tibble %>%
  arrange(Year)


rainfall_tibble <- rainfall_tibble %>%
  mutate(Year_N = 1903 + Year) 


# 
# for (i in seq_along(rainfall_tibble$Year)) {
#   rainfall_tibble$Year[i] <- 1903 + rainfall_tibble$Year[i]
# }

rainfall_tibble_1904<- rainfall_tibble %>%  filter(Year_N=='1904')
rainfall_raster <- rast(rainfall_tibble_1904$rainfall_data[[1]])
crs(rainfall_raster) <- "EPSG:4269"
combined_shp_leaflet <- st_transform(combined_shp_leaflet, crs(rainfall_raster))
rainfall_clipped_raster <- crop(rainfall_raster, combined_shp_leaflet)  
rainfall_clipped_raster <- mask(rainfall_clipped_raster, vect(combined_shp_leaflet))  
rainfall_clipped_df <- as.data.frame(rainfall_clipped_raster, xy = TRUE)


rainfall_clipped_map_1904 <- ggplot() +
  geom_raster(data = rainfall_clipped_df, aes(x = x, y = y, fill = total_annual_rainfall_1)) +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "black", size = 1) +
  labs(title = "Clipped Mean Annual Rainfall 1904") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()  
  )

print(rainfall_clipped_map_1904)
