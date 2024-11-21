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

# change file_Path_Variable 

file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
precipitation_folder <- file.path(file_Path_Variable_I, "final_Stationwise_Precipitation_NOAA")
MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_step3.rds"))
MediumTerm_StreamflowData


desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
#combined_shp <- st_union(combined_shp, RiverNetwork_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)




csv_files <- list.files(precipitation_folder, pattern = "\\.csv$", full.names = TRUE)
all_dataframes <- lapply(csv_files, read.csv)
names(all_dataframes) <- basename(csv_files)
lapply(all_dataframes, head)
station_names <- gsub("\\.csv$", "", basename(csv_files))

Precipitation_Data <- tibble(
  station_name = station_names,
  Precipitation_data = lapply(all_dataframes, as_tibble)  # Nest the data frames as tibbles
)

Precipitation_Data <- Precipitation_Data %>%
  mutate(
    Lat = sapply(Precipitation_data, function(x) x$Lat[1]),
    Long = sapply(Precipitation_data, function(x) x$Long[1])
  )

stations_sf <- st_as_sf(
  Precipitation_Data,
  coords = c("Long", "Lat"),
  crs = st_crs(combined_shp_leaflet)  
)
stations_within_boundary <- stations_sf[st_intersects(stations_sf, combined_shp_leaflet, sparse = FALSE), ]



Precipitation_Data_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = Precipitation_Data, 
             aes(x = Long, y = Lat, 
                 color = 'red', 
                 size = 2),  # Map both color and size to streamflow
             alpha = 0.8) +
  # scale_color_viridis_c(option = "plasma", name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the color scale limits from 0 to 10,000
  # scale_size_continuous(range = c(2, 10), name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Precipitation Data Locations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )
ggsave(filename = (file.path(file_Path_Variable_O,"Step11_Precipitation.jpg")), 
       plot = Precipitation_Data_Map, 
       width = 8, height = 6, dpi = 300)




stations_sf <- Precipitation_Data %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(combined_shp_leaflet))

stations_within_boundary <- st_intersection(stations_sf, combined_shp_leaflet)

stations_within_boundary_tibble <- stations_within_boundary %>%
  as_tibble() 

print(stations_within_boundary_tibble)
stations_within_boundary_tibble <- stations_within_boundary_tibble %>%
  mutate(
    Long = st_coordinates(geometry)[, 1],
    Lat = st_coordinates(geometry)[, 2]
  )
Precipitation_Data_WB<- stations_within_boundary_tibble %>%  select(station_name,Precipitation_data,Long,Lat)


Precipitation_Data_WB_Map2 <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) + # Outline the shapefile
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust river network style
  geom_point(data = Precipitation_Data_WB, 
             aes(x = Long, y = Lat, color = "Precipitation Data"),  # Add to legend
             size = 4,  # Fixed size for points
             alpha = 0.8) +
  geom_point(data = MediumTerm_StreamflowData, 
             aes(x = station_lon, y = station_lat, color = "Streamflow Data"),  # Add to legend
             size = 4,  # Fixed size for points
             alpha = 0.8) +
  scale_color_manual(
    values = c("Precipitation Data" = "red", "Streamflow Data" = "black"),  # Custom colors for legend
    name = "Data Type"  # Legend title
  ) +
  labs(title = "Precipitation Data Locations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  # Adjust legend position
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )


Precipitation_Data_WB_Map3 <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = Precipitation_Data_WB, 
             aes(x = Long, y = Lat, 
                 color = 'red', 
                 size = 2),  # Map both color and size to streamflow
             alpha = 0.8) +
  # geom_point(data = MediumTerm_StreamflowData, 
  #            aes(x = station_lon, y = station_lat, 
  #                color = 'black', 
  #                size = 2),  # Map both color and size to streamflow
  #            alpha = 0.8) +
  # # scale_color_viridis_c(option = "plasma", name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the color scale limits from 0 to 10,000
  # scale_size_continuous(range = c(2, 10), name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Precipitation Data Locations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(), 
    axis.text = element_blank(),   
    axis.ticks = element_blank()  
  )

Precipitation_Data_Map_All<-Precipitation_Data_WB_Map2
ggsave(filename = (file.path(file_Path_Variable_O,"Step11_Precipitation.jpg")), 
       plot = Precipitation_Data_Map_All, 
       width = 14, height =10, dpi = 600)

