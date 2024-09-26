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


# AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_step2.rds"))
# AllYear_StreamflowData
# 
# 
# 
# MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_MediumSubset_step3.rds"))
# MediumTerm_StreamflowData
# 
# 
# LongTerm_StreamflowData <-  readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_LongSubset_step3.rds"))
# LongTerm_StreamflowData





############Figure 


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

### All average 

AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "AllYear_StreamflowData_MMD.rds") )
AllYear_StreamflowData


MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_MMD_step3.rds"))
LongTerm_StreamflowData




overall_average_streamflow_AllYear <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow_mm_per_day, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_AllYear)

overall_average_streamflow_AllYear<- overall_average_streamflow_AllYear %>% 
  left_join(AllYear_StreamflowData, by='site_no')





overall_average_streamflow_MediumTerm <- MediumTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow_mm_per_day, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_MediumTerm)

overall_average_streamflow_MediumTerm<- overall_average_streamflow_MediumTerm %>% 
  left_join(MediumTerm_StreamflowData, by='site_no')









overall_average_streamflow_LongTerm <- LongTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow_mm_per_day, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_LongTerm)

overall_average_streamflow_LongTerm<- overall_average_streamflow_LongTerm %>% 
  left_join(LongTerm_StreamflowData, by='site_no')





maxcf<- max(overall_average_streamflow_AllYear$OverallAvgStreamflow__MMD)
mincf<- min(overall_average_streamflow_AllYear$OverallAvgStreamflow__MMD)

overall_average_streamflow_AllYear_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_AllYear, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations (All) \nOverall Average Streamflow (_MMD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

print(overall_average_streamflow_AllYear_Map)

ggsave(filename = (file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_AllYear_MMD.jpg")), 
       plot = overall_average_streamflow_AllYear_Map, 
       width = 8, height = 6, dpi = 300)








overall_average_streamflow_MediumTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations (Medium term) \nOverall Average Streamflow (MMD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

print(overall_average_streamflow_MediumTerm_Map)

ggsave(filename =(file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_MediumTerm_Map_MMD.jpg")), 
       plot = overall_average_streamflow_MediumTerm_Map, 
       width = 8, height = 6, dpi = 300)












overall_average_streamflow_LongTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (_MMD)", 
                        limits = c(0, 2.172572)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations (Long term) \nOverall Average Streamflow (_MMD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

print(overall_average_streamflow_LongTerm_Map)

ggsave(filename =(file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_LongTerm_Map_MMD.jpg")), 
       plot = overall_average_streamflow_LongTerm_Map, 
       width = 8, height = 6, dpi = 300)
