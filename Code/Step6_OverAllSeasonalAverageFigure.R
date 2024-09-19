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
AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_step2.rds"))
AllYear_StreamflowData



MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_MediumSubset_step3.rds")) 
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_LongSubset_step3.rds"))
LongTerm_StreamflowData





############Figure 
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"

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

seasonal_average_streamflow <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  mutate(Season = case_when(
    Month %in% c(9, 10, 11) ~ "Fall",
    Month %in% c(12, 1, 2)  ~ "Winter",
    Month %in% c(3, 4, 5)   ~ "Spring",
    Month %in% c(6, 7, 8)   ~ "Summer"
  )) %>%
  group_by(site_no, Season) %>%
  summarise(MeanSeasonalQ_cfs = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()

seasonal_average_streamflow_pivot <- seasonal_average_streamflow %>%
  pivot_wider(names_from = Season, values_from = MeanSeasonalQ_cfs, 
              names_prefix = "Mean", values_fill = NA)

print(seasonal_average_streamflow_pivot)

seasonal_average_streamflow_pivot<- seasonal_average_streamflow_pivot %>% 
  left_join(AllYear_StreamflowData, by='site_no')


seasonal_average_streamflow_pivot<- seasonal_average_streamflow_pivot %>% 
  select(MeanFall, MeanSpring ,MeanSummer, MeanWinter ,station_name ,site_no,station_lat, station_lon)
#check data 

# 
# l=readRDS(file.path(file_Path_Variable_O,"LongTerm_StreamflowData_Annual_step4.rds"))
# c=l %>% filter(site_no==seasonal_average_streamflow_pivot$site_no[2])
# mean(c$MeanSummer)


seasonal_average_streamflow_pivot<- na.omit(seasonal_average_streamflow_pivot)

max(seasonal_average_streamflow_pivot$MeanFall)

overall_average_streamflow_LongTerm_Map_MeanFall <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanFall, 
                 size = MeanFall),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanFall (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

print(overall_average_streamflow_LongTerm_Map_MeanFall)

ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanFall_AllStation.jpg")), 
       plot = overall_average_streamflow_LongTerm_Map_MeanFall, 
       width = 8, height = 6, dpi = 300)







overall_average_streamflow_LongTerm_Map_MeanSummer <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanSummer, 
                 size = MeanSummer),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSummer (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSummer_AllStation.jpg")), 
       plot = overall_average_streamflow_LongTerm_Map_MeanSummer, 
       width = 8, height = 6, dpi = 300)














overall_average_streamflow_LongTerm_Map_MeanSpring<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanSpring, 
                 size = MeanSpring),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSpring (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSpring_AllStation.jpg")), 
       plot = overall_average_streamflow_LongTerm_Map_MeanSpring, 
       width = 8, height = 6, dpi = 300)















overall_average_streamflow_LongTerm_Map_MeanWinter<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanWinter, 
                 size = MeanWinter),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanWinter (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanWinter_AllStation.jpg")), 
       plot = overall_average_streamflow_LongTerm_Map_MeanWinter, 
       width = 8, height = 6, dpi = 300)







SeasonalAveragePlot<- (overall_average_streamflow_LongTerm_Map_MeanFall+overall_average_streamflow_LongTerm_Map_MeanSummer)/
  (overall_average_streamflow_LongTerm_Map_MeanWinter+overall_average_streamflow_LongTerm_Map_MeanSpring)





ggsave(filename =(file.path(file_Path_Variable_O,"Step6_SeasonalAveragePlot_AllYear_AllStation.jpg")), 
       plot = SeasonalAveragePlot, 
       width = 16, height = 8, dpi = 300)











########################medium term





seasonal_average_streamflow_pivot_MediumTerm <- seasonal_average_streamflow_pivot %>%
  filter(site_no %in% MediumTerm_StreamflowData$site_no)








overall_average_streamflow_Map_MeanFall_MediumTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanFall, 
                 size = MeanFall),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanFall (cfs)\n Medium Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanFall_MediumTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanFall_MediumTerm, 
       width = 8, height = 6, dpi = 300)







overall_average_streamflow_Map_MeanSummer_MediumTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanSummer, 
                 size = MeanSummer),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSummer (cfs)\n Medium Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSummer__MediumTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanSummer_MediumTerm, 
       width = 8, height = 6, dpi = 300)














overall_average_streamflow_Map_MeanSpring_MediumTerm<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanSpring, 
                 size = MeanSpring),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSpring (cfs) \n Medium Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSpring_AllStation_MediumTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanSpring_MediumTerm, 
       width = 8, height = 6, dpi = 300)















overall_average_streamflow_Map_MeanWinter_MediumTerm<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanWinter, 
                 size = MeanWinter),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanWinter (cfs)\n Medium Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanWinter__MediumTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanWinter_MediumTerm, 
       width = 8, height = 6, dpi = 300)







SeasonalAveragePlot_MediumTerm<- (overall_average_streamflow_Map_MeanFall_MediumTerm+overall_average_streamflow_Map_MeanSummer_MediumTerm)/
  (overall_average_streamflow_Map_MeanWinter_MediumTerm+overall_average_streamflow_Map_MeanSpring_MediumTerm)





ggsave(filename =(file.path(file_Path_Variable_O,"Step6_SeasonalAveragePlot_AllYear__MediumTerm.jpg")), 
       plot = SeasonalAveragePlot_MediumTerm, 
       width = 16, height = 8, dpi = 300)



























#############################Long term







seasonal_average_streamflow_pivot_LongTerm <- seasonal_average_streamflow_pivot %>%
  filter(site_no %in% LongTerm_StreamflowData$site_no)








overall_average_streamflow_Map_MeanFall_LongTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanFall, 
                 size = MeanFall),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanFall (cfs)\n long Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanFall_LongTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanFall_LongTerm, 
       width = 8, height = 6, dpi = 300)







overall_average_streamflow_Map_MeanSummer_LongTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanSummer, 
                 size = MeanSummer),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSummer (cfs)\n long Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSummer__LongTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanSummer_LongTerm, 
       width = 8, height = 6, dpi = 300)














overall_average_streamflow_Map_MeanSpring_LongTerm<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanSpring, 
                 size = MeanSpring),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanSpring (cfs) \n long Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanSpring_AllStation_LongTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanSpring_LongTerm, 
       width = 8, height = 6, dpi = 300)















overall_average_streamflow_Map_MeanWinter_LongTerm<- ggplot() +
  geom_sf(data = combined_shp_leaflet, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = seasonal_average_streamflow_pivot_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color =MeanWinter, 
                 size = MeanWinter),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, 10000)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations \n MeanWinter (cfs)\n Medium Term") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


ggsave(filename =(file.path(file_Path_Variable_O,"Step6_overall_average_streamflow_AllYear_Map_MeanWinter__LongTerm.jpg")), 
       plot = overall_average_streamflow_Map_MeanWinter_LongTerm, 
       width = 8, height = 6, dpi = 300)







SeasonalAveragePlot_LongTerm<- (overall_average_streamflow_Map_MeanFall_LongTerm+overall_average_streamflow_Map_MeanSummer_LongTerm)/
  (overall_average_streamflow_Map_MeanWinter_LongTerm+overall_average_streamflow_Map_MeanSpring_LongTerm)





ggsave(filename =(file.path(file_Path_Variable_O,"Step6_SeasonalAveragePlot_AllYear__LongTerm.jpg")), 
       plot = SeasonalAveragePlot_LongTerm, 
       width = 16, height = 8, dpi = 300)


