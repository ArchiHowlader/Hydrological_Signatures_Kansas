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
library(Kendall)

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

AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_step2.rds") )
AllYear_StreamflowData



MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_step3.rds"))
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_step3.rds"))
LongTerm_StreamflowData



overall_average_streamflow_AllYear <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_AllYear)

overall_average_streamflow_AllYear<- overall_average_streamflow_AllYear %>% 
  left_join(AllYear_StreamflowData, by='site_no')





overall_average_streamflow_MediumTerm <- MediumTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_MediumTerm)

overall_average_streamflow_MediumTerm<- overall_average_streamflow_MediumTerm %>% 
  left_join(MediumTerm_StreamflowData, by='site_no')









overall_average_streamflow_LongTerm <- LongTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  summarise(OverallAvgStreamflow__MMD = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()

print(overall_average_streamflow_LongTerm)

overall_average_streamflow_LongTerm<- overall_average_streamflow_LongTerm %>% 
  left_join(LongTerm_StreamflowData, by='site_no')





maxcf<- max(overall_average_streamflow_AllYear$OverallAvgStreamflow__MMD)
mincf<- min(overall_average_streamflow_AllYear$OverallAvgStreamflow__MMD)

overall_average_streamflow_AllYear_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_AllYear, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  
  labs(title = "Streamflow Data Locations (All) \nOverall Average Streamflow (CFS)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

#print(overall_average_streamflow_AllYear_Map)
# 
# ggsave(filename = (file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_AllYear_MMD.jpg")), 
#        plot = overall_average_streamflow_AllYear_Map, 
#        width = 8, height = 6, dpi = 300)
# 







overall_average_streamflow_MediumTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations (Medium term) \nOverall Average Streamflow (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

#print(overall_average_streamflow_MediumTerm_Map)
# 
# ggsave(filename =(file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_MediumTerm_Map_MMD.jpg")), 
#        plot = overall_average_streamflow_MediumTerm_Map, 
#        width = 8, height = 6, dpi = 300)
# 











overall_average_streamflow_LongTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = overall_average_streamflow_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAvgStreamflow__MMD, 
                 size = OverallAvgStreamflow__MMD),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Avg Streamflow (cfs)", 
                        limits = c(0, maxcf)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Streamflow Data Locations (Long term) \nOverall Average Streamflow (cfs)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

#print(overall_average_streamflow_LongTerm_Map)


LongMedium<- overall_average_streamflow_MediumTerm_Map/overall_average_streamflow_LongTerm_Map

# 
# 
# ggsave(filename =(file.path(file_Path_Variable_O,"Step4_overall_average_streamflow_LongTerm_Map_MMD.jpg")), 
#        plot = overall_average_streamflow_LongTerm_Map, 
#        width = 8, height = 6, dpi = 300)

# 
# 
# ggsave(filename =(file.path(file_Path_Variable_O,"Step5a_LongMedium.jpg")), 
#        plot = LongMedium, 
#        width = 14, height = 10, dpi = 300)
# 
# 
# 
# 






#######Add annual trend to the figure

# Calculate the yearly average 



yearly_avg_streamflow_MediumTerm<- MediumTerm_StreamflowData %>% 
  unnest(cols=streamflow_data) %>% 
  mutate(Year=year(Date)) %>% 
  group_by(site_no,Year) %>% 
  summarise(MeanAnnualQ_mmd=mean(mean_streamflow,na.rm=TRUE)) %>% 
  ungroup()

print(yearly_avg_streamflow_MediumTerm)


yearly_avg_streamflow_LongTerm<- LongTerm_StreamflowData %>% 
  unnest(cols=streamflow_data) %>% 
  mutate(Year=year(Date)) %>% 
  group_by(site_no,Year) %>% 
  summarise(MeanAnnualQ_mmd=mean(mean_streamflow,na.rm=TRUE)) %>% 
  ungroup()

print(yearly_avg_streamflow_LongTerm)




# Function to calculate Kendall tau for each site
calculate_kendall_tau <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau = Kendall(Year, MeanAnnualQ_mmd)$tau, 
      p_value = Kendall(Year, MeanAnnualQ_mmd)$sl  # 'sl' is the p-value returned by Kendall function
    ) %>%
    ungroup()
}

tau_MediumTerm <- calculate_kendall_tau(yearly_avg_streamflow_MediumTerm)
tau_LongTerm <- calculate_kendall_tau(yearly_avg_streamflow_LongTerm)

print(tau_MediumTerm)
print(tau_LongTerm)

#add lat lon to the tau table

tau_MediumTerm<- tau_MediumTerm %>% left_join(MediumTerm_StreamflowData,by='site_no')
tau_MediumTerm<- tau_MediumTerm %>% select(site_no,Tau,p_value,station_name,station_lat,station_lon,streamflowUnit)



tau_LongTerm<- tau_LongTerm %>% left_join(LongTerm_StreamflowData,by='site_no')
tau_LongTerm<- tau_LongTerm %>% select(site_no,Tau,p_value,station_name,station_lat,station_lon,streamflowUnit)



tau_LongTerm <- tau_LongTerm %>%
  mutate(significant = ifelse(p_value < 0.05, "Significant", "NotSignificant"))


maxT<- max(tau_LongTerm$Tau)
minT<- min(tau_LongTerm$Tau)

kendall_tau_map_LongTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = tau_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = Tau, 
                 shape = significant, 
                 fill=significant,
                 size = abs(Tau)), 
             alpha = 0.8, stroke = 1.2) + 
  scale_color_viridis_c(option = "plasma", name = "Tau Value", limits = c(minT, maxT)) +  # Set the color scale limits to minT and maxT
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude") +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") + 
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white")) +  
  labs(title = "Streamflow Data Locations (Long term) \nKendall Tau Values and Significance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )


# ggsave(filename =(file.path(file_Path_Variable_O,"Step5a_kendall_tau_map_LongTerm.jpg")), 
#        plot = kendall_tau_map_LongTerm, 
#        width = 14, height = 10, dpi = 300)
# 


tau_MediumTerm <- tau_MediumTerm %>%
  mutate(significant = ifelse(p_value < 0.05, "Significant", "NotSignificant"))

# Calculate max and min tau values for MediumTerm
maxT_Medium <- max(tau_MediumTerm$Tau, na.rm = TRUE)
minT_Medium <- min(tau_MediumTerm$Tau, na.rm = TRUE)

# Create the Kendall Tau map for MediumTerm
kendall_tau_map_MediumTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  
  geom_point(data = tau_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = Tau,  
                 fill=significant,
                 shape = significant,  
                 size = abs(Tau)),  
             alpha = 0.8, stroke = 1.2) +  
  scale_color_viridis_c(option = "plasma", name = "Tau Value", limits = c(minT_Medium, maxT_Medium)) +  
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude") +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white")) +  
  labs(title = "Streamflow Data Locations (Medium term) \nKendall Tau Values and Significance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()   
  )

# 
# ggsave(filename =(file.path(file_Path_Variable_O, "Step5a_kendall_tau_map_MediumTerm.jpg")), 
#        plot = kendall_tau_map_MediumTerm, 
#        width = 14, height = 10, dpi = 300)
# 


CombinedPlot<- (overall_average_streamflow_MediumTerm_Map+overall_average_streamflow_LongTerm_Map)/(kendall_tau_map_MediumTerm+kendall_tau_map_LongTerm)



ggsave(filename =(file.path(file_Path_Variable_O, "Step5a_kendall_CombinedPlot_CFS.jpg")), 
       plot = CombinedPlot, 
       width = 18, height = 12, dpi = 300)
