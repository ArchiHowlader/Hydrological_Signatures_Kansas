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

AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_step2.rds"))
AllYear_StreamflowData


######Calculate mm per day
cfs_to_m3s <- 0.0283168  
seconds_per_day <- 86400 

retrieve_watershed_area <- function(site_no) {
  site_info <- readNWISsite(site_no)
  if (!is.null(site_info$drain_area_va)) {
    return(site_info$drain_area_va)  # Drainage area in square miles
  } else {
    return(NA)  
  }
}

AllYear_StreamflowData <- AllYear_StreamflowData %>%
  mutate(watershed_area_sqmi = map_dbl(site_no, retrieve_watershed_area)) %>%
  mutate(watershed_area_m2 = measurements::conv_unit(watershed_area_sqmi, from = "mi2", to = "m2"))
# 549 square miles 06863500
NoData<-AllYear_StreamflowData %>% filter( is.na(AllYear_StreamflowData$watershed_area_m2))
NoData$site_no


convert_cfs_to_mm_per_day <- function(cfs_streamflow, watershed_area_m2) {
  streamflow_m3_per_day <- cfs_streamflow * cfs_to_m3s * seconds_per_day
  streamflow_mm_per_day <- (streamflow_m3_per_day / watershed_area_m2) * 1000
  return(streamflow_mm_per_day)
}

AllYear_StreamflowData$streamflow_data <- AllYear_StreamflowData$streamflow_data %>%
  map2(AllYear_StreamflowData$watershed_area_m2, ~ .x %>%
         mutate(mean_streamflow_mm_per_day = convert_cfs_to_mm_per_day(mean_streamflow, .y)))
print(AllYear_StreamflowData)

ggplot(data = AllYear_StreamflowData, aes(x =  seq_along(site_no), y = watershed_area_m2)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Use a bar plot
  theme_minimal() +  # Minimal theme for a clean look
  labs(
    title = "Watershed Area by Site",
    x = "Site Number",
    y = "Watershed Area (m²)"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  )


df_cfs<- data.frame(AllYear_StreamflowData$streamflow_data[[5]])
mm<- ggplot(data=df_cfs,aes(x=df_cfs$Date,y=df_cfs$mean_streamflow_mm_per_day))+
  geom_line()
cfs<- ggplot(data=df_cfs,aes(x=df_cfs$Date,y=df_cfs$mean_streamflow))+
  geom_line()


mm/cfs


# 549 square miles 06863500


#remove canal stations
canal_stations<- AllYear_StreamflowData[grepl('canal',AllYear_StreamflowData$station_name,ignore.case = TRUE),]
AllYear_StreamflowData_NoCanal<- AllYear_StreamflowData[!grepl('canal',AllYear_StreamflowData$station_name,ignore.case = TRUE),]



#plot of no data available stations 

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
NoAreaData_locations<-ggplot() +
  # Add the shapefile layer
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = NoData, 
             aes(x = station_lon, y = station_lat),
             size = 2,  # Set a constant size for all points
             alpha = 0.8) +
  geom_text(data = NoData, 
            aes(x = station_lon, y = station_lat, label = station_name),  
            size = 3,  
            nudge_y = 0.02, 
            color = "black")
  # Titles and labels
  labs(title = "NoAreaData_locations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

ggsave(filename = (file.path(file_Path_Variable_O,'Step2a_NoLocation.jpg')), 
       plot = NoAreaData_locations, 
       width = 8, height = 6, dpi = 300)  # Optional: adjust width, height, and dpi as needed




##Make seperate data.frame for stream flow data with mmd unit
AllYear_StreamflowData_NoCanalF<-na.omit(AllYear_StreamflowData_NoCanal)
AllYear_StreamflowData_NoCanalF<- AllYear_StreamflowData_NoCanalF %>% rename(streamflow_data_CFS_MMD=streamflow_data)
AllYear_StreamflowData_NoCanalFk<- AllYear_StreamflowData_NoCanalF %>% 
  mutate(StreamflowMMD=map(streamflow_data_CFS_MMD,~select(.x,Date,mean_streamflow_mm_per_day)))
AllYear_StreamflowData_NoCanalFk<- AllYear_StreamflowData_NoCanalFk %>% mutate(streamflow_data = StreamflowMMD)
AllYear_StreamflowData_NoCanalFk

MMD<- 'MMD'
AllYear_StreamflowData_NoCanalFk$streamflowUnit<- MMD                                                                         




# save that to rds 
saveRDS(NoData, file = file.path(file_Path_Variable_O, "step2a_Nodata_Canal_WA.rds"))
saveRDS(AllYear_StreamflowData_NoCanalFk, file = file.path(file_Path_Variable_O, "AllYear_StreamflowData_MMD.rds"))
