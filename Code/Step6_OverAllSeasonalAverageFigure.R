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


AllYear_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_step2.rds") #%>% 
AllYear_StreamflowData



MediumTerm_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_MediumSubset_step3.rds") #%>% 
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_LongSubset_step3.rds") #%>% 
LongTerm_StreamflowData





############Figure 

desoto_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/DeSoto_shp/DeSoto.shp"
watershed_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/WatershedBoundary_KN_20230113/watershed_bndry.shp"
RiverNetwork<- "/Users/ahowl/Desktop/KGS Data analysis/rivers_ksrb/rivers_ksrb.shp"
desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
#combined_shp <- st_union(combined_shp, RiverNetwork_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)

### All average 

AllYear_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_step2.rds") #%>% 
AllYear_StreamflowData





seasonal_average_streamflow <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  mutate(Season = case_when(
    Month %in% c(9, 10, 11) ~ "Fall",
    Month %in% c(12, 1, 2)  ~ "Winter",
    Month %in% c(3, 4, 5)   ~ "Spring",
    Month %in% c(6, 7, 8)   ~ "Summer"
  )) %>%
  group_by(site_no, Year, Season) %>%
  summarise(MeanSeasonalQ_cfs = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()

seasonal_average_streamflow_pivot <- seasonal_average_streamflow %>%
  pivot_wider(names_from = Season, values_from = MeanSeasonalQ_cfs, 
              names_prefix = "Mean", values_fill = NA)

print(seasonal_average_streamflow_pivot)
seasonal_average_streamflow_pivot