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


file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"

StreamflowData <- readRDS(file.path(file_Path_Variable_O,"AllYear_StreamflowData_MMD.rds")) #%>% 
StreamflowData


# Medium term: data on >90% of days between 1/1/1979 and 12/31/2023
# Long term: data on >90% of days between 1/1/1944 and 12/31/2023

#Medium Term 

library(dplyr)
library(lubridate)
library(purrr)
library(tibble)

start_date <- as.Date("1979-01-01")
end_date <- as.Date("2023-12-31")

full_date_range <- tibble(Date = seq(start_date, end_date, by = "day"))

combine_with_full_date_range <- function(data) {
  full_data <- full_date_range %>%
    left_join(data, by = "Date")  # Left-join to retain all dates in the range, fill missing with NA
  return(full_data)
}

StreamflowData_filtered <- StreamflowData %>%
  mutate(
    full_streamflow_with_complete_dates = map(full_streamflow_data, combine_with_full_date_range),
    
    missing_days_forMediumSubset = map_dbl(full_streamflow_with_complete_dates, ~ sum(is.na(.x$mean_streamflow))),
    
    total_days_in_range_forMediumSubset = nrow(full_date_range)
  ) %>%
  mutate(
    percent_available_forMediumSubset = (total_days_in_range_forMediumSubset - missing_days_forMediumSubset) / total_days_in_range_forMediumSubset * 100
  ) %>%  
  filter(percent_available_forMediumSubset > 90)

print(StreamflowData_filtered)


#check manually
df<- data.frame(StreamflowData_filtered$streamflow_data[1])
df
df2<- StreamflowData_filtered %>% filter(site_no=='06836500')
df2
p=data.frame(df2$streamflow_data)
#make a time line fig 


TimePlotDF<- StreamflowData_filtered %>%  
  unnest(cols=SamplingYears)
df2<- TimePlotDF %>% filter(site_no=='06836500')
df2

FinalData_cleaned_split <- split(TimePlotDF, TimePlotDF$site_no)


min_year <- min(TimePlotDF$SamplingYears)
max_year <- max(TimePlotDF$SamplingYears)

plot_list <- list()

for (i in seq(1, length(FinalData_cleaned_split), by = 18)) {
  end_idx <- min(i + 17, length(FinalData_cleaned_split))
  print(end_idx)
  combined_data <- bind_rows(FinalData_cleaned_split[i:end_idx])
  
  p <- ggplot(combined_data, aes(x = SamplingYears, y = site_no)) +
    geom_point(color = "blue", size = 3) +  # Points for each sampling event
    labs(title = paste("Sampling Years for Stations", i, "to", end_idx),
         x = "Year",
         y = "Station") +
    theme_minimal(base_size = 15) +
    theme(axis.text.y = element_text(size = 8)) +
    xlim(min_year, max_year)
  plot_list[[length(plot_list) + 1]] <- p
  
  # Print the plot for each batch
}

Timelineplot1 <- (plot_list[[1]] + plot_list[[2]] )/(plot_list[[3]] + plot_list[[4]] )

# ggsave(filename = (file.path(file_Path_Variable_O,"Step3_Timelineplot1_MediumSubset_MMD.jpg")), 
#        plot = Timelineplot1, 
#        width = 16, height = 8, dpi = 300)  





#map creation



##make a map with all the stations 


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
StreamflowData_filtered_locations<-ggplot() +
  # Add the shapefile layer
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = StreamflowData_filtered, 
             aes(x = station_lon, y = station_lat),
             size = 6,  # Set a constant size for all points
             alpha = 0.8) +
  # Titles and labels
  labs(title = "Streamflow data locations (Medium subset) \ndata on >90% of days between 1/1/1979 and 12/31/2023 ") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )
#Streamflowdata_locations

# ggsave(filename = (file.path(file_Path_Variable_O,"Step3_Streamflowdata_locations_MediumSubset_MMD.jpg")), 
#        plot = StreamflowData_filtered_locations, 
#        width = 8, height = 6, dpi = 300)  


#######filtering dates for the medium term subset
###data on >90% of days between 1/1/1979 and 12/31/2023
StreamflowData_filtered_seasonal<-StreamflowData_filtered



StreamflowData_filtered <- StreamflowData_filtered %>%
  mutate(streamflow_data = map(streamflow_data, ~ .x %>%
                                 filter(Date >= as.Date("1979-01-01") & Date <= as.Date("2023-12-31"))))

StreamflowData_filtered

StreamflowData_filtered$streamflow_data <- lapply(StreamflowData_filtered$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})





StreamflowData_filtered_seasonal <- StreamflowData_filtered_seasonal %>%
  mutate(streamflow_data = map(streamflow_data, ~ .x %>%
                                 filter(Date >= as.Date("1979-01-01") & Date <= as.Date("2024-03-01"))))

StreamflowData_filtered_seasonal


StreamflowData_filtered_seasonal$streamflow_data <- lapply(StreamflowData_filtered_seasonal$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})

#saveRDS(StreamflowData_filtered,file = file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))



# 
# StreamflowData_M_CFS <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_MediumSubset_step3.rds")) #%>% 
# Check=anti_join(StreamflowData_M_CFS,StreamflowData_filtered,by='site_no')
# CheckDF<- data.frame(Check$streamflow_data[1])
# StreamflowData <- readRDS(file.path(file_Path_Variable_O,"AllYear_StreamflowData_MMD.rds")) #%>% 
# StreamflowData %>% filter(site_no==Check$site_no)
# k=StreamflowData_M_CFS %>% filter(site_no=='06852500')

###########################Long term subset


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


file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"

StreamflowData <- readRDS(file.path(file_Path_Variable_O,"AllYear_StreamflowData_MMD.rds")) #%>% 
StreamflowData

# Medium term: data on >90% of days between 1/1/1979 and 12/31/2023
# Long term: data on >90% of days between 1/1/1944 and 12/31/2023

#Long term



# Define the date range (Medium Term)
start_date <- as.Date("1944-01-01")
end_date <- as.Date("2023-12-31")

full_date_range <- tibble(Date = seq(start_date, end_date, by = "day"))

combine_with_full_date_range <- function(data) {
  full_data <- full_date_range %>%
    left_join(data, by = "Date")  # Left-join to retain all dates in the range, fill missing with NA
  return(full_data)
}

StreamflowData_filtered <- StreamflowData %>%
  mutate(
    full_streamflow_with_complete_dates = map(full_streamflow_data, combine_with_full_date_range),
    
    missing_days_forLongSubset = map_dbl(full_streamflow_with_complete_dates, ~ sum(is.na(.x$mean_streamflow))),
    
    total_days_in_range_forLongSubset = nrow(full_date_range)
  ) %>%
  mutate(
    percent_available_forLongSubset = (total_days_in_range_forLongSubset - missing_days_forLongSubset) / total_days_in_range_forLongSubset * 100
  ) %>%  
  filter(percent_available_forLongSubset > 90)

print(StreamflowData_filtered)


#check manually
df<- data.frame(StreamflowData_filtered$streamflow_data[1])
df
df2<- StreamflowData_filtered %>% filter(site_no=='06836500')
df2
p=data.frame(df2$streamflow_data)
#make a time line fig 


TimePlotDF<- StreamflowData_filtered %>%  
  unnest(cols=SamplingYears)
df2<- TimePlotDF %>% filter(site_no=='06836500')
df2

FinalData_cleaned_split <- split(TimePlotDF, TimePlotDF$site_no)


min_year <- min(TimePlotDF$SamplingYears)
max_year <- max(TimePlotDF$SamplingYears)

plot_list <- list()

for (i in seq(1, length(FinalData_cleaned_split), by = 18)) {
  end_idx <- min(i + 17, length(FinalData_cleaned_split))
  print(end_idx)
  combined_data <- bind_rows(FinalData_cleaned_split[i:end_idx])
  
  p <- ggplot(combined_data, aes(x = SamplingYears, y = site_no)) +
    geom_point(color = "blue", size = 3) +  # Points for each sampling event
    labs(title = paste("Sampling Years for Stations", i, "to", end_idx),
         x = "Year",
         y = "Station") +
    theme_minimal(base_size = 15) +
    theme(axis.text.y = element_text(size = 8)) +
    xlim(min_year, max_year)
  plot_list[[length(plot_list) + 1]] <- p
  
  # Print the plot for each batch
}

Timelineplot1 <- (plot_list[[1]] + plot_list[[2]] )

# ggsave(filename = (file.path(file_Path_Variable_O,"Step3_Timelineplot1_LongSubset_MMD.jpg")), 
#        plot = Timelineplot1, 
#        width = 16, height = 8, dpi = 300)  





#map creation



##make a map with all the stations 
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
StreamflowData_filtered_locations<-ggplot() +
  # Add the shapefile layer
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = StreamflowData_filtered, 
             aes(x = station_lon, y = station_lat),
             size = 6,  # Set a constant size for all points
             alpha = 0.8) +
  # Titles and labels
  labs(title = "Streamflow data locations (Long subset) \ndata on >90% of days between 1/1/1944 and 12/31/2023 ") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )
#Streamflowdata_locations

# ggsave(filename = (file.path(file_Path_Variable_O,"Step3_Streamflowdata_locations_LongSubset_MMD.jpg")), 
#        plot = StreamflowData_filtered_locations, 
#        width = 8, height = 6, dpi = 300)  




######filtering dates for the long term subset
###data on >90% of days between 1/1/1944 and 12/31/2023

StreamflowData_filtered <- StreamflowData_filtered %>%
  mutate(streamflow_data = map(streamflow_data, ~.x %>%
                                 filter(Date >= as.Date('1944-01-01') & Date <= as.Date('2023-12-31'))))


StreamflowData_filtered$streamflow_data <- lapply(StreamflowData_filtered$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})


#saveRDS(StreamflowData_filtered,file = file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_MMD_step3.rds"))




StreamflowData_M_CFS <- readRDS(file.path(file_Path_Variable_O,"streamflow_tibbles_Filtered_LongSubset_step3.rds")) #%>% 
StreamflowData_M_CFS




