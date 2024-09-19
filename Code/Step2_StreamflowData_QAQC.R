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


StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_step1.rds") #%>% 
StreamflowData



### Make another Column for which years it had been sampled 
extract_years<- function(df){
  year(as.Date(df$Date))
}

StreamflowData<- StreamflowData %>%  mutate(SamplingYears=map(streamflow_data,extract_years))



##make a map with all the stations 

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
Streamflowdata_locations<-ggplot() +
  # Add the shapefile layer
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = StreamflowData, 
             aes(x = station_lon, y = station_lat),
             size = 6,  # Set a constant size for all points
             alpha = 0.8) +
  # Titles and labels
  labs(title = "Streamflow data locations (All stations)") +
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

ggsave(filename = '/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Step2_Streamflowdata_locationsAll.jpg', 
       plot = Streamflowdata_locations, 
       width = 8, height = 6, dpi = 300)  


#######make time line figures
TimePlotDF<- StreamflowData %>%  
  unnest(cols=SamplingYears)


FinalData_cleaned_split <- split(TimePlotDF, TimePlotDF$site_no)


min_year <- min(TimePlotDF$SamplingYears)
max_year <- max(TimePlotDF$SamplingYears)

plot_list <- list()

for (i in seq(1, length(FinalData_cleaned_split), by = 18)) {
  end_idx <- min(i + 17, length(FinalData_cleaned_split))  
  
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
}




Timelineplot1 <- (plot_list[[1]] + plot_list[[2]] + plot_list[[3]]) / 
  (plot_list[[4]] + plot_list[[5]] + plot_list[[6]])
Timelineplot2 <- (plot_list[[7]] + plot_list[[8]] + plot_list[[9]]) / 
  (plot_list[[10]] + plot_list[[11]] + plot_list[[12]])
Timelineplot3 <- (plot_list[[13]]+plot_list[[14]])
#plot_list[[15]]

ggsave(filename = '/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Step2_Timelineplot1.jpg', 
       plot = Timelineplot1, 
       width = 14, height = 6, dpi = 300)  
ggsave(filename = '/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Step2_Timelineplot2.jpg', 
       plot = Timelineplot2, 
       width = 14, height = 6, dpi = 300)  
ggsave(filename = '/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Step2_Timelineplot3.jpg', 
       plot = Timelineplot3, 
       width = 14, height = 6, dpi = 300)  







#####missing data info


##make sequential dates df, combine it with original data, so that missing days would have na 

fill_missing_dates <- function(data) {
  if (nrow(data) > 0) {
    full_dates <- tibble(Date = seq(min(data$Date, na.rm = TRUE), max(data$Date, na.rm = TRUE), by = "day"))
    
    full_data <- full_dates %>%
      left_join(data, by = "Date")
    
    return(full_data)
  } else {
    return(tibble(Date = as.Date(character()), mean_streamflow = numeric()))
  }
}

StreamflowData2 <- StreamflowData %>%
  mutate(full_streamflow_data = map(streamflow_data, fill_missing_dates))


#check data 

k=StreamflowData2 %>%  select(station_name,full_streamflow_data,streamflow_data)
k


df_wm<- data.frame(k$full_streamflow_data[2])
df_wom<- data.frame(k$streamflow_data[2])

df_wm<- data.frame(k$full_streamflow_data[219])
df_wom<- data.frame(k$streamflow_data[219])


#### find the missing values


StreamflowData<- StreamflowData2


calculate_metrics <- function(data) {
  if (nrow(data) > 0) {
    start_year <- min(year(data$Date), na.rm = TRUE)
    end_year <- max(year(data$Date), na.rm = TRUE)
    start_day <- min(data$Date, na.rm = TRUE)
    end_day <- max(data$Date, na.rm = TRUE)
    
    num_years <- n_distinct(year(data$Date), na.rm = TRUE)
    total_days <- as.numeric(difftime(end_day, start_day, units = "days")) + 1
    missing_days <- sum(is.na(data$mean_streamflow)) # all dates are counted as we are taking the full_streamflow_data 
    percent_missing <- (missing_days / total_days) * 100
  } else {
    start_year <- NA
    end_year <- NA
    start_day <- NA
    end_day <- NA
    num_years <- NA
    total_days <- NA
    missing_days <- NA
    percent_missing <- NA
  }
  
  tibble(start_year, end_year, start_day, end_day, num_years, total_days, missing_days, percent_missing)
}

metrics <- StreamflowData %>%
  mutate(metrics = map(full_streamflow_data, calculate_metrics)) %>%
  unnest(metrics)

print(metrics)

###check data individually to see if calculation is ok

CheckData<-metrics %>% select(station_name,full_streamflow_data,start_year, end_year, start_day, end_day, num_years, total_days, missing_days, percent_missing)

df_wm<- data.frame(k$full_streamflow_data[2])
df_wom<- data.frame(k$streamflow_data[2])
CheckData$missing_days[2]
nrow(df_wm)-nrow(df_wom)
pm=((nrow(df_wm)-nrow(df_wom))/nrow(df_wm))*100
pm
CheckData$percent_missing[2]
CheckData$num_years[2]
num_years <- (CheckData$end_year[2] - CheckData$start_year[2] )+ 1
num_years


df_wm<- data.frame(k$full_streamflow_data[168])
df_wom<- data.frame(k$streamflow_data[168])
CheckData$missing_days[168]
nrow(df_wm)-nrow(df_wom)
pm=((nrow(df_wm)-nrow(df_wom))/nrow(df_wm))*100
pm
CheckData$percent_missing[168]
CheckData$num_years[168]


StreamflowData<- metrics





### percent missing plot 
max_percent_missing <- max(StreamflowData$percent_missing, na.rm = TRUE)

histogram_plot_N <- ggplot(StreamflowData, aes(x = percent_missing)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Percent of missing data",
       x = "Percent of missing data",
       y = "Count of Stations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size = 14),  # Increase font size for x-axis label
    axis.title.y = element_text(size = 14),  # Increase font size for y-axis label
    #panel.grid = element_blank(),  # Remove grid
    #axis.ticks = element_line(color = "black")  # Ensure tick marks are shown
  ) 

print(histogram_plot_N)









# filter the data where percent missing is less than 10



filtered_tibble <- StreamflowData %>%
  filter(percent_missing < 10)



histogram_plot <- ggplot(filtered_tibble, aes(x = num_years)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Number of Years of Data per Station \nless than 10% missing data",
       x = "Number of Years of Data",
       y = "Count of Stations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
  ) 
#scale_x_continuous(limits = c(NA, 150)) +
#scale_y_continuous(limits = c(NA, 15)) 
# Display the histogram
print(histogram_plot)





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
filtered_tibble_locations<-ggplot() +
  # Add the shapefile layer
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = filtered_tibble, 
             aes(x = station_lon, y = station_lat),
             size = 6,  # Set a constant size for all points
             alpha = 0.8) +
  # Titles and labels
  labs(title = "Streamflow data locations (percent_missing < 10)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )
filtered_tibble_locations

ggsave(filename = '/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Step2_Streamflowdata_locations_lessthan10percentmiss.jpg', 
       plot = filtered_tibble_locations, 
       width = 8, height = 6, dpi = 300)  # Optional: adjust width, height, and dpi as needed





saveRDS(filtered_tibble,'/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_step2.rds')
