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



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_step3.rds"))
AllYear_StreamflowData


##Logical steps

# # fake data
# df <- data.frame(date = seq(1, 10),
#                  Q_m3d = c(seq(5,1,-1), seq(1,5,1)))
# 
# # fake threshold
# threshold <- 3
# #value
# # daily drought deficit
# 
# valueofthreshold
# df$deficit_m3d <- ifelse(df$Q_m3d < valueofthreshold, valueofthreshold - df$Q_m3d, 0)
# 
# # total drought deficit
# #  when flow is in units of L3/day or L/day, so we can just add up days
# #  if flow has a different time denominator, woudl need to convert accordingly
# sum(df$deficit_m3d)





threshold_values_df <- data.frame()

for (i in 1:nrow(AllYear_StreamflowData)) {
  #i=8
  streamflow_data <- data.frame(AllYear_StreamflowData$streamflow_data[[i]])
  streamflow_data$Date <- lubridate::as_date(streamflow_data$Date)  
  
  streamflow_data <- streamflow_data %>%
    rename(value = mean_streamflow)
  
  df <- streamflow_data
  
  moving_average_number = 7
  df$mean_value <- round(zoo::rollmean(df$value, k = moving_average_number, align = "center", na.pad = TRUE), digits = 4)
  
  # Calculate fixed thresholds
  threshold_fixed_2 <- quantile(df$mean_value, probs = 0.02, na.rm = TRUE)
  threshold_fixed_5 <- quantile(df$mean_value, probs = 0.05, na.rm = TRUE)
  threshold_fixed_10 <- quantile(df$mean_value, probs = 0.10, na.rm = TRUE)
  threshold_fixed_20 <- quantile(df$mean_value, probs = 0.20, na.rm = TRUE)
  threshold_fixed_30 <- quantile(df$mean_value, probs = 0.30, na.rm = TRUE)
  
  # Calculate DOY-based thresholds
  df_dailyPrc <- df %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    summarise(
      threshold_DOY_2 = quantile(mean_value, probs = 0.02, na.rm = TRUE),
      threshold_DOY_5 = quantile(mean_value, probs = 0.05, na.rm = TRUE),
      threshold_DOY_10 = quantile(mean_value, probs = 0.10, na.rm = TRUE),
      threshold_DOY_20 = quantile(mean_value, probs = 0.20, na.rm = TRUE),
      threshold_DOY_30 = quantile(mean_value, probs = 0.30, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Combine fixed and DOY thresholds with the original data
  df_combined <- df %>%
    mutate(DOY = yday(Date)) %>%
    left_join(df_dailyPrc, by = "DOY") %>%
    mutate(
      station_name = AllYear_StreamflowData$station_name[[i]],
      threshold_fixed_2 = threshold_fixed_2,
      threshold_fixed_5 = threshold_fixed_5,
      threshold_fixed_10 = threshold_fixed_10,
      threshold_fixed_20 = threshold_fixed_20,
      threshold_fixed_30 = threshold_fixed_30
    )
  
  # Calculate drought deficit for fixed thresholds
  #The line calculates the "deficit" for the 2% fixed threshold, which represents how much 
  #the streamflow falls below that threshold on a given day. If the streamflow is above or 
  #equal to the threshold, the deficit is 0.
  #If it's below, the deficit is the difference between the threshold and the streamflow
  df_combined <- df_combined %>%
    mutate(
      deficit_fixed_2 = ifelse(mean_value < threshold_fixed_2, threshold_fixed_2 - mean_value, 0),
      deficit_fixed_5 = ifelse(mean_value < threshold_fixed_5, threshold_fixed_5 - mean_value, 0),
      deficit_fixed_10 = ifelse(mean_value < threshold_fixed_10, threshold_fixed_10 - mean_value, 0),
      deficit_fixed_20 = ifelse(mean_value < threshold_fixed_20, threshold_fixed_20 - mean_value, 0),
      deficit_fixed_30 = ifelse(mean_value < threshold_fixed_30, threshold_fixed_30 - mean_value, 0),
      
      # Calculate drought deficit for DOY thresholds
      deficit_DOY_2 = ifelse(mean_value < threshold_DOY_2, threshold_DOY_2 - mean_value, 0),
      deficit_DOY_5 = ifelse(mean_value < threshold_DOY_5, threshold_DOY_5 - mean_value, 0),
      deficit_DOY_10 = ifelse(mean_value < threshold_DOY_10, threshold_DOY_10 - mean_value, 0),
      deficit_DOY_20 = ifelse(mean_value < threshold_DOY_20, threshold_DOY_20 - mean_value, 0),
      deficit_DOY_30 = ifelse(mean_value < threshold_DOY_30, threshold_DOY_30 - mean_value, 0)
    )
  
  # Sum the total drought deficit for each station
  total_deficit_fixed_2 <- sum(df_combined$deficit_fixed_2, na.rm = TRUE)
  total_deficit_fixed_5 <- sum(df_combined$deficit_fixed_5, na.rm = TRUE)
  total_deficit_fixed_10 <- sum(df_combined$deficit_fixed_10, na.rm = TRUE)
  total_deficit_fixed_20 <- sum(df_combined$deficit_fixed_20, na.rm = TRUE)
  total_deficit_fixed_30 <- sum(df_combined$deficit_fixed_30, na.rm = TRUE)
  
  total_deficit_DOY_2 <- sum(df_combined$deficit_DOY_2, na.rm = TRUE)
  total_deficit_DOY_5 <- sum(df_combined$deficit_DOY_5, na.rm = TRUE)
  total_deficit_DOY_10 <- sum(df_combined$deficit_DOY_10, na.rm = TRUE)
  total_deficit_DOY_20 <- sum(df_combined$deficit_DOY_20, na.rm = TRUE)
  total_deficit_DOY_30 <- sum(df_combined$deficit_DOY_30, na.rm = TRUE)
  
  # Display the total drought deficit for the current station
  cat("\nStation:", AllYear_StreamflowData$station_name[[i]], "\n")
  cat("Total Drought Deficit for Fixed Thresholds:\n")
  cat("2% Threshold:", total_deficit_fixed_2, "\n")
  cat("5% Threshold:", total_deficit_fixed_5, "\n")
  cat("10% Threshold:", total_deficit_fixed_10, "\n")
  cat("20% Threshold:", total_deficit_fixed_20, "\n")
  cat("30% Threshold:", total_deficit_fixed_30, "\n\n")
  
  cat("Total Drought Deficit for DOY-based Thresholds:\n")
  cat("2% Threshold:", total_deficit_DOY_2, "\n")
  cat("5% Threshold:", total_deficit_DOY_5, "\n")
  cat("10% Threshold:", total_deficit_DOY_10, "\n")
  cat("20% Threshold:", total_deficit_DOY_20, "\n")
  cat("30% Threshold:", total_deficit_DOY_30, "\n")
  
  summary_df <- data.frame(
    station_name = AllYear_StreamflowData$station_name[[i]],
    threshold_fixed_2 = total_deficit_fixed_2,
    threshold_fixed_5 = total_deficit_fixed_5,
    threshold_fixed_10 = total_deficit_fixed_10,
    threshold_fixed_20 = total_deficit_fixed_20,
    threshold_fixed_30 = total_deficit_fixed_30,
    threshold_DOY_2 = total_deficit_DOY_2,
    threshold_DOY_5 = total_deficit_DOY_5,
    threshold_DOY_10 = total_deficit_DOY_10,
    threshold_DOY_20 = total_deficit_DOY_20,
    threshold_DOY_30 = total_deficit_DOY_30
  )
  
  threshold_values_df <- bind_rows(threshold_values_df, summary_df)
}

print(threshold_values_df)

threshold_values_df_Fig<- threshold_values_df %>% 
  left_join(AllYear_StreamflowData,by='station_name')




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


thresholds <- 2
types <- "F"

minimum<-min(threshold_values_df_Fig$threshold_fixed_2)
maximum<-max(threshold_values_df_Fig$threshold_fixed_2)

color_palette <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850", "#4575b4", "#313695")  # A strong color palette from red to green, with added blue shades

threshold_values_df_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = threshold_values_df_Fig, 
             aes(x = station_lon, y = station_lat, 
                 color = threshold_fixed_2, 
                 size = threshold_fixed_2),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (cfs)", thresholds, types),
                        limits = c(minimum, maximum), 
                        breaks = seq(minimum, maximum, length.out = 8)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Drought Deficit(cfs)", 
                        limits = c(minimum, maximum)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (cfs)",thresholds,types)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )







ggsave(filename = (file.path(file_Path_Variable_O,"Step9_DroughtDeficit_Threshold2_F_medium.jpg")), 
       plot = threshold_values_df_Map, 
       width = 8, height = 6, dpi = 300)



thresholds <- c(2, 5, 10, 20, 30)
types <- c("F", "V")  
color_palette <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850", "#4575b4", "#313695")

global_min <- min(sapply(thresholds, function(t) {
  sapply(types, function(type) {
    col_name <- ifelse(type == "F", paste0("threshold_fixed_", t), paste0("threshold_DOY_", t))
    min(threshold_values_df[[col_name]], na.rm = TRUE)
  })
}), na.rm = TRUE)

global_max <- max(sapply(thresholds, function(t) {
  sapply(types, function(type) {
    col_name <- ifelse(type == "F", paste0("threshold_fixed_", t), paste0("threshold_DOY_", t))
    max(threshold_values_df[[col_name]], na.rm = TRUE)
  })
}), na.rm = TRUE)

for (Threshold in thresholds) {
  for (Type in types) {
    col_name <- ifelse(Type == "F", paste0("threshold_fixed_", Threshold), paste0("threshold_DOY_", Threshold))
    
    threshold_values_df_Map <- ggplot() +
      geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +
      geom_point(data = threshold_values_df_Fig, 
                 aes(x = station_lon, y = station_lat, 
                     color = .data[[col_name]], 
                     size = .data[[col_name]]), 
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (cfs)", Threshold, Type),
                            limits = c(global_min, global_max), 
                            breaks = seq(global_min, global_max, length.out = 8)) +
      scale_size_continuous(range = c(2, 10), name = "Drought Deficit (cfs)", 
                            limits = c(global_min, global_max)) +
      labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (cfs)", Threshold, Type)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggsave(filename = file.path(file_Path_Variable_O, paste0("Step9_DroughtDeficit_Threshold", Threshold, "_", Type, ".jpg")), 
           plot = threshold_values_df_Map, width = 8, height = 6, dpi = 300)
  }
}






##Combine plot



fixed_plots <- list()
variable_plots <- list()

for (Threshold in thresholds) {
  for (Type in types) {
    col_name <- ifelse(Type == "F", paste0("threshold_fixed_", Threshold), paste0("threshold_DOY_", Threshold))
    
    # Create the map
    threshold_values_df_Map <- ggplot() +
      geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +
      geom_point(data = threshold_values_df_Fig, 
                 aes(x = station_lon, y = station_lat, 
                     color = .data[[col_name]], 
                     size = .data[[col_name]]), 
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (cfs)", Threshold, Type),
                            limits = c(global_min, global_max), 
                            breaks = seq(global_min, global_max, length.out = 8)) +
      scale_size_continuous(range = c(2, 10), name = "Drought Deficit (cfs)", 
                            limits = c(global_min, global_max)) +
      labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (cfs)", Threshold, Type)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggsave(filename = file.path(file_Path_Variable_O, paste0("Step9_DroughtDeficit_Threshold_Medium", Threshold, "_", Type, ".jpg")), 
           plot = threshold_values_df_Map, width = 8, height = 6, dpi = 300)
    
    if (Type == "F") {
      fixed_plots[[paste0("Threshold_", Threshold)]] <- threshold_values_df_Map
    } else {
      variable_plots[[paste0("Threshold_", Threshold)]] <- threshold_values_df_Map
    }
  }
}




combined_fixed_plots <- wrap_plots(fixed_plots) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'none')

combined_variable_plots <- wrap_plots(variable_plots) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'none')

ggsave(filename = file.path(file_Path_Variable_O, "Combined_Fixed_Thresholds_Step8_Medium.jpg"), 
       plot = combined_fixed_plots, width = 20, height = 12, dpi = 300)

ggsave(filename = file.path(file_Path_Variable_O, "Combined_Variable_Thresholds_Step8_Medium.jpg"), 
       plot = combined_variable_plots, width = 20, height = 12, dpi = 300)

