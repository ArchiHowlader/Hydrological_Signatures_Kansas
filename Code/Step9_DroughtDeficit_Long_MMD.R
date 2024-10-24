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
AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_MMD_step3.rds"))
AllYear_StreamflowData

AllYear_StreamflowData$streamflow_data <- lapply(AllYear_StreamflowData$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})


# #convert it cfs to MMD
# 
# # Function to convert cfs to MMD
# convert_cfs_to_MMD <- function(streamflow_tibble) {
#   # Multiply the mean_streamflow values by 86400 (seconds in a day)
#   streamflow_tibble %>%
#     mutate(mean_streamflow = mean_streamflow * 86400)
# }
# 
# AllYear_StreamflowData <- AllYear_StreamflowData %>%
#   mutate(streamflow_data = map(streamflow_data, convert_cfs_to_MMD))
# 
# print(AllYear_StreamflowData)
# AllYear_StreamflowData$streamflow_data[1]

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
  
  streamflow_data <- data.frame(AllYear_StreamflowData$streamflow_data[[i]])
  streamflow_data$Date <- lubridate::as_date(streamflow_data$Date)  
  
  streamflow_data <- streamflow_data %>%
    rename(value = mean_streamflow_mm_per_day)
  
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
  
  # Sum the total drought deficit for each station  ##MMD convert 
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
is.data.frame(threshold_values_df)














stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
threshold_values_df_Fig<- threshold_values_df %>% 
  left_join(stationLatLon,by='station_name')










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
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = threshold_values_df_Fig, 
             aes(x = station_lon, y = station_lat, 
                 color = threshold_fixed_2, 
                 size = threshold_fixed_2),  # Map both color and size to streamflow
             alpha = 0.8) +
  scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (MMD)", thresholds, types),
                        limits = c(minimum, maximum), 
                        breaks = seq(minimum, maximum, length.out = 8)) +  # Set the color scale limits from 0 to 10,000
  scale_size_continuous(range = c(2, 10), name = "Drought Deficit(MMD)", 
                        limits = c(minimum, maximum)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (MMD)",thresholds,types)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )







ggsave(filename = (file.path(file_Path_Variable_O,"Step9_DroughtDeficit_Threshold2_F_Long.jpg")), 
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
      geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
      geom_point(data = threshold_values_df_Fig, 
                 aes(x = station_lon, y = station_lat, 
                     color = .data[[col_name]], 
                     size = .data[[col_name]]), 
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (MMD)", Threshold, Type),
                            limits = c(global_min, global_max), 
                            breaks = seq(global_min, global_max, length.out = 8)) +
      scale_size_continuous(range = c(2, 10), name = "Drought Deficit (MMD)", 
                            limits = c(global_min, global_max)) +
      labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (MMD)", Threshold, Type)) +
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
      geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
      geom_point(data = threshold_values_df_Fig, 
                 aes(x = station_lon, y = station_lat, 
                     color = .data[[col_name]], 
                     size = .data[[col_name]]), 
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Drought Deficit (MMD)", Threshold, Type),
                            limits = c(global_min, global_max), 
                            breaks = seq(global_min, global_max, length.out = 8)) +
      scale_size_continuous(range = c(2, 10), name = "Drought Deficit (MMD)", 
                            limits = c(global_min, global_max)) +
      labs(title = paste("Streamflow Data Locations (All) \nDrought Deficit (MMD)", Threshold, Type)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    ggsave(filename = file.path(file_Path_Variable_O, paste0("Step9_DroughtDeficit_Threshold_Long_MMD", Threshold, "_", Type, ".jpg")), 
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

ggsave(filename = file.path(file_Path_Variable_O, "Combined_Fixed_Thresholds_Step8_Long_MMD.jpg"), 
       plot = combined_fixed_plots, width = 20, height = 12, dpi = 300)

ggsave(filename = file.path(file_Path_Variable_O, "Combined_Variable_Thresholds_Step8_Long_MMD.jpg"), 
       plot = combined_variable_plots, width = 20, height = 12, dpi = 300)
















########annual sum deficit 

threshold_values_df_annual <- data.frame()

for (i in 1:nrow(AllYear_StreamflowData)) {
  
  streamflow_data <- data.frame(AllYear_StreamflowData$streamflow_data[[i]])
  streamflow_data$Date <- lubridate::as_date(streamflow_data$Date)  
  
  streamflow_data <- streamflow_data %>%
    rename(value = mean_streamflow_mm_per_day) %>%
    mutate(Year = year(Date))  # Add a 'Year' column
  
  df <- streamflow_data
  
  moving_average_number = 7
  df$mean_value <- round(zoo::rollmean(df$value, k = moving_average_number, align = "center", na.pad = TRUE), digits = 4)
  
  threshold_fixed_2 <- quantile(df$mean_value, probs = 0.02, na.rm = TRUE)
  threshold_fixed_5 <- quantile(df$mean_value, probs = 0.05, na.rm = TRUE)
  threshold_fixed_10 <- quantile(df$mean_value, probs = 0.10, na.rm = TRUE)
  threshold_fixed_20 <- quantile(df$mean_value, probs = 0.20, na.rm = TRUE)
  threshold_fixed_30 <- quantile(df$mean_value, probs = 0.30, na.rm = TRUE)
  
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
  
  df_combined <- df_combined %>%
    group_by(Year) %>%  # Group by year
    mutate(
      deficit_fixed_2 = ifelse(mean_value < threshold_fixed_2, threshold_fixed_2 - mean_value, 0),
      deficit_fixed_5 = ifelse(mean_value < threshold_fixed_5, threshold_fixed_5 - mean_value, 0),
      deficit_fixed_10 = ifelse(mean_value < threshold_fixed_10, threshold_fixed_10 - mean_value, 0),
      deficit_fixed_20 = ifelse(mean_value < threshold_fixed_20, threshold_fixed_20 - mean_value, 0),
      deficit_fixed_30 = ifelse(mean_value < threshold_fixed_30, threshold_fixed_30 - mean_value, 0),
      
      deficit_DOY_2 = ifelse(mean_value < threshold_DOY_2, threshold_DOY_2 - mean_value, 0),
      deficit_DOY_5 = ifelse(mean_value < threshold_DOY_5, threshold_DOY_5 - mean_value, 0),
      deficit_DOY_10 = ifelse(mean_value < threshold_DOY_10, threshold_DOY_10 - mean_value, 0),
      deficit_DOY_20 = ifelse(mean_value < threshold_DOY_20, threshold_DOY_20 - mean_value, 0),
      deficit_DOY_30 = ifelse(mean_value < threshold_DOY_30, threshold_DOY_30 - mean_value, 0)
    ) %>%
    summarise(
      total_deficit_fixed_2 = sum(deficit_fixed_2, na.rm = TRUE),
      total_deficit_fixed_5 = sum(deficit_fixed_5, na.rm = TRUE),
      total_deficit_fixed_10 = sum(deficit_fixed_10, na.rm = TRUE),
      total_deficit_fixed_20 = sum(deficit_fixed_20, na.rm = TRUE),
      total_deficit_fixed_30 = sum(deficit_fixed_30, na.rm = TRUE),
      
      total_deficit_DOY_2 = sum(deficit_DOY_2, na.rm = TRUE),
      total_deficit_DOY_5 = sum(deficit_DOY_5, na.rm = TRUE),
      total_deficit_DOY_10 = sum(deficit_DOY_10, na.rm = TRUE),
      total_deficit_DOY_20 = sum(deficit_DOY_20, na.rm = TRUE),
      total_deficit_DOY_30 = sum(deficit_DOY_30, na.rm = TRUE)
    ) %>%
    ungroup()
  
  summary_df <- df_combined %>%
    mutate(station_name = AllYear_StreamflowData$station_name[[i]])
  
  threshold_values_df_annual <- bind_rows(threshold_values_df_annual, summary_df)
}

print(threshold_values_df_annual)

threshold_values_df_annual<- threshold_values_df_annual %>% select(station_name,everything())



colnames(threshold_values_df_annual) <- as.character(colnames(threshold_values_df_annual))

threshold_values_df_annual_rds <- threshold_values_df_annual %>%
  rename(
    DroughtDeficit_fixed2_MMD = total_deficit_fixed_2,
    DroughtDeficit_fixed5_MMD = total_deficit_fixed_5,
    DroughtDeficit_fixed10_MMD = total_deficit_fixed_10,
    DroughtDeficit_fixed20_MMD = total_deficit_fixed_20,
    DroughtDeficit_fixed30_MMD = total_deficit_fixed_30,
    DroughtDeficit_variable2_MMD = total_deficit_DOY_2,
    DroughtDeficit_variable5_MMD = total_deficit_DOY_5,
    DroughtDeficit_variable10_MMD = total_deficit_DOY_10,
    DroughtDeficit_variable20_MMD = total_deficit_DOY_20,
    DroughtDeficit_variable30_MMD = total_deficit_DOY_30
  )


stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
threshold_values_df_annual_rds<- threshold_values_df_annual_rds %>% 
  left_join(stationLatLon,by='station_name')



# 
LongTerm_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_DroughtDuration_step8.rds"))
# 
# 
LongTerm_StreamflowData_Annual_DroughtDeficitAnnual<- LongTerm_StreamflowData_Annual %>%
  left_join(threshold_values_df_annual_rds, by=c('site_no','Year'))
LongTerm_StreamflowData_Annual_DroughtDeficitAnnual
# 


saveRDS(LongTerm_StreamflowData_Annual_DroughtDeficitAnnual,file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_DroughtDuration_DroughtDeficit_step9.rds"))








########annual sum deficit 
library(Kendall)
#####annual sum deficit trend tau and p 




calculate_kendall_tau <- function(df, metric_col) {
  df_clean <- df %>%
    filter(!is.na(Year), !is.na(!!sym(metric_col)))  
  
  if (nrow(df_clean) > 1) {
    tau_result <- Kendall(df_clean$Year, df_clean[[metric_col]])
    
    return(list(tau = tau_result$tau, p_value = tau_result$sl))
  } else {
    
    return(list(tau = NA, p_value = NA))
  }
}
# Initialize an empty data frame to store results
kendall_tau_results <- data.frame()

# List of columns to calculate Kendall's Tau for
metrics <- c("total_deficit_fixed_2", "total_deficit_fixed_5", "total_deficit_fixed_10",
             "total_deficit_fixed_20", "total_deficit_fixed_30", 
             "total_deficit_DOY_2", "total_deficit_DOY_5", "total_deficit_DOY_10", 
             "total_deficit_DOY_20", "total_deficit_DOY_30")

# Loop through each station
stations <- unique(threshold_values_df_annual$station_name)

for (station in stations) {
  # Filter the data for the current station
  station_data <- threshold_values_df_annual %>% filter(station_name == station)
  
  # Create a list to store Kendall's tau and p-values for this station
  station_tau <- list(station_name = station)
  
  # Loop through each metric and calculate Kendall's tau and p-value
  for (metric in metrics) {
    result <- calculate_kendall_tau(station_data, metric)
    station_tau[[paste0("tau_", metric)]] <- result$tau
    station_tau[[paste0("p_value_", metric)]] <- result$p_value
  }
  
  # Convert the list to a data frame and bind it to the results data frame
  kendall_tau_results <- bind_rows(kendall_tau_results, as.data.frame(station_tau))
}

# Print the results
print(kendall_tau_results)






threshold_values_df_annual %>% filter(station_name=='Arikaree River nr Haigler, Nebr.')







#####annual sum deficit trend tau and p 



######kendall tau figure


stationLatLon <- data.frame(AllYear_StreamflowData$station_name, AllYear_StreamflowData$site_no, AllYear_StreamflowData$station_lat, AllYear_StreamflowData$station_lon)
colnames(stationLatLon) <- c('station_name', 'site_no', 'station_lat', 'station_lon')

tau_data_with_coords <- kendall_tau_results %>%
  left_join(stationLatLon, by = "station_name")

tau_data_with_coords <- tau_data_with_coords %>%
  mutate(significant_10F = ifelse(p_value_total_deficit_fixed_10 <= 0.05, "Significant", "NotSignificant"),
         significant_10V = ifelse(p_value_total_deficit_DOY_10 <= 0.05, "Significant", "NotSignificant"))

color_palette <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850", "#4575b4", "#313695")

min_tau <- min(tau_data_with_coords$tau_total_deficit_fixed_10, tau_data_with_coords$tau_total_deficit_DOY_10, na.rm = TRUE)
max_tau <- max(tau_data_with_coords$tau_total_deficit_fixed_10, tau_data_with_coords$tau_total_deficit_DOY_10, na.rm = TRUE)

tau_10F_map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
  geom_point(data = na.omit(tau_data_with_coords), 
             aes(x = station_lon, y = station_lat, 
                 color = tau_total_deficit_fixed_10,     
                 shape = significant_10F,              
                 fill = significant_10F,               
                 size = abs(tau_total_deficit_fixed_10)),  
             alpha = 0.8, stroke = 3) +
  scale_color_gradientn(colors = color_palette, name = "Tau Value (10F)",
                        limits = c(-1, 1)) +
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") + 
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  
  labs(title = "Trend in Drought Deficit - Threshold 10F") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

tau_10V_map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
  geom_point(data = na.omit(tau_data_with_coords), 
             aes(x = station_lon, y = station_lat, 
                 color = tau_total_deficit_DOY_10,     
                 shape = significant_10V,              
                 fill = significant_10V,               
                 size = abs(tau_total_deficit_DOY_10)),  
             alpha = 0.8, stroke = 3) +
  scale_color_gradientn(colors = color_palette, name = "Tau Value (10V)",
                        limits = c(-1, 1)) +
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") + 
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  
  labs(title = "Trend in Drought Deficit - Threshold 10V") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )













#############make figure seperately 




# Define the specific threshold you want to focus on
Threshold <- 10
Type_F <- "F"
Type_V <- "V"

# Define the color palette and calculate global min/max for better scaling
color_palette <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850", "#4575b4", "#313695")


### Fixed Threshold Plot (10% Fixed) ###
fixed_10_plot <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
  geom_point(data = threshold_values_df_Fig, 
             aes(x = station_lon, y = station_lat, 
                 color = threshold_fixed_10, 
                 size = threshold_fixed_10), 
             alpha = 0.8) +
  scale_color_gradientn(colors = color_palette, name = "Drought Deficit (MMD) 10% F",
                        limits = c(global_min, global_max), 
                        breaks = seq(global_min, global_max, length.out = 8)) +
  scale_size_continuous(range = c(2, 10), name = "Drought Deficit (MMD)",
                        limits = c(global_min, global_max)) +
  labs(title = "Streamflow Data Locations (All)\nDrought Deficit (MMD) 10% Fixed") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



### Variable Threshold Plot (10% Variable) ###
variable_10_plot <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
  geom_point(data = threshold_values_df_Fig, 
             aes(x = station_lon, y = station_lat, 
                 color = threshold_DOY_10, 
                 size = threshold_DOY_10), 
             alpha = 0.8) +
  scale_color_gradientn(colors = color_palette, name = "Drought Deficit (MMD) 10% V",
                        limits = c(global_min, global_max), 
                        breaks = seq(global_min, global_max, length.out = 8)) +
  scale_size_continuous(range = c(2, 10), name = "Drought Deficit (MMD)",
                        limits = c(global_min, global_max)) +
  labs(title = "Streamflow Data Locations (All)\nDrought Deficit (MMD) 10% Variable") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



FV<- fixed_10_plot+variable_10_plot




















station_nameU<-AllYear_StreamflowData %>% filter(site_no=="06821500")
station_name1<-station_nameU $station_name
DataF <- threshold_values_df_annual %>% filter(station_name == station_name1)

plot_Arikaree_Var <- ggplot(data = DataF, aes(x = Year)) +
  geom_line(aes(y = total_deficit_fixed_10, color = "total_deficit_fixed_10")) +
  geom_point(aes(y = total_deficit_fixed_10, color = "total_deficit_fixed_10")) +
  geom_line(aes(y = total_deficit_DOY_10, color = "total_deficit_DOY_10")) +
  geom_point(aes(y = total_deficit_DOY_10, color = "total_deficit_DOY_10")) +
  labs(y = "Drought Deficit", x = "Year", color = "Legend") +
  theme_minimal() +
  ylim(0,3)+
  ggtitle(paste(DataF$station_name))
plot_Arikaree_Var

station_nameb<-AllYear_StreamflowData %>% filter(site_no=="06892350")
station_name2<-station_nameb $station_name

# Create plots for "WAKARUSA R NR LAWRENCE, KS"
Data2 <- threshold_values_df_annual %>% filter(station_name == station_name2)
plot_Wakarusa_Fix <- ggplot(data = Data2, aes(x = Year)) +
  geom_line(aes(y = total_deficit_fixed_10, color = "total_deficit_fixed_10")) +
  geom_point(aes(y = total_deficit_fixed_10, color = "total_deficit_fixed_10")) +
  geom_line(aes(y = total_deficit_DOY_10, color = "total_deficit_DOY_10")) +
  geom_point(aes(y = total_deficit_DOY_10, color = "total_deficit_DOY_10")) +
  labs(y = "Drought Deficit", x = "Year", color = "Legend") +
  theme_minimal() +
  ylim(0,3)+
  #ylim(0,365)+
  ggtitle(paste(Data2$station_name)) 

plot_Wakarusa_Fix

# 
# combined_figure <- (fixed_10_plot | variable_10_plot|plot_Arikaree_Var ) /
#   (tau_10F_map | tau_10V_map|plot_Wakarusa_Fix)

combined_figure <- (fixed_10_plot | tau_10F_map|plot_Arikaree_Var ) /
  (variable_10_plot | tau_10V_map|plot_Wakarusa_Fix)


ggsave(filename = file.path(file_Path_Variable_O, "Step9_MMD_Longterm_FinalFigure.jpg"), 
       plot = combined_figure, width = 20, height = 12, dpi = 300)

