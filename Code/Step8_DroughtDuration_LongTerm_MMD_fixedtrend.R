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

calculate_kendall_tau <- function(df, metric_col) {
  # Remove rows where either Year or the metric is NA
  df_clean <- df %>%
    filter(!is.na(Year), !is.na(!!sym(metric_col)))  # Use !!sym(metric_col) to dynamically access the column
  
  # Ensure both columns are non-empty and have the same length
  if (nrow(df_clean) > 1) {
    # Apply Kendall's tau on the Year and the specified metric
    tau_result <- Kendall(df_clean$Year, df_clean[[metric_col]])
    
    # Return the tau and p-value
    return(list(tau = tau_result$tau, p_value = tau_result$sl))
  } else {
    # Return NA if not enough data to calculate Kendall's tau
    return(list(tau = NA, p_value = NA))
  }
}




combined_DataF <- data.frame()
average_annual_Drought_Duration_DF<- data.frame()
TrendDF<- data.frame()
for (i in 1:nrow(AllYear_StreamflowData)) {
  streamflow_data <- data.frame(AllYear_StreamflowData$streamflow_data[[i]])
  streamflow_data$Date <- lubridate::as_date(streamflow_data$Date)  
  
  streamflow_data <- streamflow_data %>%
    rename(value = mean_streamflow_mm_per_day)
  df <- streamflow_data
  
  moving_average_number=7
  df$mean_value <- round(zoo::rollmean(df$value, k = moving_average_number, align = "center", na.pad = TRUE), digits = 4)
  
  df_withPrc <- 
    df %>%
    subset(is.finite(mean_value)) %>%
    mutate(drought_fixed_2 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.02)), TRUE, FALSE),
           drought_fixed_5 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.05)), TRUE, FALSE),
           drought_fixed_10 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.1)), TRUE, FALSE),
           drought_fixed_20 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.2)), TRUE, FALSE),
           drought_fixed_30 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.3)), TRUE, FALSE))
  
  
  
  df_dailyPrc <-
    df %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    mutate(drought_DOY_2 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.02)), TRUE, FALSE),
           drought_DOY_5 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.05)), TRUE, FALSE),
           drought_DOY_10 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.1)), TRUE, FALSE),
           drought_DOY_20 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.2)), TRUE, FALSE),
           drought_DOY_30 = if_else(mean_value <= quantile(mean_value,na.rm = T,probs = c(0.3)), TRUE, FALSE))%>%
    ungroup()
  
  
  
  
  
  df_bothPrc <-
    df_withPrc %>%
    mutate(DOY = yday(Date)) %>%
    left_join(df_dailyPrc, by = "Date")
  
  
  
  
  df_dailyCount <-
    df_bothPrc %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    mutate(Total_day_2_F = sum(drought_fixed_2),
           Total_day_5_F = sum(drought_fixed_5),
           Total_day_10_F = sum(drought_fixed_10),
           Total_day_20_F = sum(drought_fixed_20),
           Total_day_30_F = sum(drought_fixed_30),
           Total_day_2_V = sum(drought_DOY_2),
           Total_day_5_V = sum(drought_DOY_5),
           Total_day_10_V = sum(drought_DOY_10),
           Total_day_20_V = sum(drought_DOY_20),
           Total_day_30_V = sum(drought_DOY_30),
    )
  
  
  df_unique_years <- df_dailyCount %>%
    distinct(Year, .keep_all = TRUE)
  
  
  DataF<-df_unique_years %>% select(Year,Total_day_2_F, Total_day_5_F , Total_day_10_F ,
                                    Total_day_20_F , Total_day_30_F , Total_day_2_V , Total_day_5_V ,
                                    Total_day_10_V , Total_day_20_V , Total_day_30_V )
  DataF
  
  DataF$Unit<- 'cfs'
  DataF$station_name<- AllYear_StreamflowData$station_name[[i]]
  DataF$site<- AllYear_StreamflowData$site_no[[i]]
  
  
  df_trend <- DataF %>%
    summarise(
      Tau_Total_day_2_F = calculate_kendall_tau(DataF, "Total_day_2_F")$tau,
      p_value_Total_day_2_F = calculate_kendall_tau(DataF, "Total_day_2_F")$p_value,
      
      Tau_Total_day_5_F = calculate_kendall_tau(DataF, "Total_day_5_F")$tau,
      p_value_Total_day_5_F = calculate_kendall_tau(DataF, "Total_day_5_F")$p_value,
      
      Tau_Total_day_10_F = calculate_kendall_tau(DataF, "Total_day_10_F")$tau,
      p_value_Total_day_10_F = calculate_kendall_tau(DataF, "Total_day_10_F")$p_value,
      
      Tau_Total_day_20_F = calculate_kendall_tau(DataF, "Total_day_20_F")$tau,
      p_value_Total_day_20_F = calculate_kendall_tau(DataF, "Total_day_20_F")$p_value,
      
      Tau_Total_day_30_F = calculate_kendall_tau(DataF, "Total_day_30_F")$tau,
      p_value_Total_day_30_F = calculate_kendall_tau(DataF, "Total_day_30_F")$p_value,
      
      Tau_Total_day_2_V = calculate_kendall_tau(DataF, "Total_day_2_V")$tau,
      p_value_Total_day_2_V = calculate_kendall_tau(DataF, "Total_day_2_V")$p_value,
      
      Tau_Total_day_5_V = calculate_kendall_tau(DataF, "Total_day_5_V")$tau,
      p_value_Total_day_5_V = calculate_kendall_tau(DataF, "Total_day_5_V")$p_value,
      
      Tau_Total_day_10_V = calculate_kendall_tau(DataF, "Total_day_10_V")$tau,
      p_value_Total_day_10_V = calculate_kendall_tau(DataF, "Total_day_10_V")$p_value,
      
      Tau_Total_day_20_V = calculate_kendall_tau(DataF, "Total_day_20_V")$tau,
      p_value_Total_day_20_V = calculate_kendall_tau(DataF, "Total_day_20_V")$p_value,
      
      Tau_Total_day_30_V = calculate_kendall_tau(DataF, "Total_day_30_V")$tau,
      p_value_Total_day_30_V = calculate_kendall_tau(DataF, "Total_day_30_V")$p_value
    )
  
  
  df_trend$Unit<- 'mmd'
  df_trend$station_name<- AllYear_StreamflowData$station_name[[i]]
  df_trend$site<- AllYear_StreamflowData$site_no[[i]]
  
  
  DataOA <- DataF %>%
    ungroup() %>%  # Remove any grouping
    select(-Year)  # Remove the Year column
  
  average_drought_duration <- DataOA %>%
    summarise(
      Avg_Total_day_2_F = mean(Total_day_2_F, na.rm = TRUE),
      Avg_Total_day_5_F = mean(Total_day_5_F, na.rm = TRUE),
      Avg_Total_day_10_F = mean(Total_day_10_F, na.rm = TRUE),
      Avg_Total_day_20_F = mean(Total_day_20_F, na.rm = TRUE),
      Avg_Total_day_30_F = mean(Total_day_30_F, na.rm = TRUE),
      Avg_Total_day_2_V = mean(Total_day_2_V, na.rm = TRUE),
      Avg_Total_day_5_V = mean(Total_day_5_V, na.rm = TRUE),
      Avg_Total_day_10_V = mean(Total_day_10_V, na.rm = TRUE),
      Avg_Total_day_20_V = mean(Total_day_20_V, na.rm = TRUE),
      Avg_Total_day_30_V = mean(Total_day_30_V, na.rm = TRUE)
    )
  
  average_drought_duration
  average_drought_duration$Unit<- 'cfs'
  average_drought_duration$station_name<- AllYear_StreamflowData$station_name[[i]]
  average_drought_duration$site<- AllYear_StreamflowData$site_no[[i]]
  
  
  TrendDF <- rbind(TrendDF, df_trend)
  combined_DataF <- rbind(combined_DataF, DataF)
  average_annual_Drought_Duration_DF<- rbind(average_annual_Drought_Duration_DF,average_drought_duration)
  
  p <- ggplot(data = DataF, aes(x = Year)) +
    geom_line(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
    geom_point(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
    
    geom_line(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
    geom_point(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
    
    geom_line(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
    geom_point(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
    
    geom_line(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
    geom_point(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
    
    labs(y = "Total Days", x = "Year", color = "Legend") +
    theme_minimal() +
    ggtitle("Fixed")
  
  # Plot 2 (Variable)
  k <- ggplot(data = DataF, aes(x = Year)) +
    geom_line(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
    geom_point(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
    
    geom_line(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
    geom_point(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
    
    geom_line(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
    geom_point(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
    
    geom_line(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
    geom_point(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
    
    labs(y = "Total Days", x = "Year", color = "Legend") +
    theme_minimal() +
    ggtitle("Variable")
  
  combined_plot <- p + k + plot_layout(guides = 'collect') & theme(legend.position = "top")
  
  # ggsave(filename = paste0(file_Path_Variable_O, "/Drought duration each station/DroughtDuration_", 
  #                          AllYear_StreamflowData$site_no[[i]], ".jpg"),
  #        plot = combined_plot, 
  #        width = 16, height = 8, dpi = 300) 
  
  
}

combined_DataF
average_annual_Drought_Duration_DF
DroughtDurationAnnual<- combined_DataF

AnnualTrendDroughtDuration<- TrendDF %>%  select(station_name,everything())
DroughtDurationAnnual<- DroughtDurationAnnual %>% select(station_name,everything())
average_annual_Drought_Duration_DF<- average_annual_Drought_Duration_DF %>% select(station_name,everything())


# saveRDS(DroughtDurationAnnual, (file.path(file_Path_Variable_O,"DroughtDurationAnnual_Step8_MediumTerm.rds")))
# saveRDS(average_annual_Drought_Duration_DF, (file.path(file_Path_Variable_O,"average_annual_Drought_Duration_DF_Step8_MediumTerm..rds")))
# saveRDS(AnnualTrendDroughtDuration, (file.path(file_Path_Variable_O,"AnnualTrendDroughtDuration_Step8_MediumTerm..rds")))



###Figure
average_annual_Drought_Duration_DF_Fig<- average_annual_Drought_Duration_DF %>% 
  left_join(AllYear_StreamflowData,by='station_name')

AnnualTrendDroughtDuration_Fig<-AnnualTrendDroughtDuration %>% 
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


# Define thresholds and types
thresholds <- c(2, 5, 10, 20, 30)
types <- c("F", "V")  # 'F' for fixed, 'V' for variable
# Calculate global max and min for Fixed (F) and Variable (V) types


# Calculate global max and min for Fixed (F) and Variable (V) types for average drought duration
maxcf_F <- max(sapply(thresholds, function(t) {
  max(average_annual_Drought_Duration_DF_Fig %>%
        subset(is.finite(get(paste0("Avg_Total_day_", t, "_F")))) %>%
        pull(get(paste0("Avg_Total_day_", t, "_F"))), na.rm = TRUE)
}))

mincf_F <- min(sapply(thresholds, function(t) {
  min(average_annual_Drought_Duration_DF_Fig %>%
        subset(is.finite(get(paste0("Avg_Total_day_", t, "_F")))) %>%
        pull(get(paste0("Avg_Total_day_", t, "_F"))), na.rm = TRUE)
}))

maxcf_V <- max(sapply(thresholds, function(t) {
  max(average_annual_Drought_Duration_DF_Fig %>%
        subset(is.finite(get(paste0("Avg_Total_day_", t, "_V")))) %>%
        pull(get(paste0("Avg_Total_day_", t, "_V"))), na.rm = TRUE)
}))

mincf_V <- min(sapply(thresholds, function(t) {
  min(average_annual_Drought_Duration_DF_Fig %>%
        subset(is.finite(get(paste0("Avg_Total_day_", t, "_V")))) %>%
        pull(get(paste0("Avg_Total_day_", t, "_V"))), na.rm = TRUE)
}))

# Calculate global max and min for Fixed (F) and Variable (V) types for trend data
maxcf_trend_F <- max(sapply(thresholds, function(t) {
  max(AnnualTrendDroughtDuration_Fig %>%
        subset(is.finite(get(paste0("Tau_Total_day_", t, "_F")))) %>%
        pull(get(paste0("Tau_Total_day_", t, "_F"))), na.rm = TRUE)
}))

mincf_trend_F <- min(sapply(thresholds, function(t) {
  min(AnnualTrendDroughtDuration_Fig %>%
        subset(is.finite(get(paste0("Tau_Total_day_", t, "_F")))) %>%
        pull(get(paste0("Tau_Total_day_", t, "_F"))), na.rm = TRUE)
}))

maxcf_trend_V <- max(sapply(thresholds, function(t) {
  max(AnnualTrendDroughtDuration_Fig %>%
        subset(is.finite(get(paste0("Tau_Total_day_", t, "_V")))) %>%
        pull(get(paste0("Tau_Total_day_", t, "_V"))), na.rm = TRUE)
}))

mincf_trend_V <- min(sapply(thresholds, function(t) {
  min(AnnualTrendDroughtDuration_Fig %>%
        subset(is.finite(get(paste0("Tau_Total_day_", t, "_V")))) %>%
        pull(get(paste0("Tau_Total_day_", t, "_V"))), na.rm = TRUE)
}))





# Loop over the thresholds and types
for (Threshold in thresholds) {
  for (Type in types) {
    
    # Dynamically create column names based on Threshold and Type
    avg_col_name <- paste0("Avg_Total_day_", Threshold, "_", Type)
    trend_col_name <- paste0("Tau_Total_day_", Threshold, "_", Type)
    
    # Set the overall min and max based on Type (F or V) for both average and trend data
    if (Type == "F") {
      maxcf <- maxcf_F
      mincf <- mincf_F
      maxcf_trend <- maxcf_trend_F
      mincf_trend <- mincf_trend_F
    } else {
      maxcf <- maxcf_V
      mincf <- mincf_V
      maxcf_trend <- maxcf_trend_V
      mincf_trend <- mincf_trend_V
    }
    # Create the average annual drought duration map
    # Define a custom color palette with 6 strong colors
    color_palette <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850", "#4575b4", "#313695")  # A strong color palette from red to green, with added blue shades
    
    # Create the average annual drought duration map with the custom color ramp
    average_annual_Drought_Duration_DF_Fig_Map <- ggplot() +
      geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
      geom_point(data = average_annual_Drought_Duration_DF_Fig, 
                 aes(x = station_lon, y = station_lat, 
                     color = !!sym(avg_col_name), 
                     size = !!sym(avg_col_name)),  # Map both color and size to streamflow
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Avg Drought Duration", Threshold, Type),
                            limits = c(mincf, maxcf), 
                            breaks = seq(mincf, maxcf, length.out = 8)) +  # Divide the scale into 6 equal breaks
      scale_size_continuous(range = c(2, 10), name = paste("Avg Drought Duration", Threshold, Type), 
                            limits = c(mincf, maxcf)) +  # Set the size scale limits dynamically
      labs(title = paste("Average Annual Drought Duration - Threshold", Threshold, Type)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),   # Remove axis labels
        axis.ticks = element_blank()   # Remove axis ticks
      )
    
    # Create the trend map with the custom color ramp
    AnnualTrendDroughtDuration_Fig_Map <- ggplot() +
      geom_sf(data = combined_shp_leaflet, fill = "yellow", color = "blue", size = 0.5) +
      geom_sf(data = RiverNetwork_shp, color = "red", size = 0.7) +  # Adjust color and size as needed
      geom_point(data = na.omit(AnnualTrendDroughtDuration_Fig), 
                 aes(x = station_lon, y = station_lat, 
                     color = !!sym(trend_col_name), 
                     size = !!sym(trend_col_name)),  # Map both color and size to streamflow
                 alpha = 0.8) +
      scale_color_gradientn(colors = color_palette, name = paste("Trend Drought Duration", Threshold, Type),
                            limits = c(mincf_trend, maxcf_trend), 
                            breaks = seq(mincf_trend, maxcf_trend, length.out = 8)) +  # Divide the trend scale into 6 equal breaks
      scale_size_continuous(range = c(2, 10), name = paste("Trend Drought Duration", Threshold, Type), 
                            limits = c(mincf_trend, maxcf_trend)) +  # Set the size scale limits dynamically
      labs(title = paste("Trend in Drought Duration - Threshold", Threshold, Type)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "right",
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),   # Remove axis labels
        axis.ticks = element_blank()   # Remove axis ticks
      )
    
    # Create plots for "Arikaree River nr Haigler, Nebr."
    DataF <- DroughtDurationAnnual %>% filter(station_name == 'Arikaree River nr Haigler, Nebr.')
    plot_Arikaree_Fix <- ggplot(data = DataF, aes(x = Year)) +
      geom_line(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
      geom_point(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
      geom_line(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
      geom_point(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
      geom_line(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
      geom_point(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
      geom_line(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
      geom_point(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
      labs(y = "Total Days", x = "Year", color = "Legend") +
      theme_minimal() +
      ggtitle(paste("Fixed", DataF$station_name))
    
    plot_Arikaree_Var <- ggplot(data = DataF, aes(x = Year)) +
      geom_line(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
      geom_point(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
      geom_line(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
      geom_point(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
      geom_line(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
      geom_point(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
      geom_line(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
      geom_point(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
      labs(y = "Total Days", x = "Year", color = "Legend") +
      theme_minimal() +
      ggtitle(paste("Variable", DataF$station_name))
    
    combined_plot_Arikaree <- plot_Arikaree_Fix + plot_Arikaree_Var + plot_layout(guides = 'collect') & theme(legend.position = "top")
    
    # Create plots for "WAKARUSA R NR LAWRENCE, KS"
    Data2 <- DroughtDurationAnnual %>% filter(station_name == 'WAKARUSA R NR LAWRENCE, KS')
    plot_Wakarusa_Fix <- ggplot(data = Data2, aes(x = Year)) +
      geom_line(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
      geom_point(aes(y = Total_day_2_F, color = "Total_day_2_F")) +
      geom_line(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
      geom_point(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
      geom_line(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
      geom_point(aes(y = Total_day_20_F, color = "Total_day_20_F")) +
      geom_line(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
      geom_point(aes(y = Total_day_30_F, color = "Total_day_30_F")) +
      labs(y = "Total Days", x = "Year", color = "Legend") +
      theme_minimal() +
      ggtitle(paste("Fixed", Data2$station_name)) +
      theme(legend.position = "none")
    
    plot_Wakarusa_Var <- ggplot(data = Data2, aes(x = Year)) +
      geom_line(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
      geom_point(aes(y = Total_day_2_V, color = "Total_day_2_V")) +
      geom_line(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
      geom_point(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
      geom_line(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
      geom_point(aes(y = Total_day_20_V, color = "Total_day_20_V")) +
      geom_line(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
      geom_point(aes(y = Total_day_30_V, color = "Total_day_30_V")) +
      labs(y = "Total Days", x = "Year", color = "Legend") +
      theme_minimal() +
      ggtitle(paste("Variable", Data2$station_name)) +
      theme(legend.position = "none")
    
    combined_plot_Wakarusa <- plot_Wakarusa_Fix + plot_Wakarusa_Var + plot_layout(guides = 'collect') & theme(legend.position = "none")
    
    # Combine everything into the final plot
    FinalPlot <- (average_annual_Drought_Duration_DF_Fig_Map + AnnualTrendDroughtDuration_Fig_Map) /
      (combined_plot_Arikaree + combined_plot_Wakarusa)
    
    # Save the final plot
    # ggsave(filename = paste0(file_Path_Variable_O, "/Step8output/MediumTerm/Step8_FinalPlot_", Threshold, "_", Type, ".jpg"),
    #        plot = FinalPlot,
    #        width = 20, height = 10, dpi = 300)
  }
}


ggsave(filename =(file.path(file_Path_Variable_O,"Step8a.jpg")),
       plot = FinalPlot,
       width = 20, height = 12, dpi = 300)






##################Fig for 10 F and V

# Create the four plots for 10 Fixed (F) and 10 Variable (V)
Threshold <- 10
Types <- c("F", "V")

for (Type in Types) {
  avg_col_name <- paste0("Avg_Total_day_", Threshold, "_", Type)
  trend_col_name <- paste0("Tau_Total_day_", Threshold, "_", Type)
  
  if (Type == "F") {
    maxcf <- maxcf_F
    mincf <- mincf_F
    maxcf_trend <- maxcf_trend_F
    mincf_trend <- mincf_trend_F
  } else {
    maxcf <- maxcf_V
    mincf <- mincf_V
    maxcf_trend <- maxcf_trend_V
    mincf_trend <- mincf_trend_V
  }
  
  # Average drought duration map
  avg_map <- ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
    geom_point(data = average_annual_Drought_Duration_DF_Fig, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(avg_col_name), 
                   size = !!sym(avg_col_name)),
               alpha = 0.8) +
    scale_color_gradientn(colors = color_palette, name = paste("Avg Drought Duration", Threshold, Type),
                          limits = c(0, 365), 
                          breaks = seq(0, 365, length.out = 8)) +
    scale_size_continuous(range = c(2, 10), name = paste("Avg Drought Duration", Threshold, Type), 
                          limits = c(0, 365)) +
    labs(title = paste("Average Annual Drought Duration \n- Threshold", Threshold, Type)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "right",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  # Add a significance column to the dataset
  AnnualTrendDroughtDuration_Fig <- AnnualTrendDroughtDuration_Fig %>%
    mutate(significant = ifelse(get(paste0("p_value_Total_day_", Threshold, "_", Type)) <= 0.05, "Significant", "NotSignificant"))
  
  # Update trend map to include Tau and significance information
  trend_mapWithP <- ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
    geom_point(data = na.omit(AnnualTrendDroughtDuration_Fig), 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(trend_col_name),     # Color for Tau
                   shape = significant,              # Shape for significance
                   fill = significant,               # Fill based on significance
                   size = abs(!!sym(trend_col_name))),# Size based on Tau magnitude
               alpha = 0.8, stroke = 3) +
    scale_color_viridis_c(option = "viridis", name = paste("Tau Value", Threshold, Type),
                          limits = c(mincf_trend, maxcf_trend)) +
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") + 
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  
    labs(title = paste("Trend in Drought Duration \n- Threshold", Threshold, Type)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  # Trend drought duration map
  trend_map <- ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +
    geom_point(data = na.omit(AnnualTrendDroughtDuration_Fig), 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(trend_col_name), 
                   size = !!sym(trend_col_name)),
               alpha = 0.8) +
    scale_color_gradientn(colors = color_palette, name = paste("Trend Drought Duration", Threshold, Type),
                          limits = c(mincf_trend, maxcf_trend), 
                          breaks = seq(mincf_trend, maxcf_trend, length.out = 8)) +
    scale_size_continuous(range = c(2, 10), name = paste("Trend Drought Duration", Threshold, Type), 
                          limits = c(mincf_trend, maxcf_trend)) +
    labs(title = paste("Trend in Drought Duration \n- Threshold", Threshold, Type)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "right",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  # Store the plots as variables based on type
  if (Type == "F") {
    average_annual_Drought_Duration_DF_Fig_Map_F <- avg_map
    AnnualTrendDroughtDuration_Fig_Map_F <- trend_mapWithP
  } else {
    average_annual_Drought_Duration_DF_Fig_Map_V <- avg_map
    AnnualTrendDroughtDuration_Fig_Map_V <- trend_mapWithP
  }
}


station_nameb<-AllYear_StreamflowData %>% filter(site_no=="06892350")
station_name2<-station_nameb $station_name

# station_name<-"06821500"
# station_name<-"06892350"

station_nameU<-AllYear_StreamflowData %>% filter(site_no=="06821500")
station_name1<-station_nameU $station_name
DataF <- DroughtDurationAnnual %>% filter(station_name == station_name1)

plot_Arikaree_Var <- ggplot(data = DataF, aes(x = Year)) +
  geom_line(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
  geom_point(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
  geom_line(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
  geom_point(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
  labs(y = "Total Days", x = "Year", color = "Legend") +
  theme_minimal() +
  ylim(0,365)+
  ggtitle(paste(DataF$station_name))
plot_Arikaree_Var

station_nameb<-AllYear_StreamflowData %>% filter(site_no=="06892350")
station_name2<-station_nameb $station_name

# Create plots for "WAKARUSA R NR LAWRENCE, KS"
Data2 <- DroughtDurationAnnual %>% filter(station_name == station_name2)
plot_Wakarusa_Fix <- ggplot(data = Data2, aes(x = Year)) +
  geom_line(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
  geom_point(aes(y = Total_day_10_F, color = "Total_day_10_F")) +
  geom_line(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
  geom_point(aes(y = Total_day_10_V, color = "Total_day_10_V")) +
  labs(y = "Total Days", x = "Year", color = "Legend") +
  theme_minimal() +
  ylim(0,365)+
  ggtitle(paste(Data2$station_name)) 

plot_Wakarusa_Fix
# Combine the four plots using patchwork
combined_figure <- (average_annual_Drought_Duration_DF_Fig_Map_F | AnnualTrendDroughtDuration_Fig_Map_F|plot_Arikaree_Var ) /
  (average_annual_Drought_Duration_DF_Fig_Map_V | AnnualTrendDroughtDuration_Fig_Map_V|plot_Wakarusa_Fix)


ggsave(filename = (file.path(file_Path_Variable_O,"Step8_LongTerm.jpg")), 
       plot = combined_figure, 
       width =20, height = 10, dpi = 300)






