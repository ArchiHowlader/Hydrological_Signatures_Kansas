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
library(writexl)
library(readxl)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"


Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData.xlsx"))
Medium_StreamflowData
calculate_kendall_tau_multiple <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$tau,    
      p_value_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$sl, 
      Tau_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$tau, 
      p_value_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$sl, 
      Tau_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$tau,  
      p_value_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$sl, 
      Tau_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$tau,  
      p_value_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$sl  
    ) %>%
    ungroup()
}

tau_MediumTerm_Seasons <- calculate_kendall_tau_multiple(Medium_StreamflowData)

tau_MediumTerm_Seasons


AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))

stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
tau_MediumTerm_Seasons_wl<- tau_MediumTerm_Seasons %>% 
  left_join(stationLatLon,by='site_no')

tau_MediumTerm_Seasons_wl

desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)







minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)





create_kendall_tau_map <- function(data, tau_column, p_value_column, title, min_tau, max_tau) {
  ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +  # Base map
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # River network
    geom_point(data = data, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(tau_column),  # Use dynamic column for Tau
                   fill = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Significance check
                   shape = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Shape based on significance
                   size = abs(!!sym(tau_column))),  # Size proportional to Tau magnitude
               alpha = 0.8, stroke = 3) +  # Styling
    scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minV, maxV)) +  # Color scale for Tau
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  # Size scale
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  # Shape based on significance
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # Fill style
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Title formatting
      legend.position = "right",  # Legend position
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis labels
      axis.ticks = element_blank()   # Remove axis ticks
    )
}
minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)

kendall_tau_map_Fall_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Fall", 
  "p_value_Fall", 
  "MaxQ7 MediumTerm Fall\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Spring_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Spring", 
  "p_value_Spring", 
  "MaxQ7 (Spring) MediumTerm\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Summer_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Summer", 
  "p_value_Summer", 
  "MaxQ7 MediumTerm (Summer)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Winter_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Winter", 
  "p_value_Winter", 
  "MaxQ7 MediumTerm (Winter)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

MediumTermSeasonalTrend <- kendall_tau_map_Fall_Medium + kendall_tau_map_Spring_Medium + 
  kendall_tau_map_Summer_Medium + kendall_tau_map_Winter_Medium

ggsave(filename = file.path(file_Path_Variable_O, "MediumTermSeasonalTrendMax7_step7abc.jpg"), 
       plot = MediumTermSeasonalTrend, width = 20, height = 14, dpi = 300)





















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
library(writexl)
library(readxl)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"


LongTerm_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_LongTerm_StreamflowData.xlsx"))
LongTerm_StreamflowData
calculate_kendall_tau_multiple <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$tau,    
      p_value_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$sl, 
      Tau_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$tau, 
      p_value_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$sl, 
      Tau_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$tau,  
      p_value_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$sl, 
      Tau_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$tau,  
      p_value_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$sl  
    ) %>%
    ungroup()
}

tau_LongTerm_Seasons <- calculate_kendall_tau_multiple(LongTerm_StreamflowData)

tau_LongTerm_Seasons


AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_MMD_step3.rds"))

stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
tau_LongTerm_Seasons_wl<- tau_LongTerm_Seasons %>% 
  left_join(stationLatLon,by='site_no')

tau_LongTerm_Seasons_wl

desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)







minV <- min(tau_LongTerm_Seasons_wl$Tau_Winter, tau_LongTerm_Seasons_wl$Tau_Spring, 
            tau_LongTerm_Seasons_wl$Tau_Summer, tau_LongTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_LongTerm_Seasons_wl$Tau_Winter, tau_LongTerm_Seasons_wl$Tau_Spring, 
            tau_LongTerm_Seasons_wl$Tau_Summer, tau_LongTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)





create_kendall_tau_map <- function(data, tau_column, p_value_column, title, min_tau, max_tau) {
  ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +  # Base map
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # River network
    geom_point(data = data, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(tau_column),  # Use dynamic column for Tau
                   fill = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Significance check
                   shape = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Shape based on significance
                   size = abs(!!sym(tau_column))),  # Size proportional to Tau magnitude
               alpha = 0.8, stroke = 3) +  # Styling
    scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minV, maxV)) +  # Color scale for Tau
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  # Size scale
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  # Shape based on significance
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # Fill style
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Title formatting
      legend.position = "right",  # Legend position
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis labels
      axis.ticks = element_blank()   # Remove axis ticks
    )
}
minV <- min(tau_LongTerm_Seasons_wl$Tau_Winter, tau_LongTerm_Seasons_wl$Tau_Spring, 
            tau_LongTerm_Seasons_wl$Tau_Summer, tau_LongTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_LongTerm_Seasons_wl$Tau_Winter, tau_LongTerm_Seasons_wl$Tau_Spring, 
            tau_LongTerm_Seasons_wl$Tau_Summer, tau_LongTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)

kendall_tau_map_Fall_Long <- create_kendall_tau_map(
  tau_LongTerm_Seasons_wl, 
  "Tau_Fall", 
  "p_value_Fall", 
  "MaxQ7 LongTerm Fall\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Spring_Long <- create_kendall_tau_map(
  tau_LongTerm_Seasons_wl, 
  "Tau_Spring", 
  "p_value_Spring", 
  "MaxQ7 (Spring) LongTerm\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Summer_Long <- create_kendall_tau_map(
  tau_LongTerm_Seasons_wl, 
  "Tau_Summer", 
  "p_value_Summer", 
  "MaxQ7 LongTerm (Summer)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Winter_Long <- create_kendall_tau_map(
  tau_LongTerm_Seasons_wl, 
  "Tau_Winter", 
  "p_value_Winter", 
  "MaxQ7 LongTerm (Winter)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

LongTermSeasonalTrend <- kendall_tau_map_Fall_Long + kendall_tau_map_Spring_Long + 
  kendall_tau_map_Summer_Long + kendall_tau_map_Winter_Long

ggsave(filename = file.path(file_Path_Variable_O, "LongTermSeasonalTrendMax7_step7abc.jpg"), 
       plot = LongTermSeasonalTrend, width = 20, height = 14, dpi = 300)















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
library(writexl)
library(readxl)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"


Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData.xlsx"))
Medium_StreamflowData
calculate_kendall_tau_multiple <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$tau,    
      p_value_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$sl, 
      Tau_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$tau, 
      p_value_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$sl, 
      Tau_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$tau,  
      p_value_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$sl, 
      Tau_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$tau,  
      p_value_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$sl  
    ) %>%
    ungroup()
}

tau_MediumTerm_Seasons <- calculate_kendall_tau_multiple(Medium_StreamflowData)

tau_MediumTerm_Seasons


AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))

stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
tau_MediumTerm_Seasons_wl<- tau_MediumTerm_Seasons %>% 
  left_join(stationLatLon,by='site_no')

tau_MediumTerm_Seasons_wl

desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)







minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)





create_kendall_tau_map <- function(data, tau_column, p_value_column, title, min_tau, max_tau) {
  ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +  # Base map
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # River network
    geom_point(data = data, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(tau_column),  # Use dynamic column for Tau
                   fill = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Significance check
                   shape = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Shape based on significance
                   size = abs(!!sym(tau_column))),  # Size proportional to Tau magnitude
               alpha = 0.8, stroke = 3) +  # Styling
    scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minV, maxV)) +  # Color scale for Tau
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  # Size scale
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  # Shape based on significance
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # Fill style
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Title formatting
      legend.position = "right",  # Legend position
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis labels
      axis.ticks = element_blank()   # Remove axis ticks
    )
}
minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)

kendall_tau_map_Fall_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Fall", 
  "p_value_Fall", 
  "MaxQ7 MediumTerm Fall\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Spring_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Spring", 
  "p_value_Spring", 
  "MaxQ7 (Spring) MediumTerm\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Summer_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Summer", 
  "p_value_Summer", 
  "MaxQ7 MediumTerm (Summer)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Winter_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Winter", 
  "p_value_Winter", 
  "MaxQ7 MediumTerm (Winter)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

MediumTermSeasonalTrend <- kendall_tau_map_Fall_Medium + kendall_tau_map_Spring_Medium + 
  kendall_tau_map_Summer_Medium + kendall_tau_map_Winter_Medium

ggsave(filename = file.path(file_Path_Variable_O, "MediumTermSeasonalTrendMax7_step7abc.jpg"), 
       plot = MediumTermSeasonalTrend, width = 20, height = 14, dpi = 300)





















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
library(writexl)
library(readxl)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"


MediumTerm_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData.xlsx"))
MediumTerm_StreamflowData
calculate_kendall_tau_multiple <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$tau,    
      p_value_Fall = Kendall(Year, Max_7DayStreamflow_Fall_MMD)$sl, 
      Tau_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$tau, 
      p_value_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MMD)$sl, 
      Tau_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$tau,  
      p_value_Summer = Kendall(Year, Max_7DayStreamflow_Summer_MMD)$sl, 
      Tau_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$tau,  
      p_value_Winter = Kendall(Year, Max_7DayStreamflow_Winter_MMD)$sl  
    ) %>%
    ungroup()
}

tau_MediumTerm_Seasons <- calculate_kendall_tau_multiple(MediumTerm_StreamflowData)

tau_MediumTerm_Seasons


AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))

stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
tau_MediumTerm_Seasons_wl<- tau_MediumTerm_Seasons %>% 
  left_join(stationLatLon,by='site_no')

tau_MediumTerm_Seasons_wl

desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)







minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)





create_kendall_tau_map <- function(data, tau_column, p_value_column, title, min_tau, max_tau) {
  ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +  # Base map
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # River network
    geom_point(data = data, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(tau_column),  # Use dynamic column for Tau
                   fill = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Significance check
                   shape = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  # Shape based on significance
                   size = abs(!!sym(tau_column))),  # Size proportional to Tau magnitude
               alpha = 0.8, stroke = 3) +  # Styling
    scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minV, maxV)) +  # Color scale for Tau
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  # Size scale
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  # Shape based on significance
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # Fill style
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Title formatting
      legend.position = "right",  # Legend position
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis labels
      axis.ticks = element_blank()   # Remove axis ticks
    )
}
minV <- min(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)
maxV <- max(tau_MediumTerm_Seasons_wl$Tau_Winter, tau_MediumTerm_Seasons_wl$Tau_Spring, 
            tau_MediumTerm_Seasons_wl$Tau_Summer, tau_MediumTerm_Seasons_wl$Tau_Fall, na.rm = TRUE)

kendall_tau_map_Fall_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Fall", 
  "p_value_Fall", 
  "MaxQ7 MediumTerm Fall\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Spring_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Spring", 
  "p_value_Spring", 
  "MaxQ7 (Spring) MediumTerm\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Summer_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Summer", 
  "p_value_Summer", 
  "MaxQ7 MediumTerm (Summer)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

kendall_tau_map_Winter_Medium <- create_kendall_tau_map(
  tau_MediumTerm_Seasons_wl, 
  "Tau_Winter", 
  "p_value_Winter", 
  "MaxQ7 MediumTerm (Winter)\nKendall Tau Values and Significance",
  min_tau = minV, max_tau = maxV
)

MediumTermSeasonalTrend <- kendall_tau_map_Fall_Medium + kendall_tau_map_Spring_Medium + 
  kendall_tau_map_Summer_Medium + kendall_tau_map_Winter_Medium

ggsave(filename = file.path(file_Path_Variable_O, "MediumTermSeasonalTrendMax7_step7abc.jpg"), 
       plot = MediumTermSeasonalTrend, width = 20, height = 14, dpi = 300)

