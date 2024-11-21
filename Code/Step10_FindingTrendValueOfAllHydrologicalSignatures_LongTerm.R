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
library(stringr)
library(Kendall)


file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"

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


Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_LongTerm_StreamflowData_Adjustingseason.xlsx"))
selected_columns <- Medium_StreamflowData %>%
  select(site_no, everything()[6:ncol(Medium_StreamflowData)])


calculate_kendall_tau_multiple <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau_Fall = Kendall(Year, Max_7DayStreamflow_Fall_SON_MMD)$tau,    
      p_value_Fall = Kendall(Year, Max_7DayStreamflow_Fall_SON_MMD)$sl, 
      Tau_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MAM_MMD)$tau, 
      p_value_Spring = Kendall(Year, Max_7DayStreamflow_Spring_MAM_MMD)$sl, 
      Tau_Summer = Kendall(Year, Max_7DayStreamflow_Summer_JJA_MMD)$tau,  
      p_value_Summer = Kendall(Year, Max_7DayStreamflow_Summer_JJA_MMD)$sl, 
      Tau_Winter = Kendall(Year, Max_7DayStreamflow_Winter_DJF_MMD)$tau,  
      p_value_Winter = Kendall(Year, Max_7DayStreamflow_Winter_DJF_MMD)$sl  
    ) %>%
    ungroup()
}

tau_MediumTerm_Seasons <- calculate_kendall_tau_multiple(Medium_StreamflowData)

tau_MediumTerm_Seasons


calculate_kendall_tau_by_site <- function(df, col_name) {
  df %>%
    group_by(site_no) %>%
    summarise(
      tau = Kendall(Year, .data[[col_name]])$tau,
      p_value = Kendall(Year, .data[[col_name]])$sl
    ) %>%
    mutate(column = col_name)
}

tau_results_grouped <- map_dfr(names(selected_columns)[-1], 
                               ~ calculate_kendall_tau_by_site(selected_columns, .))

tau_results_grouped <- tau_results_grouped %>%
  select(site_no, column, tau, p_value) %>%
  arrange(site_no, column)


tau_results_grouped

tau_results_pivoted <- tau_results_grouped %>%
  pivot_wider(
    names_from = column,
    values_from = c(tau, p_value),
    names_glue = "{column}_{.value}"
  )

tau_results_pivoted



tau_results_pivoted$Max_7DayStreamflow_Fall_SON_MMD_tau-tau_MediumTerm_Seasons$Tau_Fall
tau_results_pivoted$Max_7DayStreamflow_Fall_SON_MMD_p_value-tau_MediumTerm_Seasons$p_value_Fall






AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))

stationLatLon<- data.frame(AllYear_StreamflowData$station_name,AllYear_StreamflowData$site_no,AllYear_StreamflowData$station_lat,AllYear_StreamflowData$station_lon)
colnames(stationLatLon)<- c('station_name','site_no','station_lat','station_lon')
tau_results_pivoted<- tau_results_pivoted %>% 
  left_join(stationLatLon,by='site_no')
tau_results_pivoted


create_kendall_tau_map <- function(data, tau_column, p_value_column, title, min_tau, max_tau) {
  ggplot() +
    geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +  # Base map
    geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # River network
    geom_point(data = data, 
               aes(x = station_lon, y = station_lat, 
                   color = !!sym(tau_column),  # Use dynamic column for Tau
                   fill = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  
                   shape = ifelse(!!sym(p_value_column) < 0.05, "Significant", "NotSignificant"),  
                   size = abs(!!sym(tau_column))), 
               alpha = 0.8, stroke = 3) +  
    scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(-1, 1)) +  
    scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
    scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  
    scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Title formatting
      legend.position = "right",  
      axis.title = element_blank(),  
      axis.text = element_blank(),   
      axis.ticks = element_blank()  
    )
}

tau_results_pivoted$MeanAnnualQ_MMD_p_value

MeanAnnualQ_MMD_Kendall_Tau <- create_kendall_tau_map(
  tau_results_pivoted, 
  "MeanAnnualQ_MMD_p_value", 
  "MeanAnnualQ_MMD_p_value", 
  "MeanAnnualQ\nKendall Tau Values and Significance",
  min_tau = -1, max_tau = 1
)

write.csv(tau_results_pivoted,file.path(file_Path_Variable_O, "LongSubset_AnnualTrendAnalysis_HydrologicalSignatures.csv") )


output_path <- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/Output/TrendResults_LongTerm"

create_and_save_kendall_tau_maps <- function(data, output_folder) {
  tau_columns <- grep("_tau$", names(data), value = TRUE)
  p_value_columns <- str_replace(tau_columns, "_tau$", "_p_value")
  
  for (i in seq_along(tau_columns)) {
    tau_col <- tau_columns[i]
    p_value_col <- p_value_columns[i]
    title <- paste("Kendall Tau Values and Significance for", str_remove(tau_col, "_tau"))
    
    map <- create_kendall_tau_map(
      data = data,
      tau_column = tau_col,
      p_value_column = p_value_col,
      title = title,
      min_tau = -1,
      max_tau = 1
    )
    
    file_name <- paste0(output_folder, "/", tau_col, "_Kendall_Tau_Map.png")
    ggsave(file_name, plot = map, width = 10, height = 8, dpi = 300)
  }
}

create_and_save_kendall_tau_maps(tau_results_pivoted, output_path)
