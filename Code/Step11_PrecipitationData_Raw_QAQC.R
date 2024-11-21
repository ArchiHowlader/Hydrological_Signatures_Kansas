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
precipitation_folder <- file.path(file_Path_Variable_I, "final_Stationwise_Precipitation_NOAA_Raw")
MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_step3.rds"))
MediumTerm_StreamflowData


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




csv_files <- list.files(precipitation_folder, pattern = "\\.csv$", full.names = TRUE)
all_dataframes <- lapply(csv_files, read.csv)
names(all_dataframes) <- basename(csv_files)
lapply(all_dataframes, head)
station_names <- gsub("\\.csv$", "", basename(csv_files))

Precipitation_Data <- tibble(
  station_name = station_names,
  Precipitation_data = lapply(all_dataframes, as_tibble)  # Nest the data frames as tibbles
)

Precipitation_Data <- Precipitation_Data %>%
  mutate(
    Lat = sapply(Precipitation_data, function(x) x$Lat[1]),
    Long = sapply(Precipitation_data, function(x) x$Long[1])
  )

names(Precipitation_Data$Precipitation_data)
names(Precipitation_Data$Precipitation_data) <- NULL



# 
# stations_sf <- st_as_sf(
#   Precipitation_Data,
#   coords = c("Long", "Lat"),
#   crs = st_crs(combined_shp_leaflet)  
# )
# stations_within_boundary <- stations_sf[st_intersects(stations_sf, combined_shp_leaflet, sparse = FALSE), ]
# 





###########QAQC

Precipitation_Data <- Precipitation_Data %>%
  mutate(Precipitation_data = map(Precipitation_data, ~ .x %>%
                                    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))))


extract_years <- function(df) {
  df %>%
    mutate(Year = as.numeric(format(Date, "%Y"))) %>%
    pull(Year) %>%
    unique() %>%
    sort()
}

Precipitation_Data <- Precipitation_Data %>%
  mutate(SamplingYears = map(Precipitation_data, extract_years))


##make sequential dates df, combine it with original data, so that missing days would have na 

fill_missing_dates <- function(data) {
  if (nrow(data) > 0 && all(!is.na(data$Date))) {
    full_dates <- tibble(Date = seq(min(data$Date, na.rm = TRUE), max(data$Date, na.rm = TRUE), by = "day"))
    
    full_data <- full_dates %>%
      left_join(data, by = "Date")
    
    return(full_data)
  } else {
    return(tibble(Date = as.Date(character()), mean_streamflow = numeric()))
  }
}

Precipitation_Data2 <- Precipitation_Data %>%
  mutate(full_Precipitation_Data = map(Precipitation_data, fill_missing_dates))


Precipitation_Data<- Precipitation_Data2


#check data

k1= data.frame(Precipitation_Data$Precipitation_data[1])
k2= data.frame(Precipitation_Data$full_Precipitation_Data[1])
a=plot(k1$Date,k1$precip_mm)


calculate_metrics <- function(data) {
  if (nrow(data) > 0) {
    start_year <- min(year(data$Date), na.rm = TRUE)
    end_year <- max(year(data$Date), na.rm = TRUE)
    start_day <- min(data$Date, na.rm = TRUE)
    end_day <- max(data$Date, na.rm = TRUE)
    
    num_years <- n_distinct(year(data$Date), na.rm = TRUE)
    total_days <- as.numeric(difftime(end_day, start_day, units = "days")) + 1
    missing_days <- sum(is.na(data$precip_mm)) # all dates are counted as we are taking the full_precipitation_data 
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

metrics <- Precipitation_Data %>%
  mutate(metrics = map(full_Precipitation_Data, calculate_metrics)) %>%
  unnest(metrics)

print(metrics)
Precipitation_Data<- metrics

##check data
k1= data.frame(Precipitation_Data$Precipitation_data[1])
k2= data.frame(Precipitation_Data$full_Precipitation_Data[1])
missing_daysk2 <- sum(is.na(k2$precip_mm)) 



filtered_tibble <- Precipitation_Data %>%
  filter(percent_missing < 10)


# filtered_tibble <- filtered_tibble %>%
#   mutate(
#     Precipitation_Data = map(Precipitation_Data, ~ {
#       if (inherits(.x, "data.frame")) {
#         .x
#       } else {
#         tibble(Date = as.Date(character()), precip_mm = numeric())
#       }
#     })
#   )
# 


#####make sure each year have 95% data 




filter_years_by_data_availability <- function(Precipitation_Data) {
  Precipitation_Data %>%
    mutate(year = year(Date)) %>%
    group_by(year) %>%
    summarise(
      days_in_year = n(),
      total_days_in_year = ifelse(leap_year(year), 366, 365),
      percent_data = (days_in_year / total_days_in_year) * 100
    ) %>%
    filter(percent_data >= 95) %>%
    select(year)
}

filtered_tibbleWP <- filtered_tibble %>%
  mutate(
    Precipitation_Data = map(Precipitation_data, ~ {
      if (inherits(.x, "data.frame")) {
        available_years <- filter_years_by_data_availability(.x)
        .x %>%
          mutate(year = year(Date)) %>%
          filter(year %in% available_years$year) %>%
          select(-year)
      } else {
        tibble()  # Return an empty tibble for invalid cases
      }
    })
  )

filtered_tibbleWP

filtered_tibbleWP_DataOmit<- filtered_tibbleWP %>% filter(map_lgl(Precipitation_Data,~!is.null(.x)&&nrow(.x)>0))
filtered_tibble<-filtered_tibbleWP_DataOmit
Precipitation_Data<- filtered_tibble
# saveRDS(filtered_tibble, file = file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_step2.rds"))



###########QAQC


##stations in the boundary

stations_sf <- st_as_sf(
  Precipitation_Data,
  coords = c("Long", "Lat"),
  crs = st_crs(combined_shp_leaflet)  
)
stations_within_boundary <- stations_sf[st_intersects(stations_sf, combined_shp_leaflet, sparse = FALSE), ]









Precipitation_Data_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = Precipitation_Data, 
             aes(x = Long, y = Lat, 
                 color = 'red', 
                 size = 2),  # Map both color and size to streamflow
             alpha = 0.8) +
  # scale_color_viridis_c(option = "plasma", name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the color scale limits from 0 to 10,000
  # scale_size_continuous(range = c(2, 10), name = "Precipitation", 
  #                       limits = c(0, maxcf)) +  # Set the size scale limits from 0 to 10,000  # Titles and labels
  labs(title = "Precipitation Data Locations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )
ggsave(filename = (file.path(file_Path_Variable_O,"Step11_Precipitation.jpg")), 
       plot = Precipitation_Data_Map, 
       width = 8, height = 6, dpi = 300)


# 
# stations_sf <- st_as_sf(
#   filtered_tibble,
#   coords = c("Long", "Lat"),
#   crs = st_crs(combined_shp_leaflet)  
# )
# stations_within_boundary <- stations_sf[st_intersects(stations_sf, combined_shp_leaflet, sparse = FALSE), ]
# 

stations_sf <- Precipitation_Data %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(combined_shp_leaflet))

stations_within_boundary <- st_intersection(stations_sf, combined_shp_leaflet)

stations_within_boundary_tibble <- stations_within_boundary %>%
  as_tibble() 

print(stations_within_boundary_tibble)
stations_within_boundary_tibble <- stations_within_boundary_tibble %>%
  mutate(
    Long = st_coordinates(geometry)[, 1],
    Lat = st_coordinates(geometry)[, 2]
  )
Precipitation_Data_WB<- stations_within_boundary_tibble %>%  select(station_name,Precipitation_data,Long,Lat)


#saveRDS(stations_within_boundary_tibble, file = file.path(file_Path_Variable_O, "PrecipitationData_RawQAQC_tibbles_Filtered_step11.rds"))
