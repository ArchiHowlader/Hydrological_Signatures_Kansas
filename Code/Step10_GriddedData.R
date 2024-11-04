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
library(terra)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"


# Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_I, "mean_annual_rainfall_1904_2022.tfw"))
# Medium_StreamflowData


rainfall_raster <- rast(file.path(file_Path_Variable_I, "mean_annual_rainfall_1904_2022.tif"))
rainfall_df <- as.data.frame(rainfall_raster, xy = TRUE)




rainfall_raster_df <- as.data.frame(rainfall_raster, xy = TRUE)


color_palette <- c("darkred", "red", "orange", "yellow", "lightgreen", "green", "cyan", "blue", "darkblue")


minimum <- min(rainfall_raster_df$mean_annual_rainfall_1904_2022, na.rm = TRUE)
maximum <- max(rainfall_raster_df$mean_annual_rainfall_1904_2022, na.rm = TRUE)


rainfall_raster_map <- ggplot() +
  geom_raster(data = rainfall_raster_df, aes(x = x, y = y, 
                                             fill = mean_annual_rainfall_1904_2022)) +
  
  scale_fill_gradientn(colors = color_palette, name = "Rainfall (mm)",
                       limits = c(minimum, maximum),
                       breaks = seq(minimum, maximum, length.out = 8)) +
  
  labs(title = "Mean Annual Rainfall (1904-2022)",
       subtitle = "Spatial distribution of mean annual rainfall") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),  
    axis.ticks = element_blank()  
  )

print(rainfall_raster_map)




##########Clip it according to our dataframe

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







combined_shp_leaflet <- st_transform(combined_shp_leaflet, crs(rainfall_raster))

rainfall_clipped_raster <- crop(rainfall_raster, combined_shp_leaflet)  
rainfall_clipped_raster <- mask(rainfall_clipped_raster, vect(combined_shp_leaflet))  


rainfall_clipped_df <- as.data.frame(rainfall_clipped_raster, xy = TRUE)





rainfall_clipped_map <- ggplot() +
  geom_raster(data = rainfall_clipped_df, aes(x = x, y = y, 
                                              fill = mean_annual_rainfall_1904_2022)) +
  
  scale_fill_gradientn(colors = color_palette, name = "Rainfall (mm)",
                       limits = c(minimum, maximum), 
                       breaks = seq(minimum, maximum, length.out = 8)) +
  
  labs(title = "Clipped Mean Annual Rainfall (1904-2022)",
       subtitle = "Spatial distribution of mean annual rainfall within the combined boundary") +

  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()  
  )


print(rainfall_clipped_map)



Comparison<- rainfall_raster_map+rainfall_clipped_map

ggsave(filename = file.path(file_Path_Variable_O, "Comparison.jpg"), 
       plot = Comparison, width = 24, height = 12, dpi = 300)












rainfall_raster_map_with_outline <- ggplot() +
  geom_raster(data = rainfall_raster_df, aes(x = x, y = y, 
                                             fill = mean_annual_rainfall_1904_2022)) +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_gradientn(colors = color_palette, name = "Rainfall (mm)",
                       limits = c(minimum, maximum),
                       breaks = seq(minimum, maximum, length.out = 8)) +
  
  labs(title = "Mean Annual Rainfall (1904-2022) with Clipped Boundary",
       subtitle = "Spatial distribution of mean annual rainfall with clipped outline") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),  
    axis.ticks = element_blank()  
  )

print(rainfall_raster_map_with_outline)
ggsave(filename = file.path(file_Path_Variable_O, "Comparison.jpg"), 
       plot = rainfall_raster_map_with_outline, width = 24, height = 12, dpi = 300)









###read all the tiff files

file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"



tiff_folder_path <- file.path(file_Path_Variable_I, "Prism/Avg_annual_rainfall/renamed_annual_avg - Copy/")

tiff_files <- list.files(tiff_folder_path, pattern = "\\.tif$", full.names = TRUE)

rainfall_tibble <- tibble(
  file_name = character(),
  rainfall_data = list()
)

for (tiff_file in tiff_files) {
  rainfall_raster <- rast(tiff_file)
  
  rainfall_df <- as.data.frame(rainfall_raster, xy = TRUE)
  
  rainfall_tibble <- rainfall_tibble %>%
    add_row(
      file_name = basename(tiff_file),  
      rainfall_data = list(rainfall_df)  
    )
}


rainfall_tibble <- rainfall_tibble %>%
  mutate(Year = as.numeric(gsub("\\D", "", file_name)))


a=rainfall_tibble %>% filter(Year=='1980')

Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData.xlsx"))
Medium_StreamflowData


Medium_StreamflowData_nested_yeardata <- Medium_StreamflowData %>%
  group_by(Year) %>%
  nest()


Medium_StreamflowData_nested_yeardata
colnames(Medium_StreamflowData_nested_yeardata)<- c('Year','MediumStreamFlow_YearData_HydrologicalSignatures')


Medium_Streamflow_Rainfall<- rainfall_tibble %>% left_join(Medium_StreamflowData_nested_yeardata,by='Year')

Medium_Streamflow_Rainfall_filtered<- Medium_Streamflow_Rainfall %>%  filter(!map_lgl(MediumStreamFlow_YearData_HydrologicalSignatures, is.null))


Medium_Streamflow_Rainfall_filtered_1980<- Medium_Streamflow_Rainfall_filtered %>%  filter(Year=='1980')
rainfall_raster <- rast(Medium_Streamflow_Rainfall_filtered_1980$rainfall_data[[1]])
crs(rainfall_raster) <- "EPSG:4269"
combined_shp_leaflet <- st_transform(combined_shp_leaflet, crs(rainfall_raster))
rainfall_clipped_raster <- crop(rainfall_raster, combined_shp_leaflet)  
rainfall_clipped_raster <- mask(rainfall_clipped_raster, vect(combined_shp_leaflet))  
rainfall_clipped_df <- as.data.frame(rainfall_clipped_raster, xy = TRUE)

streamflow_data_1980 <- Medium_Streamflow_Rainfall_filtered_1980$MediumStreamFlow_YearData_HydrologicalSignatures[[1]]
color_palette <- c("darkred", "red", "orange", "yellow", "lightgreen", "green", "cyan", "blue", "darkblue")
minimum <- 0
maximum <- 120
max_streamflow <- 1.1
rainfall_clipped_map_1980  <- ggplot() +
  geom_raster(data = rainfall_clipped_df, aes(x = x, y = y, fill = PRISM_avg_annual_rainfall_1980)) +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "black", size = 1) +
  geom_point(data = streamflow_data_1980, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanAnnualQ_MMD, 
                 size = MeanAnnualQ_MMD), 
             alpha = 0.8) +
    scale_fill_gradientn(colors = color_palette, name = "Rainfall (mm)",
                       limits = c(0, 110), 
                       breaks = seq(minimum, maximum, length.out = 8)) +
    scale_color_viridis_c(option = "magma", name = "Mean Annual Streamflow (MMD)",
                        limits = c(0, max_streamflow)) +
  scale_size_continuous(range = c(2, 10), name = "Mean Annual Streamflow (MMD)", 
                        limits = c(0, max_streamflow)) +
    labs(title = "Clipped Mean Annual Rainfall (1980) with Streamflow Stations",
       subtitle = "Spatial distribution of mean annual rainfall and streamflow for 1980") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()  
  )

print(rainfall_clipped_map_1980)



Medium_Streamflow_Rainfall_filtered_2010<- Medium_Streamflow_Rainfall_filtered %>%  filter(Year=='2010')
rainfall_raster <- rast(Medium_Streamflow_Rainfall_filtered_2010$rainfall_data[[1]])
crs(rainfall_raster) <- "EPSG:4269"
combined_shp_leaflet <- st_transform(combined_shp_leaflet, crs(rainfall_raster))
rainfall_clipped_raster <- crop(rainfall_raster, combined_shp_leaflet)  
rainfall_clipped_raster <- mask(rainfall_clipped_raster, vect(combined_shp_leaflet))  
rainfall_clipped_df <- as.data.frame(rainfall_clipped_raster, xy = TRUE)

streamflow_data_2010 <- Medium_Streamflow_Rainfall_filtered_2010$MediumStreamFlow_YearData_HydrologicalSignatures[[1]]
color_palette <- c("darkred", "red", "orange", "yellow", "lightgreen", "green", "cyan", "blue", "darkblue")
minimum <- 0
maximum <- 120
max_streamflow <- 1.1
rainfall_clipped_map_2010 <- ggplot() +
  geom_raster(data = rainfall_clipped_df, aes(x = x, y = y, fill = PRISM_avg_annual_rainfall_2010)) +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "black", size = 1) +
  geom_point(data = streamflow_data_2010, 
             aes(x = station_lon, y = station_lat, 
                 color = MeanAnnualQ_MMD, 
                 size = MeanAnnualQ_MMD), 
             alpha = 0.8) +
  scale_fill_gradientn(colors = color_palette, name = "Rainfall (mm)",
                       limits = c(minimum, maximum), 
                       breaks = seq(minimum, maximum, length.out = 8)) +
  scale_color_viridis_c(option = "magma", name = "Mean Annual Streamflow (MMD)",
                        limits = c(0, max_streamflow)) +
  scale_size_continuous(range = c(2, 10), name = "Mean Annual Streamflow (MMD)", 
                        limits = c(0, max_streamflow)) +
  labs(title = "Clipped Mean Annual Rainfall (2010) with Streamflow Stations",
       subtitle = "Spatial distribution of mean annual rainfall and streamflow for 2010") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",  
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()  
  )

print(rainfall_clipped_map_2010)

RainMap<- rainfall_clipped_map_1980+rainfall_clipped_map_2010
ggsave(filename = file.path(file_Path_Variable_O, "Comparison.jpg"), 
       plot = RainMap, width = 24, height = 12, dpi = 300)
