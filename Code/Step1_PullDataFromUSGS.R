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




desoto_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/DeSoto_shp/DeSoto.shp"
watershed_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/WatershedBoundary_KN_20230113/watershed_bndry.shp"

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)

watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))

combined_shp <- st_union(desoto_shp, watershed_shp)

combined_shp_leaflet <- st_as_sf(combined_shp)

combined_shp_map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = combined_shp_leaflet, color = "red", weight = 2, fillOpacity = 0.5) %>%
  addScaleBar(position = "bottomleft")

# Display the map
combined_shp_map

bounding_box <- st_bbox(combined_shp)

xmin <- sprintf("%.7f", bounding_box["xmin"])
ymin <- sprintf("%.7f", bounding_box["ymin"])
xmax <- sprintf("%.7f", bounding_box["xmax"])
ymax <- sprintf("%.7f", bounding_box["ymax"])

stations <- whatNWISsites(
  bBox = c(xmin, ymin, xmax, ymax)
)

stations_sf <- st_as_sf(stations, coords = c("dec_long_va", "dec_lat_va"), crs = st_crs(combined_shp))

stations_within_combined <- st_intersection(stations_sf, combined_shp)

site_numbers <- stations_within_combined$site_no
data_avail_df <- whatNWISdata(siteNumber = site_numbers)

stations_with_streamflow <- data_avail_df %>%
  filter(parm_cd == "00060") %>%
  distinct(site_no)

stations_with_streamflow_sf <- stations_within_combined %>%
  filter(site_no %in% stations_with_streamflow$site_no)

map <- combined_shp_map

for (i in 1:nrow(stations_with_streamflow_sf)) {
  station_name <- stations_with_streamflow_sf$station_nm[i]
  site_no <- stations_with_streamflow_sf$site_no[i]
  
  data <- readNWISdv(siteNumber = site_no, parameterCd = "00060")
  
  if (nrow(data) > 0) {
    data <- data %>%
      select(Date, X_00060_00003) %>%
      rename(mean_streamflow = X_00060_00003)
    
    plot <- ggplot(data, aes(x = Date, y = mean_streamflow)) +
      geom_line(color = "blue") +
      labs(title = paste(station_name, "\n", site_no), x = "Date", y = "Mean Streamflow (cfs)") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text = element_text(),
        axis.title = element_text(),
        axis.ticks = element_line(),
        legend.position = "right",
        legend.title = element_text(),
        legend.text = element_text(),
        legend.background = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(),
        plot.caption = element_text()
      ) +
      theme_bw() +
      theme_classic()
    
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot, width = 8, height = 6, dpi = 150)
    
    img <- base64enc::dataURI(file = temp_file, mime = "image/png")
    
    popup_content <- paste0("<img src='", img, "' width='400' height='300'>")
    
    map <- map %>%
      addMarkers(
        lng = stations_with_streamflow_sf$geometry[i][[1]][1],
        lat = stations_with_streamflow_sf$geometry[i][[1]][2],
        label = station_name,
        popup = popup_content
      )
    
    # Remove the temporary file
    file.remove(temp_file)
  }
}

map



coords <- st_coordinates(stations_with_streamflow_sf)

streamflow_data_list <- map(stations_with_streamflow_sf$site_no, function(site) {
  data <- readNWISdv(siteNumber = site, parameterCd = "00060")
  if (nrow(data) > 0) {
    data <- data %>%
      select(Date, X_00060_00003) %>%
      rename(mean_streamflow = X_00060_00003)
    data <- tibble::tibble(Date = data$Date, mean_streamflow = data$mean_streamflow)
  } else {
    data <- tibble::tibble(Date = as.Date(character()), mean_streamflow = numeric())
  }
  return(data)
})


metrics_list <- map(streamflow_data_list, function(data) {
  if (nrow(data) > 0) {
    start_year <- min(year(data$Date), na.rm = TRUE)
    end_year <- max(year(data$Date), na.rm = TRUE)
    start_day <- min((data$Date), na.rm = TRUE)
    end_day <- max((data$Date), na.rm = TRUE)
    
    num_years <- end_year - start_year + 1
    total_days <- num_years * 365
    missing_days <- sum(is.na(data$mean_streamflow))
    percent_missing <- (missing_days / total_days) * 100
  } else {
    start_year <- NA
    end_year <- NA
    num_years <- 0
    percent_missing <- 100
    start_day<- NA
    end_day<- NA
  }
  tibble(start_year, end_year,start_day,end_day, num_years, percent_missing)
})
metrics_tibble <- bind_rows(metrics_list)

streamflow_tibble <- tibble(
  station_name = stations_with_streamflow_sf$station_nm,
  site_no = stations_with_streamflow_sf$site_no,
  station_lat = coords[, "Y"],
  station_lon = coords[, "X"],
  streamflow_data = streamflow_data_list,
  start_year = metrics_tibble$start_year,
  end_year = metrics_tibble$end_year,
  start_day = metrics_tibble$start_day,
  end_day = metrics_tibble$end_day,
  num_years = metrics_tibble$num_years,
  #percent_missing = round(metrics_tibble$percent_missing, 2)  # Round to 2 decimal places
)

print(streamflow_tibble)


streamflow_tibbles<- streamflow_tibble %>% 
                      filter(map_lgl(streamflow_data, ~nrow(.x)>0))
streamflow_tibbles

#Discharge, cubic feet per second (Mean)

# streamflow_tibble2<- streamflow_tibble %>% 
#   filter(map_lgl(streamflow_data, ~nrow(.x)==0))



streamflow_tibbles<- streamflow_tibbles %>% 
  mutate(streamflowUnit='cfs')

streamflow_tibbles<- streamflow_tibbles %>% select(station_name,site_no,station_lat,station_lon,streamflow_data,streamflowUnit)

saveRDS(streamflow_tibbles,'Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_step1.rds')

# check data from the usgs website 
streamflow_tibbles$site_no[35]
streamflow_tibbles$station_name[35]
Df<- data.frame(streamflow_tibbles$streamflow_data[35])


streamflow_tibbles$site_no[50]
streamflow_tibbles$station_name[50]
Df<- data.frame(streamflow_tibbles$streamflow_data[50])



streamflow_tibbles$site_no[225]
streamflow_tibbles$station_name[225]
Df<- data.frame(streamflow_tibbles$streamflow_data[225])
Df