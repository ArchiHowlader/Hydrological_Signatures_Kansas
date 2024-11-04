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
library(RColorBrewer)



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "AllYear_StreamflowData_MMD.rds") )
AllYear_StreamflowData


MediumTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_MediumSubset_MMD_step3.rds"))
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_LongSubset_MMD_step3.rds"))
LongTerm_StreamflowData

# Filtering for LongTerm_StreamflowData
LongTerm_StreamflowData$streamflow_data <- lapply(LongTerm_StreamflowData$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})

# Repeat similarly for other datasets
AllYear_StreamflowData$streamflow_data <- lapply(AllYear_StreamflowData$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})

MediumTerm_StreamflowData$streamflow_data <- lapply(MediumTerm_StreamflowData$streamflow_data, function(df) {
  df %>% filter(mean_streamflow_mm_per_day >= 0)
})


AllYear_StreamflowData_7DayAvg <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%  
  mutate(Year = year(Date)) %>%
  arrange(Date) %>%
  #mutate(Streamflow_7DayAvg = rollmean(mean_streamflow_mm_per_day, 7, fill = NA, align = "center")) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>% 
  ungroup()



Annual_max_7DayStreamflow <- AllYear_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  ungroup()



MediumTerm_StreamflowData_7DayAvg <- MediumTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%  
  mutate(Year = year(Date)) %>%
  arrange(Date) %>%
  mutate( Date= Date) %>%
  mutate(Streamflow_7DayAvgrm = rollmean(mean_streamflow_mm_per_day, 7, fill = NA, align = "center")) %>%
  #mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 7, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>%
  ungroup()

# #bug?
# 
# MediumTerm_StreamflowData_7DayAvg %>% 
#   select(site_no,Date,Streamflow_7DayAvg) %>% 
#   arrange(site_no)
# 
# min(MediumTerm_StreamflowData_7DayAvg$Streamflow_7DayAvg,na.rm=TRUE)
# MediumTerm_StreamflowData_7DayAvg %>%  filter(Streamflow_7DayAvg==min(MediumTerm_StreamflowData_7DayAvg$Streamflow_7DayAvg,na.rm=TRUE))
# ###find all the negative num
# 
# Neg<- MediumTerm_StreamflowData_7DayAvg %>% 
#   select(site_no,Date,Streamflow_7DayAvg) %>% 
#   arrange(site_no) %>% 
#   filter(Streamflow_7DayAvg<0)
# 
# unique(Neg$site_no)
# CD<- MediumTerm_StreamflowData %>%  filter(site_no=='06823500')
# CD_DF<- data.frame(CD$streamflow_data)
# min(CD_DF$mean_streamflow_mm_per_day)
# Check_CD_DF<- CD %>%
#   unnest(cols = streamflow_data) %>%
#   group_by(site_no) %>%  
#   mutate(Year = year(Date)) %>%
#   arrange(Date) %>%
#   mutate( Date= Date) %>%
#   mutate(Streamflow_7DayAvg = rollmean(mean_streamflow_mm_per_day, 7, fill = NA, align = "center")) %>%
#   ungroup()
# 
# min(Check_CD_DF$Streamflow_7DayAvg,na.rm=TRUE)


#########################bug>>??


# MediumTerm_StreamflowData_7DayAvg <- MediumTerm_StreamflowData %>%
#   unnest(cols = streamflow_data) %>%
#   group_by(site_no) %>%  
#   mutate(Year = year(Date)) %>%
#   arrange(Date) %>%
#   mutate( Date= Date) %>%
#   #mutate(Streamflow_7DayAvg = rollmean(mean_streamflow_mm_per_day, 7, fill = NA, align = "center")) %>%
#   mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
#                                          width = 7, 
#                                          FUN = mean, 
#                                          fill = NA, 
#                                          align = "center", 
#                                          partial = TRUE)) %>% 
#   ungroup()
# 


##

MediumTerm_max_7DayStreamflow <- MediumTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  ungroup()



LongTerm_StreamflowData_7DayAvg <- LongTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%  
  mutate(Year = year(Date)) %>%
  arrange(Date) %>%
  #mutate(Streamflow_7DayAvg = rollmean(mean_streamflow_mm_per_day, 7, fill = NA, align = "center")) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>% 
  ungroup()



LongTerm_max_7DayStreamflow <- LongTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  ungroup()




#########Combine the data with annual data, and subset the medium and long term, save files 

#########################Seasonal for Long term, medium term and all term 


assign_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# Step 1: Calculate 7-day rolling mean and assign seasons
LongTerm_StreamflowData_7DayAvg <- LongTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  mutate(Year = year(Date),
         Season = sapply(Date, assign_season)) %>%  # Assign seasons
  arrange(Date) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>% 
  ungroup()


LongTerm_Max_7DayStreamflow_Seasonal <- LongTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year, Season) %>%
  summarise(Max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  spread(Season, Max_7DayStreamflow, fill = NA) %>%  # Spread data by season
  rename(Max_7DayStreamflow_Winter = Winter, 
         Max_7DayStreamflow_Spring = Spring, 
         Max_7DayStreamflow_Summer = Summer, 
         Max_7DayStreamflow_Fall = Fall) %>%
  ungroup()



#medium


assign_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# Step 1: Calculate 7-day rolling mean and assign seasons
MediumTerm_StreamflowData_7DayAvg <- MediumTerm_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  mutate(Year = year(Date),
         Season = sapply(Date, assign_season)) %>%  # Assign seasons
  arrange(Date) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>% 
  ungroup()


MediumTerm_Max_7DayStreamflow_Seasonal <- MediumTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year, Season) %>%
  summarise(Max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  spread(Season, Max_7DayStreamflow, fill = NA) %>%  # Spread data by season
  rename(Max_7DayStreamflow_Winter = Winter, 
         Max_7DayStreamflow_Spring = Spring, 
         Max_7DayStreamflow_Summer = Summer, 
         Max_7DayStreamflow_Fall = Fall) %>%
  ungroup()






assign_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# Step 1: Calculate 7-day rolling mean and assign seasons
AllYear_StreamflowData_7DayAvg <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%
  mutate(Year = year(Date),
         Season = sapply(Date, assign_season)) %>%  # Assign seasons
  arrange(Date) %>%
  mutate(Streamflow_7DayAvg = rollapplyr(mean_streamflow_mm_per_day, 
                                         width = 7, 
                                         FUN = mean, 
                                         fill = NA, 
                                         align = "center", 
                                         partial = TRUE)) %>% 
  ungroup()


AllYear_Max_7DayStreamflow_Seasonal <- AllYear_StreamflowData_7DayAvg %>%
  group_by(site_no, Year, Season) %>%
  summarise(Max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  spread(Season, Max_7DayStreamflow, fill = NA) %>%  # Spread data by season
  rename(Max_7DayStreamflow_Winter = Winter, 
         Max_7DayStreamflow_Spring = Spring, 
         Max_7DayStreamflow_Summer = Summer, 
         Max_7DayStreamflow_Fall = Fall) %>%
  ungroup()




AllYear_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "AllYear_StreamflowData_Annual_Min7_Max7_Min7Seasonal_step7.rds"))
MediumTerm_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "MediumTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_step7.rds"))
LongTerm_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_step7.rds"))



AllYear_StreamflowData_Annual_Max7_Seasonal<- AllYear_StreamflowData_Annual %>% 
  left_join(AllYear_Max_7DayStreamflow_Seasonal, by=c('site_no','Year'))
AllYear_StreamflowData_Annual_Max7_Seasonal




MediumTerm_StreamflowData_Annual_Max7_Seasonal<- MediumTerm_StreamflowData_Annual %>% 
  left_join(MediumTerm_Max_7DayStreamflow_Seasonal, by=c('site_no','Year'))
MediumTerm_StreamflowData_Annual_Max7_Seasonal



LongTerm_StreamflowData_Annual_Max7_Seasonal<- LongTerm_StreamflowData_Annual %>% 
  left_join(LongTerm_Max_7DayStreamflow_Seasonal, by=c('site_no','Year'))
LongTerm_StreamflowData_Annual_Max7_Seasonal


length(unique(LongTerm_StreamflowData_Annual_Max7_Seasonal$site_no))
length(unique(MediumTerm_StreamflowData_Annual_Max7_Seasonal$site_no))


# 
saveRDS(AllYear_StreamflowData_Annual_Max7_Seasonal,file.path(file_Path_Variable_O, "AllYear_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_step7.rds"))
saveRDS(MediumTerm_StreamflowData_Annual_Max7_Seasonal,file.path(file_Path_Variable_O, "MediumTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_step7.rds"))
saveRDS(LongTerm_StreamflowData_Annual_Max7_Seasonal,file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_step7.rds"))

# 























#########make the figure to check data
Annual_max_7DayStreamflow_WithDate_MediumTerm <- MediumTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  filter(Streamflow_7DayAvg == max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE), 
            max_7DayStreamflow_Date = Date) %>%
  ungroup()

Annual_max_7DayStreamflow_WithDate_MediumTerm <- Annual_max_7DayStreamflow_WithDate_MediumTerm %>%
  left_join(Annual_max_7DayStreamflow, by = c("site_no", "Year", "max_7DayStreamflow"))




# MediumTerm_max_7DayStreamflow_fig <- MediumTerm_StreamflowData_7DayAvg %>%
#   group_by(site_no, Year) %>%
#   summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
#   ungroup()


# station1<- yearly_avg_streamflow_LongTerm %>% filter(site_no=="06821500")
# station2<- yearly_avg_streamflow_LongTerm %>% filter(site_no=="06892350")




# MediumTerm_max_7DayStreamflow

station1<- MediumTerm_StreamflowData %>% filter(site_no=="06821500")
StationData<- data.frame(station1$streamflow_data)
StationDataRollingMean<- MediumTerm_StreamflowData_7DayAvg %>% filter(site_no=="06821500")
StationDatamax7<-Annual_max_7DayStreamflow_WithDate_MediumTerm %>% filter(site_no=="06821500")
station_name<-"06821500"
p<- ggplot() +
  geom_line(data = StationData, aes(x = Date, y = mean_streamflow_mm_per_day, color = "Mean Streamflow"), size = 1) +  
  geom_line(data = StationDataRollingMean, aes(x = Date, y = Streamflow_7DayAvg, color = "7-day Moving Average"), size = 1, linetype = "dashed") +  
  geom_point(data = StationDatamax7, aes(x = max_7DayStreamflow_Date, y = max_7DayStreamflow, color = "7-day maximum"), size = 2) +  
  labs(title =paste ("Streamflow (Medium Term), 7-day Moving Average, and 7-day maximum \n",station_name),
       x = "Date",
       y = "Streamflow (MMD)") + 
  scale_color_manual(values = c("Mean Streamflow" = "blue", 
                                "7-day Moving Average" = "green", 
                                "7-day maximum" = "red")) + 
  scale_x_date(limits = as.Date(c("1980-01-01", "1990-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  ylim(0,2)+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
p





#########make the figure to check data
Annual_max_7DayStreamflow_WithDate_LongTerm <- LongTerm_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  filter(Streamflow_7DayAvg == max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE), 
            max_7DayStreamflow_Date = Date) %>%
  ungroup()

Annual_max_7DayStreamflow_WithDate_LongTerm <- Annual_max_7DayStreamflow_WithDate_LongTerm %>%
  left_join(Annual_max_7DayStreamflow, by = c("site_no", "Year", "max_7DayStreamflow"))



# MediumTerm_max_7DayStreamflow_fig <- MediumTerm_StreamflowData_7DayAvg %>%
#   group_by(site_no, Year) %>%
#   summarise(max_7DayStreamflow = max(Streamflow_7DayAvg, na.rm = TRUE)) %>%
#   ungroup()


# station1<- yearly_avg_streamflow_LongTerm %>% filter(site_no=="06821500")
# station2<- yearly_avg_streamflow_LongTerm %>% filter(site_no=="06892350")




# MediumTerm_max_7DayStreamflow

station1<- LongTerm_StreamflowData %>% filter(site_no=="06892350")
StationData<- data.frame(station1$streamflow_data)
StationDataRollingMean<- LongTerm_StreamflowData_7DayAvg %>% filter(site_no=="06892350")
StationDatamax7<-Annual_max_7DayStreamflow_WithDate_LongTerm %>% filter(site_no=="06892350")
station_name<-"06892350"
q<- ggplot() +
  geom_line(data = StationData, aes(x = Date, y = mean_streamflow_mm_per_day, color = "Mean Streamflow"), size = 1) +  
  geom_line(data = StationDataRollingMean, aes(x = Date, y = Streamflow_7DayAvg, color = "7-day Moving Average"), size = 1, linetype = "dashed") +  
  geom_point(data = StationDatamax7, aes(x = max_7DayStreamflow_Date, y = max_7DayStreamflow, color = "7-day maximum"), size = 2) +  
  labs(title =paste ("Streamflow (Long Term), 7-day Moving Average, and 7-day maximum \n",station_name),
       x = "Date",
       y = "Streamflow (MMD)") + 
  scale_color_manual(values = c("Mean Streamflow" = "blue", 
                                "7-day Moving Average" = "green", 
                                "7-day maximum" = "red")) + 
  scale_x_date(limits = as.Date(c("1980-01-01", "1990-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  ylim(0,2)+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
q








###make the figure 3
#check data

CD<- AllYear_StreamflowData %>%  filter(site_no=='06823500')
CD_DF<- data.frame(CD$streamflow_data_CFS_MMD[1])
max(CD_DF$mean_streamflow, scientific = FALSE)
max(CD_DF$mean_streamflow_mm_per_day, scientific = FALSE)
MediumTerm_max_7DayStreamflow %>%  filter(site_no=='06823500')


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





overall_max7_streamflow_MediumTerm<- MediumTerm_max_7DayStreamflow %>% 
  group_by(site_no) %>% 
  summarise(OverallAverage_max_7DayStreamflow = (mean(max_7DayStreamflow, na.rm = TRUE))) %>%
  ungroup()

overall_max7_streamflow_MediumTerm
#overall_max7_streamflow_MediumTerm<- overall_max7_streamflow_MediumTerm %>%  left_join(MediumTerm_StreamflowData,by='site_no')


# site_with_max_streamflow <- MediumTerm_max_7DayStreamflow %>%
#   filter(max_7DayStreamflow == max(max_7DayStreamflow, na.rm = TRUE))
# site_with_max_streamflow







mincf<- min(overall_max7_streamflow_MediumTerm$OverallAverage_max_7DayStreamflow)
maxcf<- max(overall_max7_streamflow_MediumTerm$OverallAverage_max_7DayStreamflow)
overall_max7_streamflow_MediumTerm<- overall_max7_streamflow_MediumTerm %>%  left_join(MediumTerm_StreamflowData,by='site_no')

dd<- overall_max7_streamflow_MediumTerm %>% filter(OverallAverage_max_7DayStreamflow==maxcf)


overall_max7_streamflow_MediumTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  
  geom_point(data = overall_max7_streamflow_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = OverallAverage_max_7DayStreamflow, 
                 size = OverallAverage_max_7DayStreamflow),  
             alpha = 0.8) +
  scale_color_viridis_c(option = "cividis", name = "max Streamflow (MMD)", 
                        limits = c(mincf, maxcf)) +  
  scale_size_continuous(range = c(2, 10), name = "max Streamflow (MMD)", 
                        limits = c(mincf, maxcf)) +  
  labs(title = "Streamflow Data Locations MediumTerm (All) \nmaxQ7day (MMD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()   
  )

# ggsave(filename = (file.path(file_Path_Variable_O,"Step7aC_MMD_max.jpg")),
#        plot = overall_max7_streamflow_MediumTerm_Map,
#        width = 8, height = 6, dpi = 300)
# 




overall_max7_streamflow_LongTerm<- LongTerm_max_7DayStreamflow %>% 
  group_by(site_no) %>% 
  summarise(Overall_max_7DayStreamflow = (mean(max_7DayStreamflow, na.rm = TRUE))) %>%
  ungroup()

overall_max7_streamflow_LongTerm
overall_max7_streamflow_LongTerm<- overall_max7_streamflow_LongTerm %>%  left_join(LongTerm_StreamflowData,by='site_no')
overall_max7_streamflow_LongTerm_Map <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  
  geom_point(data = overall_max7_streamflow_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = Overall_max_7DayStreamflow, 
                 size = Overall_max_7DayStreamflow),  
             alpha = 0.8) +
  scale_color_viridis_c(option = "cividis", name = "max Streamflow (MMD)", 
                        limits = c(mincf, maxcf)) +  
  scale_size_continuous(range = c(2, 10), name = "max Streamflow (MMD)", 
                        limits = c(mincf, maxcf)) +  
  labs(title = "Streamflow Data Locations LongTerm (All) \nmaxQ7day (MMD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()   
  )

# 
# ggsave(filename = (file.path(file_Path_Variable_O,"Step7aC_MMD_max.jpg")),
#        plot = overall_max7_streamflow_LongTerm_Map,
#        width = 8, height = 6, dpi = 300)
# 



library(Kendall)


#calculate annual trend 
MediumTerm_max_7DayStreamflow

calculate_kendall_tau <- function(df) {
  df %>%
    group_by(site_no) %>%
    summarise(
      Tau = Kendall(Year, max_7DayStreamflow)$tau,  # Extract tau value
      p_value = Kendall(Year, max_7DayStreamflow)$sl      # Extract p-value
    ) %>%
    ungroup()
}


tau_MediumTerm <- calculate_kendall_tau(MediumTerm_max_7DayStreamflow)
tau_LongTerm <- calculate_kendall_tau(LongTerm_max_7DayStreamflow)






tau_MediumTerm<- tau_MediumTerm %>% left_join(MediumTerm_StreamflowData,by='site_no')
tau_MediumTerm<- tau_MediumTerm %>% select(site_no,Tau,p_value,station_name,station_lat,station_lon,streamflowUnit)



tau_LongTerm<- tau_LongTerm %>% left_join(LongTerm_StreamflowData,by='site_no')
tau_LongTerm<- tau_LongTerm %>% select(site_no,Tau,p_value,station_name,station_lat,station_lon,streamflowUnit)



tau_LongTerm <- tau_LongTerm %>%
  mutate(significant = ifelse(p_value < 0.05, "Significant", "NotSignificant"))



maxT_Medium <- max(tau_MediumTerm$Tau, na.rm = TRUE)
minT_Medium <- min(tau_MediumTerm$Tau, na.rm = TRUE)
maxT_Long <- max(tau_LongTerm$Tau, na.rm = TRUE)
minT_Long <- min(tau_LongTerm$Tau, na.rm = TRUE)
maxT_Medium
minT_Medium
maxT_Long
minT_Long
tau_MediumTerm %>% filter(Tau==1)

kendall_tau_map_LongTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  # Adjust color and size as needed
  geom_point(data = tau_LongTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = Tau, 
                 fill = significant, 
                 shape = significant,  
                 size = abs(Tau)), 
             alpha = 0.8, stroke = 3) + 
  scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minT_Long, maxT_Medium)) +  # Set the color scale limits to minT and maxT
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") + 
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # No legend for fill
  labs(title = "Streamflow Data Locations (Long term) \nKendall Tau Values and Significance \nAnnual max7trend") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis labels
    axis.ticks = element_blank()   # Remove axis ticks
  )

# ggsave(filename =(file.path(file_Path_Variable_O,"Step7_max7dayq_exmplsta1.jpg")),
#        plot = kendall_tau_map_LongTerm,
#        width = 8, height = 6, dpi = 300)



tau_MediumTerm <- tau_MediumTerm %>%
  mutate(significant = ifelse(p_value < 0.05, "Significant", "NotSignificant"))

kendall_tau_map_MediumTerm <- ggplot() +
  geom_sf(data = combined_shp_leaflet, fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = RiverNetwork_shp, color = "black", size = 0.7) +  
  geom_point(data = tau_MediumTerm, 
             aes(x = station_lon, y = station_lat, 
                 color = Tau,  
                 fill = significant,
                 shape = significant,  
                 size = abs(Tau)),  
             alpha = 0.8, stroke = 3) +  
  scale_color_viridis_c(option = "viridis", name = "Tau Value", limits = c(minT_Long, maxT_Medium)) +  
  scale_size_continuous(range = c(2, 10), name = "Tau Magnitude", limits = c(0, 1)) +  
  scale_shape_manual(values = c("Significant" = 21, "NotSignificant" = 1), name = "Significance") +  
  scale_fill_manual(values = c("Significant" = "black", "NotSignificant" = "white"), guide = "none") +  # No legend for fill
  labs(title = "Streamflow Data Locations (Medium term) \nKendall Tau Values and Significance \nAnnual max7trend") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    axis.ticks = element_blank()   
  )


# ggsave(filename =(file.path(file_Path_Variable_O,"Step7_max7dayq_exmplsta1.jpg")),
#        plot = kendall_tau_map_MediumTerm,
#        width = 8, height = 6, dpi = 300)
# 




CombinedPlot<- (overall_max7_streamflow_MediumTerm_Map+kendall_tau_map_MediumTerm+p)/(overall_max7_streamflow_LongTerm_Map+kendall_tau_map_LongTerm+q)
#CombinedPlot<- (overall_max7_streamflow_MediumTerm_Map+kendall_tau_map_MediumTerm+p)/(overall_max7_streamflow_LongTerm_Map+kendall_tau_map_LongTerm+q)

ggsave(filename =(file.path(file_Path_Variable_O, "Step7b_kendall_CombinedPlot_max.jpg")),
       plot = CombinedPlot,
       width = 20, height = 12, dpi = 300)


