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




file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
AllYear_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "streamflow_tibbles_Filtered_step2.rds"))
AllYear_StreamflowData


finalData<- data.frame()
for (i in nrow(streamflow_data)){
#06827500
streamflow_data<- AllYear_StreamflowData$streamflow_data[[i]]
streamflow_data<- data.frame(streamflow_data$streamflow_data)
streamflow_data$Date <- lubridate::as_date(streamflow_data$Date)  

streamflow_data <- streamflow_data %>%
  rename(value = mean_streamflow)
df <- streamflow_data


moving_average_number=7
df$mean_value <- round(zoo::rollmean(df$value, k = moving_average_number, align = "center", na.pad = TRUE), digits = 4)






df_withPrc <- 
  df %>%
  subset(is.finite(mean_value)) %>%
  arrange(mean_value) %>%
  mutate(rank = seq(1, length(mean_value)),
         percentile = 100*rank/(max(rank)+1)) #plus one is because so that it is not hundred 

df_dailyPrc <-
  df %>%
  mutate(DOY = yday(Date)) %>%
  group_by(DOY) %>%
  mutate(DOYpercentile = 100 * min_rank(mean_value) / (length(mean_value) + 1)) %>%
  ungroup()


df_bothPrc <-
  df_withPrc %>%
  dplyr::select(-rank) %>%
  mutate(DOY = yday(Date)) %>%
  left_join(df_dailyPrc[,c("Date", "DOYpercentile")], by = "Date")




df_bothPrc <- df_bothPrc %>%
  mutate(drought_fixed_2 = if_else(percentile < 2, TRUE, FALSE),
         drought_fixed_5 = if_else(percentile < 5, TRUE, FALSE),
         drought_fixed_10 = if_else(percentile < 10, TRUE, FALSE),
         drought_fixed_20 = if_else(percentile < 20, TRUE, FALSE),
         drought_fixed_30 = if_else(percentile < 30, TRUE, FALSE))




df_bothPrc <- df_bothPrc %>%
  mutate(drought_DOY_2 = if_else(DOYpercentile < 2, TRUE, FALSE),
         drought_DOY_5 = if_else(DOYpercentile < 5, TRUE, FALSE),
         drought_DOY_10 = if_else(DOYpercentile < 10, TRUE, FALSE),
         drought_DOY_20 = if_else(DOYpercentile < 20, TRUE, FALSE),
         drought_DOY_30 = if_else(DOYpercentile < 30, TRUE, FALSE))



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




}


