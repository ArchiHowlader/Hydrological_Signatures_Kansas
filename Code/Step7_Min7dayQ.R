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



AllYear_StreamflowData_7DayAvg <- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  group_by(site_no) %>%  
  mutate(Year = year(Date)) %>%
  arrange(Date) %>%
  mutate(Streamflow_7DayAvg = rollmean(mean_streamflow, 7, fill = NA, align = "center")) %>%
  ungroup()



Annual_Min_7DayStreamflow <- AllYear_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  summarise(Min_7DayStreamflow = min(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  ungroup()








#########Combine the data with annual data, and subset the medium and long term, save files 


AllYear_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "AllYear_StreamflowData_Annual_step4.rds"))
MediumTerm_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "MediumTerm_StreamflowData_Annual_step4.rds"))
LongTerm_StreamflowData_Annual <- readRDS(file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_step4.rds"))



AllYear_StreamflowData_Annual_Min7<- AllYear_StreamflowData_Annual %>% 
  left_join(Annual_Min_7DayStreamflow, by=c('site_no','Year'))
AllYear_StreamflowData_Annual_Min7




MediumTerm_StreamflowData_Annual_Min7<- MediumTerm_StreamflowData_Annual %>% 
  left_join(Annual_Min_7DayStreamflow, by=c('site_no','Year'))
MediumTerm_StreamflowData_Annual_Min7



LongTerm_StreamflowData_Annual_Min7<- LongTerm_StreamflowData_Annual %>% 
  left_join(Annual_Min_7DayStreamflow, by=c('site_no','Year'))
LongTerm_StreamflowData_Annual_Min7


length(unique(LongTerm_StreamflowData_Annual_Min7$site_no))
length(unique(MediumTerm_StreamflowData_Annual_Min7$site_no))





saveRDS(AllYear_StreamflowData_Annual_Min7,file.path(file_Path_Variable_O, "AllYear_StreamflowData_Annual_Min7_step7.rds"))
saveRDS(MediumTerm_StreamflowData_Annual_Min7,file.path(file_Path_Variable_O, "MediumTerm_StreamflowData_Annual_Min7_step7.rds"))
saveRDS(LongTerm_StreamflowData_Annual_Min7,file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_step7.rds"))






















#########make the figure to check data
Annual_Min_7DayStreamflow_WithDate <- AllYear_StreamflowData_7DayAvg %>%
  group_by(site_no, Year) %>%
  filter(Streamflow_7DayAvg == min(Streamflow_7DayAvg, na.rm = TRUE)) %>%
  summarise(Min_7DayStreamflow = min(Streamflow_7DayAvg, na.rm = TRUE), 
            Min_7DayStreamflow_Date = Date) %>%
  ungroup()

Annual_Min_7DayStreamflow_WithDate <- Annual_Min_7DayStreamflow_WithDate %>%
  left_join(Annual_Min_7DayStreamflow, by = c("site_no", "Year", "Min_7DayStreamflow"))











StationData<- data.frame(AllYear_StreamflowData$streamflow_data[10])
StationDataRollingMean<- AllYear_StreamflowData_7DayAvg %>% filter(site_no==AllYear_StreamflowData$site_no[10])
StationDataMin7<-Annual_Min_7DayStreamflow_WithDate %>% filter(site_no==AllYear_StreamflowData$site_no[10])
station_name<-AllYear_StreamflowData$station_name[10]
p<- ggplot() +
  geom_line(data = StationData, aes(x = Date, y = mean_streamflow, color = "Mean Streamflow"), size = 1) +  
  geom_line(data = StationDataRollingMean, aes(x = Date, y = Streamflow_7DayAvg, color = "7-day Moving Average"), size = 1, linetype = "dashed") +  
  geom_point(data = StationDataMin7, aes(x = Min_7DayStreamflow_Date, y = Min_7DayStreamflow, color = "7-day Minimum"), size = 2) +  
  labs(title =paste ("Streamflow, 7-day Moving Average, and 7-day Minimum \n",station_name),
       x = "Date",
       y = "Streamflow (cfs)") + 
  scale_color_manual(values = c("Mean Streamflow" = "blue", 
                                "7-day Moving Average" = "green", 
                                "7-day Minimum" = "red")) + 
  scale_x_date(limits = as.Date(c("1935-01-01", "1945-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
p




ggsave(filename =(file.path(file_Path_Variable_O,"Step7_Min7dayq_exmplsta2.jpg")), 
       plot = p, 
       width = 8, height = 6, dpi = 300)












StationData<- data.frame(AllYear_StreamflowData$streamflow_data[30])
StationDataRollingMean<- AllYear_StreamflowData_7DayAvg %>% filter(site_no==AllYear_StreamflowData$site_no[30])
StationDataMin7<-Annual_Min_7DayStreamflow_WithDate %>% filter(site_no==AllYear_StreamflowData$site_no[30])
station_name<-AllYear_StreamflowData$station_name[30]
p<- ggplot() +
  geom_line(data = StationData, aes(x = Date, y = mean_streamflow, color = "Mean Streamflow"), size = 1) +  
  geom_line(data = StationDataRollingMean, aes(x = Date, y = Streamflow_7DayAvg, color = "7-day Moving Average"), size = 1, linetype = "dashed") +  
  geom_point(data = StationDataMin7, aes(x = Min_7DayStreamflow_Date, y = Min_7DayStreamflow, color = "7-day Minimum"), size = 2) +  
  labs(title =paste ("Streamflow, 7-day Moving Average, and 7-day Minimum \n",station_name),
       x = "Date",
       y = "Streamflow (cfs)") + 
  scale_color_manual(values = c("Mean Streamflow" = "blue", 
                                "7-day Moving Average" = "green", 
                                "7-day Minimum" = "red")) + 
  scale_x_date(limits = as.Date(c("1964-01-01", "1970-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
p



ggsave(filename =(file.path(file_Path_Variable_O,"Step7_Min7dayq_exmplsta1.jpg")), 
       plot = p, 
       width = 8, height = 6, dpi = 300)

