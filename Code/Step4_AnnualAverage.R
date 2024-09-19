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


AllYear_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_step2.rds") #%>% 
AllYear_StreamflowData



MediumTerm_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_MediumSubset_step3.rds") #%>% 
MediumTerm_StreamflowData


LongTerm_StreamflowData <- readRDS("/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/streamflow_tibbles_Filtered_LongSubset_step3.rds") #%>% 
LongTerm_StreamflowData





#########Annual Average

AllYear_StreamflowData<- AllYear_StreamflowData %>% select(station_name,site_no,station_lat,station_lon,streamflow_data)
AllYear_StreamflowData


AllYear_StreamflowData_AA<- AllYear_StreamflowData %>% 
  unnest(cols=streamflow_data) %>% 
  mutate(Year=year(Date)) %>% 
  group_by(site_no,Year) %>% 
  summarise(MeanAnnualQ_cfs=mean(mean_streamflow,na.rm=TRUE)) %>% 
  ungroup()
  
  
print(AllYear_StreamflowData_AA)






###for seasonal calculation


AllYear_StreamflowData_Seasonal<- AllYear_StreamflowData %>%
  unnest(cols = streamflow_data) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  mutate(Season = case_when(
    Month %in% c(9, 10, 11) ~ "Fall",
    Month %in% c(12, 1, 2) ~ "Winter",
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer"
  )) %>%
  group_by(site_no, Year, Season) %>%
  summarise(MeanSeasonalQ_cfs = mean(mean_streamflow, na.rm = TRUE)) %>%
  ungroup()


AllYear_StreamflowData_Seasonal_pivot <- AllYear_StreamflowData_Seasonal %>%
  pivot_wider(names_from = Season, values_from = MeanSeasonalQ_cfs, 
              names_prefix = "Mean", values_fill = NA)
AllYear_StreamflowData_Seasonal_pivot



Combined_Data<- AllYear_StreamflowData_AA %>% 
  left_join(AllYear_StreamflowData_Seasonal_pivot,by=c('site_no','Year'))






##Check ##############################


Combined_Data$MeanAnnualQ_cfs-AllYear_StreamflowData_AA$MeanAnnualQ_cfs
Combined_Data$MeanFall-AllYear_StreamflowData_Seasonal_pivot$MeanFall
k=data.frame(Combined_Data$MeanFall-AllYear_StreamflowData_Seasonal_pivot$MeanFall)




##Check data manually

checkdata<- data.frame(AllYear_StreamflowData$streamflow_data[25])
checkdata
checkdata<- checkdata %>% mutate(year=year(Date))
checkdata
checkdataA<- checkdata %>% 
  group_by(year) %>% 
  summarise(meanq=mean(mean_streamflow,na.rm = TRUE))
checkdataA

AllYear_StreamflowData$site_no[25]
checkdataB=Combined_Data %>% filter(site_no==AllYear_StreamflowData$site_no[25])
checkdataA$meanq-checkdataB$MeanAnnualQ_cfs


checkdataS<-checkdata %>%  mutate(Year=year(Date)) 
checkdataS<-checkdataS %>%  mutate(Month=month(Date)) 
checkdataS<- checkdataS %>% mutate(Season = case_when(
  Month %in% c(9, 10, 11) ~ "Fall",
  Month %in% c(12, 1, 2) ~ "Winter",
  Month %in% c(3, 4, 5) ~ "Spring",
  Month %in% c(6, 7, 8) ~ "Summer"
)) 

checkdataS<- checkdataS %>% 
  group_by(Year, Season) %>% 
  summarise(meanq=mean(mean_streamflow,na.rm = TRUE))
checkdataS
checkdataS<- checkdataS %>% pivot_wider(names_from = Season,values_from = meanq,values_fill = NA)
checkdataB=Combined_Data %>% filter(site_no==AllYear_StreamflowData$site_no[25])
checkdataS$Spring-checkdataB$MeanSpring





#####save


###For all year 

AllYear_StreamflowData_Annual<-Combined_Data

saveRDS(AllYear_StreamflowData_Annual,"/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/AllYear_StreamflowData_Annual_step4.rds")


#### For medium term subset 

MediumTerm_StreamflowData_Annual <- AllYear_StreamflowData_Annual %>%
  filter(site_no %in% MediumTerm_StreamflowData$site_no)

saveRDS(MediumTerm_StreamflowData_Annual,"/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/MediumTerm_StreamflowData_Annual_step4.rds")




#### For long term subset 

LongTerm_StreamflowData_Annual <- AllYear_StreamflowData_Annual %>%
  filter(site_no %in% LongTerm_StreamflowData$site_no)

saveRDS(LongTerm_StreamflowData_Annual,"/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/LongTerm_StreamflowData_Annual_step4.rds")







