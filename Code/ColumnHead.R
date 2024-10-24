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



file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
Medium_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "MediumTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_DroughtDuration_DroughtDeficit_step9.rds"))
Medium_StreamflowData

Medium_StreamflowData <- Medium_StreamflowData %>%
  rename(
    MeanAnnualQ_MMD = MeanAnnualQ_cfs,
    MeanFall_MMD = MeanFall,
    MeanSpring_MMD = MeanSpring,
    MeanSummer_MMD = MeanSummer,
    MeanWinter_MMD = MeanWinter,
    Min_7DayStreamflow_MMD = Min_7DayStreamflow,
    max_7DayStreamflow_MMD = max_7DayStreamflow,
    Min_7DayStreamflow_Fall_MMD = Min_7DayStreamflow_Fall,
    Min_7DayStreamflow_Spring_MMD = Min_7DayStreamflow_Spring,
    Min_7DayStreamflow_Summer_MMD = Min_7DayStreamflow_Summer,
    Min_7DayStreamflow_Winter_MMD = Min_7DayStreamflow_Winter
  )

Medium_StreamflowData$Unit<- 'MMD'
Medium_StreamflowData<- Medium_StreamflowData %>%  select(site_no,station_name.x, station_lat,station_lon,Unit,everything())


write_xlsx(Medium_StreamflowData,file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData.xlsx"))




Long_StreamflowData <- readRDS(file.path(file_Path_Variable_O, "LongTerm_StreamflowData_Annual_Min7_Max7_Min7Seasonal_Max7Seasonal_DroughtDuration_DroughtDeficit_step9.rds"))
Long_StreamflowData

Long_StreamflowData <- Long_StreamflowData %>%
  rename(
    MeanAnnualQ_MMD = MeanAnnualQ_cfs,
    MeanFall_MMD = MeanFall,
    MeanSpring_MMD = MeanSpring,
    MeanSummer_MMD = MeanSummer,
    MeanWinter_MMD = MeanWinter,
    Min_7DayStreamflow_MMD = Min_7DayStreamflow,
    max_7DayStreamflow_MMD = max_7DayStreamflow,
    Min_7DayStreamflow_Fall_MMD = Min_7DayStreamflow_Fall,
    Min_7DayStreamflow_Spring_MMD = Min_7DayStreamflow_Spring,
    Min_7DayStreamflow_Summer_MMD = Min_7DayStreamflow_Summer,
    Min_7DayStreamflow_Winter_MMD = Min_7DayStreamflow_Winter
  )

Long_StreamflowData$Unit<- 'MMD'
Long_StreamflowData<- Long_StreamflowData %>%  select(site_no,station_name.x, station_lat,station_lon,Unit,everything())
length(unique(Long_StreamflowData$site_no))
length(unique(Medium_StreamflowData$site_no))

#one has more years and another has more sites, so pretty same row numbers
write_xlsx(Long_StreamflowData,file.path(file_Path_Variable_O, "HydrologicalSignatures_LongTerm_StreamflowData.xlsx"))














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


library(kableExtra)
library(tibble)
column_headers <- c("Station", "Year", "MeanAnnualQ_cfs", "MeanFall", "MeanSpring", "MeanWinter", "MeanSummer", "Min7dayQ", "DroughtDuration", "DroughtDeficit")
meanings <- c("Station ID", 
              "Year of record", 
              "Mean Annual Streamflow", 
              "Mean Streamflow during Fall", 
              "Mean Streamflow during Spring", 
              "Mean Streamflow during Winter", 
              "Mean Streamflow during Summer", 
              "Minimum 7-day Streamflow", 
              "Drought Duration", 
              "Drought Deficit")
units <- c("N/A", 
           "Year", 
           "cfs", 
           "cfs", 
           "cfs", 
           "cfs", 
           "cfs", 
           "cfs", 
           "cfs", 
           "days")

description_table <- tibble(
  Column = column_headers,
  Meaning = meanings,
  Unit = units
)

description_table %>%
  kbl(caption = "Table: Column Descriptions and Units", align = "lcc") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))
