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

##Method from Preota's report

# The three types of extreme occurrences were calculated based on the percentile rank of the annual precipitation.
# The percentile rank of each pixel was calculated from 1904-2022 of the PRISM data. Depending on the percentile 
# rank and the percentile rank change between the consecutive years (isolated wet ≥ 80 percentile, isolated dry ≤ 20 
# percentile, wet-to-dry ≥ 60 percentile change, dry-to-wet ≤ 60 percentile change, dry-to-dry ≤ 20 percentile change,
# wet-to-wet ≥ 80 percentile change), the extremes were initially identified [12, 13]. Afterward, it created more than 1 
# extreme in a specific space and time. To overcome this, whiplash events were given more priority than recurring ones. 
# The remaining ones were considered isolated events. In each raster layer, the extremes were marked as 1 else 0. 
# The stack and sum of these rasters later gave the total occurrence of a certain extreme over the years.


##Gaurav's code ????? 
##check again if that was daily or yearly 
# is_wet = percentile_precipitation > 80,
# is_dry = percentile_precipitation < 20,
# is_wet2dry = percentile_change > 50,
# is_dry2wet = percentile_change < -50,
# wet2wet = is_wet & lag(is_wet),
# dry2dry = is_dry & lag(is_dry) 


##Not same?? 



#####From other code

# file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/InputFiles"
# file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
# 
# tiff_folder_path <- file.path(file_Path_Variable_I, "Prism/masked_monthly_to_annual")
# 
# tiff_files <- list.files(tiff_folder_path, pattern = "\\.tif$", full.names = TRUE)
# 
# rainfall_tibble <- tibble(
#   file_name = character(),
#   rainfall_data = list()
# )
# 
# for (tiff_file in tiff_files) {
#   rainfall_raster <- rast(tiff_file)
#   
#   rainfall_df <- as.data.frame(rainfall_raster, xy = TRUE)
#   
#   rainfall_tibble <- rainfall_tibble %>%
#     add_row(
#       file_name = basename(tiff_file),  
#       rainfall_data = list(rainfall_df)  
#     )
# }
# 
# 
# rainfall_tibble <- rainfall_tibble %>%
#   mutate(Year = as.numeric(gsub("\\D", "", file_name)))
# 
# rainfall_tibble <- rainfall_tibble %>%
#   arrange(Year)
# 
# 
# rainfall_tibble <- rainfall_tibble %>%
#   mutate(Year_N = 1903 + Year) 
# 
# saveRDS(rainfall_tibble, file = file.path(file_Path_Variable_O, "rainfall_tibble_masked_monthly_to_annual_step10.rds"))


file_Path_Variable_O<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"

rainfall_tibble <- readRDS(file.path(file_Path_Variable_O,"rainfall_tibble_masked_monthly_to_annual_step10.rds"))
rainfall_tibble
rainfall_tibble<- rainfall_tibble %>% filter(Year_N<1910)

rainfall_tibble <- rainfall_tibble %>%
  mutate(
    rainfall_data = map(rainfall_data, ~ .x %>%
                          rename(total_rainfall = starts_with("total_annual_rainfall")))
  )

# Step 1: Combine all years into a single dataframe
combined_rainfall <- bind_rows(
  rainfall_tibble %>%
    mutate(data_with_year = map2(rainfall_data, Year_N, ~ mutate(.x, Year = .y))) %>% #each rainfall_data (denoted as .x) and its corresponding Year_N value (denoted as .y)
    pull(data_with_year)
)

# Step 2: Calculate Percentile Ranks Across Years
percentile_rainfall <- combined_rainfall %>%
  group_by(x, y) %>%  #I grouped it by lon,lat
  summarise(
    p20 = quantile(total_rainfall, 0.2, na.rm = TRUE),
    p80 = quantile(total_rainfall, 0.8, na.rm = TRUE)
  )  # location-specific thresholds for identifying rainfall extremes (wet or dry)


##Combine rainfall_tibble and  percentile_rainfall

# rainfall_tibble_percentile <- left_join(rainfall_tibble, percentile_rainfall, by = c("x", "y"))


# Step 3: Identify Extreme Events
identify_extremes <- function(df, percentile_data) {
  df <- left_join(df, percentile_data, by = c("x", "y"))
  df <- df %>%
    arrange(Year) %>%
    mutate(
      isolated_wet = if_else(total_rainfall >= p80, 1, 0),
      isolated_dry = if_else(total_rainfall <= p20, 1, 0),
      change = total_rainfall - lag(total_rainfall),
      wet_to_dry = if_else(change <= -0.6 * p80, 1, 0),
      dry_to_wet = if_else(change >= 0.6 * p20, 1, 0),
      dry_to_dry = if_else(lag(total_rainfall) <= p20 & total_rainfall <= p20, 1, 0),
      wet_to_wet = if_else(lag(total_rainfall) >= p80 & total_rainfall >= p80, 1, 0)
    )
  return(df)
}
rainfall_tibble <- rainfall_tibble %>%
  mutate(
    rainfall_data = map2(rainfall_data, Year_N, ~ mutate(.x, Year = .y))
  )

# Apply to all rainfall data
rainfall_with_extremes <- rainfall_tibble %>%
  mutate(
    rainfall_data = map(rainfall_data, ~ identify_extremes(.x, percentile_rainfall))
  )

# Step 4: Handle Conflicts
resolve_conflicts <- function(df) {
  df <- df %>%
    mutate(
      whiplash = wet_to_dry | dry_to_wet,
      recurring = wet_to_wet | dry_to_dry,
      final_extreme = case_when(
        whiplash ~ 1,
        !whiplash & (isolated_wet | isolated_dry) ~ 1,
        TRUE ~ 0
      )
    )
  return(df)
}

rainfall_with_resolved <- rainfall_with_extremes %>%
  mutate(
    rainfall_data = map(rainfall_data, resolve_conflicts)
  )

# Step 5: Create Binary Rasters and Summarize
create_binary_rasters <- function(df) {
  raster <- rast(df %>% select(x, y, final_extreme))
  return(raster)
}

extreme_rasters <- rainfall_with_resolved %>%
  mutate(
    extreme_raster = map(rainfall_data, create_binary_rasters)
  )

extreme_stack <- rast(extreme_rasters$extreme_raster)
total_occurrences <- app(extreme_stack, sum)

plot(total_occurrences, main = "Total Extreme Occurrences (1904-2022)")
