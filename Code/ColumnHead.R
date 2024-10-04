
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
