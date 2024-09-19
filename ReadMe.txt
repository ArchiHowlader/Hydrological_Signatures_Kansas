Step 1: Pull data from USGS website

Code name: Step1_PullDataFromUSGS


1. Use the shapefile to pull data from the USGS website
desoto_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/DeSoto_shp/DeSoto.shp"
watershed_shapefile_path <- "/Users/ahowl/Desktop/KGS Data analysis/WatershedBoundary_KN_20230113/watershed_bndry.shp"


2. Map created with station data (mean streamflow, parameter cd=X_00060_00003

parameterCd = "00060": Requests streamflow data in cubic feet per second.
X_00060_00003: Represents the mean daily streamflow in cubic feet per second (cfs)

3. a tibble with cleaned up data 

4. Check pulled up data from code to usgs station


Output:

1. Step1_Output1_WebMapper_meanstreamflow.html 
(file with interactive mapper for the stations)

2. streamflow_tibbles_step1.rds 
(all the preliminary streamflow data)





Step 2: Streamflow data qaqc
Code name: Step2_StreamflowData_QAQC


1. Found the sampling years of the stations (plots: timeline)
2. Found missing data 
3. Found stations with less than 10 percent missing data

Output:

1. Step2_timeline figs (shows years when the stations were sampled)
2. streamflow_tibbles_Filtered_step2 (filtered tibble with  less than 10 percent missing data)
3. Histogram of a) percent of missing data vs stations b)num of years vs. stations






Step 3: Streamflow data separating Medium and Long term subset
Code name: Step3_SeperatingLongMediumSubset


1. Medium term: data on >90% of days between 1/1/1979 and 12/31/2023
    Long term: data on >90% of days between 1/1/1944 and 12/31/2023
2. Made a dataframe from start and end date and combine it with the original data to find out the missing values 
3. Found stations with less than 10 percent missing data
4. Timeline figures 
5. Stations figures 


Output:

1. Step3_timeline figs (shows years when the stations were sampled for Medium and Long term)
2. streamflow_tibbles_Filtered_LongSubset_step3.rds
3. streamflow_tibbles_Filtered_MediumSubset_step3.rds 
4. Step3_Streamflowdata_locations_LongSubset (Location map of Long term subset)
5.Step3_Streamflowdata_locations_MediumSubset (Location map of Medium term subset)






Step 4: Streamflow data annual average, seasonal average (of the years)
Code name: Step4_AnnualAverage

1. Annual and Seasonal Calculation: Calculates the annual and seasonal mean streamflow for each site using summarise() and groups by year and season.
2. Combining Data: Merges the annual and seasonal streamflow data into a single dataset (Combined_Data).
3. Subset Creation: Filters the AllYear_StreamflowData_Annual dataset for medium-term and long-term subsets based on site numbers

Output:
1. LongTerm_StreamflowData_Annual
2. MediumTerm_StreamflowData_Annual
3. AllYear_StreamflowData_Annual





Step 5: Streamflow Data - Overall Average Streamflow Mapping
Code Name: Step5_overall_average_streamflow

1. Overall Average Calculation: Computes the overall mean streamflow (cfs) for each site across all years, using summarise(), grouped by site_no.
2. Map Creation: Visualizes streamflow data on a map using ggplot2 and leaflet, with points sized and colored based on overall average streamflow.
3. Subset Filtering: Filters the data into medium-term and long-term subsets by matching site_no with respective datasets.


Outputs: Saves three maps (all-year, medium-term, long-term) with streamflow data visualized by location and average streamflow.
Output Files:

Step4_overall_average_streamflow_AllYear.jpg
Step4_overall_average_streamflow_MediumTerm_Map.jpg
Step4_overall_average_streamflow_LongTerm_Map.jpg




Step 6: Seasonal Average Streamflow Mapping
Code Name: Step6_OverAllSeasonalAverageFigure

1. Seasonal Average Calculation: Computes the seasonal mean streamflow (cfs) for each site across the Fall, Winter, Spring, and Summer seasons, grouped by site_no.
2. Map Creation: Visualizes the seasonal streamflow data on a map using ggplot2 and leaflet, with separate maps for Fall, Winter, Spring, and Summer for all stations, as well as for medium-term and long-term subsets.
3. Subset Filtering: Filters the seasonal streamflow data for medium-term and long-term subsets based on site_no.
Outputs: Saves visualized maps of seasonal streamflow for all stations, medium-term, and long-term sites.
Output Files:

Step6_overall_average_streamflow_AllYear_Map_MeanFall_AllStation.jpg
Step6_overall_average_streamflow_AllYear_Map_MeanSummer_AllStation.jpg
Step6_overall_average_streamflow_AllYear_Map_MeanSpring_AllStation.jpg
Step6_overall_average_streamflow_AllYear_Map_MeanWinter_AllStation.jpg
Step6_SeasonalAveragePlot_AllYear_AllStation.jpg
Corresponding output files for medium-term and long-term datasets (e.g., Step6_overall_average_streamflow_AllYear_Map_MeanFall_MediumTerm.jpg, etc.)

