library(dplyr)
library(stringr)

root_dir <- "/Users/ahowl/Desktop/KGS Data analysis/TooBigForGithub_Steps_Workflow_Sept17/InputFiles/PRISM"
all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
file_df <- data.frame(file_path = all_files)

file_df <- file_df %>%
  mutate(
    folder_parts = strsplit(file_path, "/") 
  ) %>%
  rowwise() %>%
  mutate(
    Folder = folder_parts[[length(folder_parts) - 2]],          
    Subfolder = folder_parts[[length(folder_parts) - 1]],       
    Subfolder_s_subfolder = ifelse(length(folder_parts) >= 3, folder_parts[[length(folder_parts)]], NA)  
  ) %>%
  ungroup() %>%
  select(Folder, Subfolder, Subfolder_s_subfolder) 

print(file_df)
