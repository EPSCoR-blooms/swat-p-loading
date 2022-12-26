# read in data from Drive

##### Load libraries #####

library(tidyverse)
library(googledrive)
library(readxl)
library(xlsx)
library(ggpubr)
library(lubridate)

##### Load in data from Google Drive #####

# collated rolling average 3, 5, and 10 yr data

tmpdir = 'tmp' #point to directories

if(!dir.exists(tmpdir)){dir.create(tmpdir)} #create temporary directory - this creates a directory in your working directory called ‘tmp’

drive_auth() # authorize googledrive to access your Google Drive -- follow browser instructions or the prompts in the console.

did = shared_drive_find('EPSCoR_SWAT')$id #find shared drive id

folder1_id = drive_ls(path = as_id(did), pattern = 'SWAT_collatedFiles')$id

folder2_id = drive_ls(path = as_id(folder1_id), pattern = 'rollingave')$id #get folder id

csv_files <- drive_ls(folder2_id, type = "csv")

#downloads all files in the "collated" folder into temporary folder in R
csv_files %>% 
  split(csv_files$id) %>% 
  walk(~drive_download(.$id, path = file.path("tmp", .$name), overwrite = TRUE))

#reads in all collated files
swat_rollingave_10 <- list.files(path="tmp/", pattern = "10yr_rolling", full.names = T) %>% 
  lapply(read.csv) %>% 
  bind_rows()

swat_rollingave_5 <- list.files(path="tmp/", pattern = "5yr_rolling", full.names = T) %>% 
  lapply(read.csv) %>% 
  bind_rows()

swat_rollingave_3 <- list.files(path="tmp/", pattern = "3yr_rolling", full.names = T) %>% 
  lapply(read.csv) %>% 
  bind_rows()

# make sure that climate model, rcp, and landcover year are all factors

swat_rollingave_10$modelrcp <- as.factor(swat_rollingave_10$modelrcp)
swat_rollingave_10$landcover_year <- as.factor(swat_rollingave_10$landcover_year)
swat_rollingave_10$lake <- as.factor(swat_rollingave_10$lake)

# plot to show increase in sediment P in by decade, for one lake at a time (for all models and lakes avg)

swat_rollingave_10_labeled <- swat_rollingave_10 %>% 
  group_by(lake) %>% 
  mutate(decade = case_when(yrs >= 1984.5 & yrs < 1994.5 ~ "diff_1984.5",
                            yrs >= 1994.5 & yrs < 2004.5 ~ "diff_1994.5",
                            yrs >= 2004.5 & yrs < 2014.5 ~ "diff_2004.5",
                            yrs >= 2014.5 & yrs < 2024.5 ~ "diff_2014.5",
                            yrs >= 2024.5 & yrs < 2034.5 ~ "diff_2024.5",
                            yrs >= 2034.5 & yrs < 2044.5 ~ "diff_2034.5",
                            yrs >= 2044.5 & yrs < 2054.5 ~ "diff_2044.5"))
  
swat_rollingave_10_labeled$decade <- as.factor(swat_rollingave_10_labeled$decade)
  
swat_rollingave_10_labeled_1 <- swat_rollingave_10_labeled %>%     
  dplyr::group_by(lake, decade) %>% 
  select(lake, decade, sedp_in_kgP_10y) %>% 
  dplyr::summarize(mean_sedp_in_kgP_10y = mean(sedp_in_kgP_10y)) %>%     
  group_by(lake) %>%
  dplyr::mutate(diff = dplyr::lag(mean_sedp_in_kgP_10y, n = 1, default = NA)) %>% 
  mutate(difference = diff - mean_sedp_in_kgP_10y) %>% 
  mutate(difference = case_when(difference < 0 ~ 0, TRUE ~ difference))
  
swat_rollingave_10_labeled_baseline <- swat_rollingave_10_labeled_1 %>% 
  filter(decade == "diff_1984.5") %>% 
  select(lake, decade, mean_sedp_in_kgP_10y) %>% 
  dplyr::rename(baseline = mean_sedp_in_kgP_10y)
  
 swat_rollingave_10_labeled_baseline_merged <- merge(swat_rollingave_10_labeled_1, swat_rollingave_10_labeled_baseline,
                                                      by = "lake") %>% 
  mutate(calc = case_when(difference = is.na(difference) ~ baseline,
                            TRUE ~ difference)) %>% 
  select(lake, decade.x, mean_sedp_in_kgP_10y, calc, baseline) %>%     
  na.omit() %>% 
  mutate(perc_change = calc/baseline)
 
ggplot() +
 geom_col(data = swat_rollingave_10_labeled_baseline_merged, 
          aes(x=lake, y=calc, fill = decade.x), 
          position = position_stack(reverse = TRUE)) +
 theme_minimal() +
  ylab("Sediment P In (kg/P)") +
  xlab("")
  
# plot to show percent change in sediment P in by decade, for one lake at a time (for all models and lakes avg)
ggplot() +
  geom_col(data = swat_rollingave_10_labeled_baseline_merged, 
           aes(x=lake, y=perc_change, fill = decade.x), 
           position = position_fill(reverse = TRUE)) + 
  theme_minimal() +
  ylab("Sediment P In (% change)") +
  xlab("")