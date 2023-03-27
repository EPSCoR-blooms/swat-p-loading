# code to collate cmip data and upload summary to Drive

#load libraries
library(tidyverse)
library(googledrive)
library(readxl)


# authorize google drive ----
drive_auth(email = 'steeleb@caryinstitute.org')

#point to shared drive
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id

inter = drive_ls(path = as_id(sd_id), pattern = 'intermediary_2021')

#find the folder you're interested in 
fid <- inter$id

## download to local temp folder ----
lake_names = c('Auburn', 'Barber', 'China', 'Cranberry','Floods', 'Great', 'Hadlock', 'Jordan', 'Rangeley', 'Sabattus', 'Sebago', 'Sunapee', 'Yawgoo')

#source scripts ----

for(l in 1:length(lake_names)){
  dir.create('download')
  dir.create('upload')
  lake_upload_id = drive_ls(as_id(fid), pattern = lake_names[l])$id
  source('download_from_drive.R')
  collated_horizontal = collated %>% 
    select(-stdev) %>% 
    pivot_wider(names_from = 'parameter',
                values_from = 'weighted_mean')
  write.csv(collated_horizontal, 'upload/cmip_collated.csv', row.names = F)
  drive_upload('upload/cmip_collated.csv', path = as_id(lake_upload_id))
  unlink('upload', recursive = T)
  unlink('download', recursive = T)
}
