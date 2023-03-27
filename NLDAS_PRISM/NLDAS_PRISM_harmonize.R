## this script downloads the collated NLDAS and PRISM files from Drive
## then harmonizes data from the two sources and formats them for 
## use in SWAT

library(tidyverse)
library(googledrive)
library(lubridate)
drive_auth(email = 'steeleb@caryinstitute.org')

## download files to temp folder ----
#create tmp folder
temp_dir = 'tmp'
dir.create('tmp')

#point to did's for Drive download
#find root folder
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id
#and parent folder
p_did = drive_ls(path =as_id(sd_id), pattern = 'Historical Daily')$id

# download to tmp folder
lake_names = c('Auburn', 'Barber', 'China', 'Cranberry','Floods', 'Great', 'Hadlock', 'Jordan', 'Rangeley', 'Sabattus', 'Sebago', 'Sunapee', 'Yawgoo')

for(l in 1:length(lake_names)) {
  lake = lake_names[l]
  l_did = drive_ls(path = as_id(p_did), pattern = lake)$id
  lake_files = drive_ls(path = as_id(l_did))
  for(f in 1:nrow(lake_files)) {
    if(grepl('PRISM', lake_files$name[f])) {
      new_file_name = paste(lake_names[l], lake_files$name[f], sep = '_')
    } else {
      new_file_name = lake_files$name[f]
    }
    drive_download(as_id(lake_files$id[f]),
                   path = file.path(temp_dir, new_file_name),
                   overwrite = T)
  }
}

## process each lake
lake_files_local = list.files(temp_dir)

for (l in 1:length(lake_names)) {
  lake = lake_names[l]
  one_lake = lake_files_local[grepl(lake, lake_files_local)]
  N_one_lake = one_lake[grepl('NLDAS', one_lake)]
  P_one_lake = one_lake[grepl('PRISM', one_lake)]
  
  for (n in 1:length(N_one_lake)) {
    file = read.csv(file.path(temp_dir, N_one_lake[n]))
    if (n == 1) {NLDAS = file} else {NLDAS = full_join(NLDAS, file)}
  }
  
  PRISM = read.csv(file.path(temp_dir, P_one_lake), skip = 10,
                   col.names = c('date', 'ppt_mm', 'min_temp_degC', 'max_temp_degC')) %>% 
    mutate(date = as.Date(date))
  
  # a prism day is 12UTC to 12UTC (7-7EST) so let's copy that timeframe by 'tricking' the tz.
  NLDAS_summary = NLDAS %>%
    mutate(datetime = (as.POSIXct(datetime, format = '%Y-%m-%d %H:%M', tz = 'UTC')),
           prism_date = as.Date(datetime-hours(7))) %>% 
    group_by(prism_date) %>% 
    summarize(solar_rad = sum(solarRad_KJpm2), 
              rel_hum = mean(rel_hum),
              wind_sp = mean(wind_mps)) %>% 
    rename(date = prism_date)
  
  # collate with prism 
  all_met = left_join(PRISM, NLDAS_summary)
  
  write.csv(all_met, 
            file.path(temp_dir,
                      paste0(lake, '_collated_met_v', Sys.Date(), '.csv')), 
            row.names = F)
  l_did = drive_ls(path = as_id(p_did), pattern = lake)$id
  drive_upload(file.path(temp_dir,(list.files(temp_dir, pattern = paste0(lake, '_collated')))),
               path = as_id(l_did))
}

unlink('tmp', recursive = T)                 
