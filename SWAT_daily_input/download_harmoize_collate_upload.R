# this script collates the NLDAS/PRISM daily data with the CMIP data for all
# projections, formatted for SWAT

library(tidyverse)
library(googledrive)

drive_auth(email = 'steeleb@caryinstitute.org')

lake_names = c('Auburn', 'Barber', 'China', 'Cranberry','Floods', 'Great', 'Hadlock', 'Jordan', 'Rangeley', 'Sabattus', 'Sebago', 'Sunapee', 'Yawgoo')

#get drive ids
did = shared_drive_find(pattern = 'EPSCoR_SWAT')$id
nldas_id = drive_ls(as_id(did), pattern = 'Historical Daily')$id
cmip_id = drive_ls(as_id(did), pattern = 'intermediary_2021')$id
dump_id = drive_ls(as_id(did), pattern = 'SWAT_daily')$id

#make directories for each lake in dump dir
for(n in 1:length(lake_names)) {
  drive_mkdir(name = lake_names[n],
              path = as_id(dump_id))
}

for (l in 2:length(lake_names)) {
  #make tmp directory
  dir.create('tmp')
  tmp_dir = 'tmp'
  
  # download and collate files ----
  
  lake = lake_names[l]
  
  #get nldas/prism file
  n_lake_fid = drive_ls(as_id(nldas_id), pattern = lake)$id
  n_file = drive_ls(as_id(n_lake_fid), pattern = 'collated')
  drive_download(as_id(n_file$id), 
                 path = file.path(tmp_dir, n_file$name))
  PN_data = read.csv(file.path(tmp_dir, n_file$name)) %>% 
    mutate(date = as.Date(date))
  
  #we need the median daily solar rad from nldas to apply to cmip file
  rad_est = PN_data %>% 
    filter(date >= as.Date('2010-01-01')) %>% 
    select(date, solar_rad) %>% 
    filter(solar_rad > 0) %>% 
    mutate(doy = format(date, '%j')) %>% 
    group_by(doy) %>% 
    summarise(solar_rad_est = median(solar_rad))
  
  #if there are any weirdo 0 values in the NLDAS, fix those too with the median
  PN_data = PN_data %>% 
    mutate(doy = format(date, '%j')) %>% 
    left_join(., rad_est) %>% 
    mutate(solar_rad = if_else(solar_rad == 0, solar_rad_est, solar_rad)) %>% 
    select(-solar_rad_est, -doy) %>% 
    filter(date < as.Date('2021-01-01'))
  #format for SWAT
  PN_data = PN_data %>% 
    mutate(ppt_mm = round(ppt_mm, 1),
           rel_hum = round(rel_hum, 2),
           min_temp_degC = round(min_temp_degC, 1),
           max_temp_degC = round(max_temp_degC, 1),
           wind_sp = round(wind_sp, 1),
           solar_rad = round(solar_rad, 2))
  
  #get cmip file
  c_lake_fid = drive_ls(as_id(cmip_id), pattern = lake)$id
  c_file = drive_ls(as_id(c_lake_fid), pattern = 'collated')
  drive_download(as_id(c_file$id),
                 path = file.path(tmp_dir, c_file$name))
  c_data = read.csv(file.path(tmp_dir, c_file$name)) %>% 
    mutate(date = as.Date(date),
           doy = format(date, '%j')) %>% 
    left_join(., rad_est) %>% 
    select(-doy)
  #format for swat
  c_data = c_data %>% 
    mutate(precip = round(precip, 1),
           relHumid = round(relHumid/100, 2),
           tasmax = round(tasmax, 1),
           tasmin = round(tasmin, 1),
           windspeed = round(windspeed, 1),
           solar_rad_est = round(solar_rad_est, 2)) %>% 
    rename(ppt_mm = precip,
           rel_hum = relHumid,
           min_temp_degC = tasmin,
           max_temp_degC = tasmax,
           wind_sp = windspeed,
           solar_rad = solar_rad_est)
  
  #join data
  all_data = full_join(PN_data, c_data) %>% 
    arrange(date)
  
  #get projections
  proj_list = unique(all_data$cmip_projection)
  proj_list = proj_list[!is.na(proj_list)]
  
  #get output folder id
  lake_out_id = drive_ls(as_id(dump_id), pattern = lake)$id
  
  #make folders for each projection
  for(proj in 1:length(proj_list)) {
    drive_mkdir(name = proj_list[proj],
                path = as_id(lake_out_id))
  }
  
  # empty tmp folder
  unlink('tmp', recursive = T)
  
  #make files for each proj
  for(proj in 1:length(proj_list)){
    dir.create('tmp')
    p = proj_list[proj]
    #make indiv files
    ppt = all_data %>% 
      filter(is.na(cmip_projection) | cmip_projection == p) %>% 
      select(ppt_mm)%>% 
      rename(`19810101` = ppt_mm)
    write_delim(ppt, file.path('tmp', 'pcp.txt'), delim = ',')
    temp = all_data %>% 
      filter(is.na(cmip_projection) | cmip_projection == p) %>% 
      select(max_temp_degC, min_temp_degC)%>% 
      rename(`19810101` = max_temp_degC,
             ` ` = min_temp_degC)
    write_delim(temp, file.path('tmp', 'tmp.txt'), delim = ',')
    wind = all_data %>% 
      filter(is.na(cmip_projection) | cmip_projection == p) %>% 
      select(wind_sp)%>% 
      rename(`19810101` = wind_sp)
    write_delim(wind, file.path('tmp', 'wnd.txt'), delim = ',')
    solar_rad = all_data %>% 
      filter(is.na(cmip_projection) | cmip_projection == p)  %>% 
      select(solar_rad)%>% 
      rename(`19810101` = solar_rad)
    write_delim(solar_rad, file.path('tmp', 'slr.txt'), delim = ',')
    hum = all_data %>% 
      filter(is.na(cmip_projection) | cmip_projection == p) %>% 
      select(rel_hum) %>% 
      rename(`19810101` = rel_hum)
    write_delim(hum, file.path('tmp', 'hum.txt'), delim = ',')
    up_list = list.files('tmp')
    proj_id = drive_ls(as_id(lake_out_id), pattern = p)$id
    for(u in 1:length(up_list)) {
      drive_upload(file.path('tmp', up_list[u]),
                   as_id(proj_id),
                   overwrite = T)
    }
    unlink('tmp', recursive = T)
  }
}
