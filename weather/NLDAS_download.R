## This script downloads NLDAS-2 forcing data using the 'data rods' approach.
## More information here: https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods

library(tidyverse)
library(googledrive)

drive_auth(email = 'steeleb@caryinstitute.org')

##Timeframe: 1980-2021
##Desired parameters: wind speed, specific humidity, solar radiation, temp (for rh calc)

start_date = '1980-01-01'
end_date = '2021-01-01'

params = c('UGRD10m', 'VGRD10m', 'TMP2m', 'DSWRFsfc', 'SPFH2m')

lake_names = c('Auburn', 'Barber', 'China', 'Cranberry','Floods', 'Great', 'Hadlock', 'Jordan', 'Rangeley', 'Sabattus', 'Sebago', 'Sunapee', 'Yawgoo')
loc_lat = c('44.1628', '41.5060', '44.4444', '44.1053', '44.7434', '44.5757', '44.3249', '44.3352', '44.9438', '44.1635', '44.0429', '43.3978', '41.5060')
loc_lon = c('-70.2528', '-71.5873', '-69.5342', '-74.8647', '-68.5155', '-69.8340', '-68.2838', '-68.2571', '-70.6622', '-70.1005', '-70.6402', '-72.0471', '-71.5873')

## FUNCTIONS ----

# specific humidity to relative humidity via pecan
# qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
# temp degrees C
# press pressure in mb
# returns rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio

qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

# meridional and zonal wind to wind speed
# mer = meridional (n/s) or 'V'
# zon = zonal (e/w) or 'U' 
# returns wind speed m/s
UV2sp = function(mer, zon) {
  sqrt(zon*zon + mer*mer)
}

# create the data rod http
# param is the NLDAS formatted parameter
# lat, long, start_date, end_date are defined elsewhere.
www = function(param, lat, lon, start_date, end_date){
  paste0('https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=NLDAS:NLDAS_FORA0125_H.002:', param, '&location=GEOM:POINT(', lon, ',%20', lat,  ')&startDate=', start_date, 'T00&endDate=', end_date, 'T23&type=asc2')
}

# download the file
# param is the NLDAS formatted parameter
# lake, start_date, end_date are defined elsewhere
fn = function(lake, param, start_date, end_date){
  paste0(lake, '_NLDAS_', param, '_', start_date, '-', end_date, '.csv')
}

## pull it all together and download the files ----
#make a tmp folder
temp_dir = 'tmp'
dir.create(temp_dir)
#download the files locally
for (l in 1:length(lake_names)) {
  lk = lake_names[l]
  lt = loc_lat[l]
  ln = loc_lon[l]
  for (p in 1:length(params)) {
    pm = params[p]
    file = fn(lk, pm, start_date, end_date)
    path = www(pm, lt , ln, start_date, end_date)
    download.file(url = path, 
                  destfile = file.path(temp_dir, file))
  }
}

## process the files ----

## get data from file
data_fx = function(data){
  as.numeric(unlist(strsplit(data, 'Z'))[2])
}
## get datetime from file
dt_fx = function(data){
  dt = as.POSIXct(unlist(strsplit(data, 'Z'))[1], '%Y-%m-%d %H', tz = 'UTC')
  date = as.character(format(dt, '%Y-%m-%d'))
  time = as.character(format(dt, '%H:%M'))
  paste(date, time, sep = ' ')
}

### temperature ----
#make list of files with TMP
temp_list = list.files(temp_dir)
temp_list = temp_list[grepl('TMP', temp_list)]

for(t in 1:length(temp_list)) {
  file = read.csv(file.path(temp_dir, temp_list[t]), skip = 39, col.names = 'date_data')
  file = file %>%
    mutate(data = map(date_data, data_fx),
           datetime = map(date_data, dt_fx),
           datetime = as.character(datetime),
           tempC = as.numeric(data)-273.15) %>% #convert from K
    select(datetime, tempC)
  write.csv(file, file.path(temp_dir, temp_list[t]), row.names = F)
}

### solar radiation ----
#make list of files with DSWRF
sr_list = list.files(temp_dir)
sr_list = sr_list[grepl('DSWRF', sr_list)]

for(s in 1:length(sr_list)) {
  file = read.csv(file.path(temp_dir, sr_list[s]), skip = 39, col.names = 'date_data')
  file = file %>%
    mutate(data = map(date_data, data_fx),
           datetime = map(date_data, dt_fx),
           datetime = as.character(datetime),
           solarRad_Wpm2 = as.numeric(data),
           solarRad_KJpm2 = solarRad_Wpm2 * 0.0036) %>% #to convert W/m2 to KJ/m2
    select(datetime, solarRad_KJpm2)
  write.csv(file, file.path(temp_dir, sr_list[s]), row.names = F)
}


### wind  ----
#make list of files with UGRD and VGRD 
wnd_list = list.files(temp_dir)
ulist = wnd_list[grepl('UGRD', wnd_list)]
vlist = wnd_list[grepl('VGRD', wnd_list)]

for(uv in 1:length(ulist)) { #these are the same length, so one will be fine
  file_u = read.csv(file.path(temp_dir, ulist[uv]), skip = 39, col.names = 'date_data')
  file_v = read.csv(file.path(temp_dir, vlist[uv]), skip = 39, col.names = 'date_data')
  file_u = file_u %>%
    mutate(data = map(date_data, data_fx),
           datetime = map(date_data, dt_fx),
           datetime = as.character(datetime),
           zon = as.numeric(data)) %>% 
    select(datetime, zon)
  file_v = file_v %>% 
    mutate(data = map(date_data, data_fx),
           datetime = map(date_data, dt_fx),
           datetime = as.character(datetime),
           mer = as.numeric(data)) %>% 
    select(datetime, mer)
  file_uv = full_join(file_u, file_v) %>% 
    mutate(wind_mps = sqrt(mer*mer + zon*zon)) %>% 
    select(datetime, wind_mps)
  fn = str_replace(ulist[uv], 'UGRD', 'WS') #make a new name for this
  write.csv(file_uv, file.path(temp_dir, fn), row.names = F)
}

### relative humidity  ----
#make list of files with SPFH
hum_list = list.files(temp_dir)
hum_list = hum_list[grepl('SPFH', hum_list)]

for(h in 1:length(hum_list)) { #we will need both humidity and temp
  file_h = read.csv(file.path(temp_dir, hum_list[h]), skip = 39, col.names = 'date_data')
  file_t = read.csv(file.path(temp_dir, temp_list[h])) %>% 
    mutate(datetime = as.character(datetime))
  file_h = file_h %>%
    mutate(data = map(date_data, data_fx),
           datetime = map(date_data, dt_fx),
           datetime = as.character(datetime),
           sp_hum = as.numeric(data)) %>% 
    select(datetime, sp_hum)
  file_h = full_join(file_h, file_t) %>% 
    mutate(rel_hum = map2(sp_hum, tempC, qair2rh),
           rel_hum = as.numeric(rel_hum)) %>% 
    select(datetime, rel_hum)
  fn = str_replace(hum_list[h], 'SPFH', 'RH') #make a new name for this
  write.csv(file_h, file.path(temp_dir, fn), row.names = F)
}

## upload to drive ----
upload_vars = c('WS10m','DSWRFsfc', 'RH2m')
#find root folder
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id
p_did = drive_ls(path =as_id(sd_id), pattern = 'Historical Daily')$id

#get list of all files
all_files  =list.files(temp_dir)

for(l in 1:length(lake_names)) {
  lake = lake_names[l]
  l_id = drive_ls(path = as_id(p_did), pattern = lake_names[l])
  lake_files = all_files[grepl(lake, all_files)]
  lake_files = lake_files[grepl(upload_vars[1], lake_files) | 
                            grepl(upload_vars[2], lake_files) |
                            grepl(upload_vars[3], lake_files)]
  for(f in 1:length(lake_files)){
    drive_upload(file.path(temp_dir, lake_files[f]), path = as_id(l_id))
  }
}

## unlink tmp folder
unlink('tmp', recursive = T)
