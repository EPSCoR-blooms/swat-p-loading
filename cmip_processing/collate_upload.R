# code to collate and prep files for SWAT analysis

message('Colalting and uploading files for ', lake_list$LakeName[l])

dir.create('upload')

cmip_files <- list.files(save_dir)

cmip_files <- cmip_files[grepl(lake_list$LakeName[l], cmip_files)] #make sure you're only grabbing lake of interest

# join files
for(f in 1:length(cmip_files)){
  df <- read.csv(file.path(export_dir, cmip_files[f]))
  if(f == 1){
    collated <- df
  } else {
    collated <- full_join(collated, df)
  }
}

#remove X column, if it exists (early processing for barber only)
if(length(colnames(collated)[grepl('X', colnames(collated))]) ==1){
  collated <- collated %>%
    dplyr::select(-X)
}

## make precip table ----
# precip - Precipitation (mm/day)

precip <- collated %>% 
  filter(parameter == 'precip')

pre_proj <- unique(precip$cmip_projection)

for(pp in 1:length(pre_proj)){
  df <- precip[precip$cmip_projection == pre_proj[pp],]
  df$title = paste(lake_list$LakeAbbreviation[l], 'precip', pre_proj[pp], sep = '; ')
  df$elevation = ''
  df$year = format(as.Date(df$date), '%Y')
  df$date = format(as.Date(df$date), '%j')
  df <- df %>% 
    rename(precipitation = weighted_mean)
  df <- df %>% 
    dplyr::select(title, elevation, year, date, precipitation)
  write.csv(df, file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_precip_', pre_proj[pp], '.csv')),
            row.names = F)
  ul_id <- drive_ls(as_id(upload_id), pattern = lake_list$LakeName[l])$id
  u_id <- drive_ls(as_id(ul_id), pattern = pre_proj[pp])$id
  drive_upload(file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_precip_', pre_proj[pp], '.csv')),
               path = as_id(u_id),
               name = paste0('daily_', lake_list$LakeAbbreviation[l], '_precip_', pre_proj[pp], '.csv'),
               overwrite = T)
}

message('Precip files uploaded for ', lake_list$LakeName[l])

## make daily temp table ----
#tasmin = minimum daily temp deg C
#tasmax = maximum daily temp deg C
temp <- collated %>% 
  filter(parameter == 'tasmin' | parameter == 'tasmax')

temp_proj <- unique(temp$cmip_projection)

for(tp in 1:length(temp_proj)){
  df <- temp[temp$cmip_projection == temp_proj[tp],]
  df$title = paste(lake_list$LakeAbbreviation[l], 'temp', temp_proj[tp], sep = '; ')
  df$elevation = ''
  df$year = format(as.Date(df$date), '%Y')
  df$date = format(as.Date(df$date), '%j')
  
  df <- df %>% 
    dplyr::select(-stdev) %>% 
    pivot_wider(names_from = parameter,
                values_from = weighted_mean) %>% 
    rename(max_temp = tasmax,
           min_temp = tasmin)

  df <- df %>% 
    dplyr::select(title, elevation, year, date, max_temp, min_temp)
  write.csv(df, file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_temp_', temp_proj[tp], '.csv')),
            row.names = F)
  ul_id <- drive_ls(as_id(upload_id), pattern = lake_list$LakeName[l])$id
  u_id <- drive_ls(as_id(ul_id), pattern = temp_proj[tp])$id
  drive_upload(file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_temp_', temp_proj[tp], '.csv')),
               path = as_id(u_id),
               name = paste0('daily_', lake_list$LakeAbbreviation[l], '_temp_', temp_proj[tp], '.csv'),
               overwrite = T)
}

message('Temperature files uploaded for ', lake_list$LakeName[l])

## make daily humidity table ----
# relHumid        - Relative Humidity (percent)

humid <- collated %>% 
  filter(parameter == 'relHumid')

humid_proj <- unique(humid$cmip_projection)

for(hp in 1:length(humid_proj)){
  df <- humid[humid$cmip_projection == humid_proj[hp],]
  df$title = paste(lake_list$LakeAbbreviation[l], 'humid', humid_proj[hp], sep = '; ')
  df$elevation = ''
  df$year = format(as.Date(df$date), '%Y')
  df$date = format(as.Date(df$date), '%j')
  
  df <- df %>% 
    rename(rhd = weighted_mean) %>% 
    mutate(rhd = rhd/100)
  
  df <- df %>% 
    dplyr::select(title, elevation, year, date, rhd)
  
  write.csv(df, file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_humid_', humid_proj[hp], '.csv')),
            row.names = F)
  ul_id <- drive_ls(as_id(upload_id), pattern = lake_list$LakeName[l])$id
  u_id <- drive_ls(as_id(ul_id), pattern = humid_proj[hp])$id
  drive_upload(file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_humid_', humid_proj[hp], '.csv')),
               path = as_id(u_id),
               name = paste0('daily_', lake_list$LakeAbbreviation[l], '_humid_', humid_proj[hp], '.csv'),
               overwrite = T)
}

message('Relative humidity files uploaded for ', lake_list$LakeName[l])

## make daily wind table ----
# windspeed       - Wind Speed (m/s)

wind <- collated %>% 
  filter(parameter == 'windspeed')

wind_proj <- unique(wind$cmip_projection)

for(wp in 1:length(wind_proj)){
  df <- wind[wind$cmip_projection == wind_proj[wp],]
  df$title = paste(lake_list$LakeAbbreviation[l], 'wind', wind_proj[wp], sep = '; ')
  df$elevation = ''
  df$year = format(as.Date(df$date), '%Y')
  df$date = format(as.Date(df$date), '%j')
  
  df <- df %>% 
    rename(wnd_sp = weighted_mean) 
  
  df <- df %>% 
    dplyr::select(title, elevation, year, date, wnd_sp)
  
  write.csv(df, file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_wind_', wind_proj[wp], '.csv')),
            row.names = F)
  ul_id <- drive_ls(as_id(upload_id), pattern = lake_list$LakeName[l])$id
  u_id <- drive_ls(as_id(ul_id), pattern = wind_proj[wp])$id
  drive_upload(file.path('upload', paste0(lake_list$LakeAbbreviation[l], '_wind_', wind_proj[wp], '.csv')),
               path = as_id(u_id),
               name = paste0('daily_', lake_list$LakeAbbreviation[l], '_wind_', wind_proj[wp], '.csv'),
               overwrite = T)
}

message('Wind files uploaded for ', lake_list$LakeName[l])

unlink('upload', recursive = T)
