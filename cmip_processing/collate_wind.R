## make daily wind table ----
# windspeed       - Wind Speed (m/s)

wind <- collated %>% 
  filter(parameter == 'windspeed')

wind_proj <- unique(wind$cmip_projection)

for(wp in 1:length(wind_proj)){
  try(dir.create(file.path('upload', temp_proj[tp])))
  
  df <- wind[wind$cmip_projection == wind_proj[wp],]
  
  df <- df %>% 
    mutate(weighted_mean = round(weighted_mean, digits = 1)) %>% 
    rename(`19760101` = weighted_mean) 
  
  df <- df %>% 
    dplyr::select(`19760101`)
  
  write_delim(df, file.path('upload', wind_proj[wp], paste0(collate$LakeAbbreviation[c], 'wnd.txt')), delim = ',')
  
  u_id <- drive_ls(as_id(lake_upload_id), pattern = wind_proj[wp])$id
  
  drive_upload(file.path('upload', wind_proj[wp], paste0(collate$LakeAbbreviation[c], 'wnd.txt')),
               path = as_id(u_id),
               name = paste0(collate$LakeAbbreviation[c], 'wnd.txt'),
               overwrite = T)
}

message('Wind files uploaded for ', collate$LakeName[c])
