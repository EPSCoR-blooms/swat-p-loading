## make daily humidity table ----
# relHumid        - Relative Humidity (percent)

humid <- collated %>% 
  filter(parameter == 'relHumid')

humid_proj <- unique(humid$cmip_projection)

for(hp in 1:length(humid_proj)){
  try(dir.create(file.path('upload', humid_proj[hp])))
  
  df <- humid[humid$cmip_projection == humid_proj[hp],]
  
  df <- df %>% 
    mutate(rhd = round(weighted_mean/100, digits = 2))%>% 
    rename(`19760101` = rhd) 
  
  df <- df %>% 
    dplyr::select(`19760101`)
  
  write_delim(df, file.path('upload', humid_proj[hp], paste0(collate$LakeAbbreviation[c], 'hmd.txt')), delim = ',')
  
  u_id <- drive_ls(as_id(lake_upload_id), pattern = humid_proj[hp])$id
  
  drive_upload(file.path('upload', humid_proj[hp], paste0(collate$LakeAbbreviation[c], 'hmd.txt')),
               path = as_id(u_id),
               name = paste0(collate$LakeAbbreviation[c], 'hmd.txt'),
               overwrite = T)
}

message('Relative humidity files uploaded for ', collate$LakeName[c])

