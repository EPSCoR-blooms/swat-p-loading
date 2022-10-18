## make precip table ----
# precip - Precipitation (mm/day)

precip <- collated %>% 
  filter(parameter == 'precip')

pre_proj <- unique(precip$cmip_projection)


for(pp in 1:length(pre_proj)){
  try(dir.create(file.path('upload', pre_proj[pp])))
  df <- precip[precip$cmip_projection == pre_proj[pp],]
  df <- df %>% 
    rename(`19760101` = weighted_mean)
  df <- df %>% 
    dplyr::select(`19760101`)
  #round value
  df <- df %>% 
    mutate(`19760101` = round(`19760101`, digits = 1))
  write_delim(df, file.path('upload', pre_proj[pp], paste0(collate$LakeAbbreviation[c], 'pcp.txt')), delim = ',')
  
  u_id <- drive_ls(as_id(lake_upload_id), pattern = pre_proj[pp])$id
  
  drive_upload(file.path('upload', pre_proj[pp], paste0(collate$LakeAbbreviation[c], 'pcp.txt')),
               path = as_id(u_id),
               name = paste0(collate$LakeAbbreviation[c], 'pcp.txt'),
               overwrite = T)
}

message('Precip files uploaded for ', collate$LakeName[c])

