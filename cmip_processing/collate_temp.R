## make daily temp table ----
#tasmin = minimum daily temp deg C
#tasmax = maximum daily temp deg C
temp <- collated %>% 
  filter(parameter == 'tasmin' | parameter == 'tasmax')

temp_proj <- unique(temp$cmip_projection)

for(tp in 1:length(temp_proj)){
  try(dir.create(file.path('upload', temp_proj[tp])))
  df <- temp[temp$cmip_projection == temp_proj[tp],]
  
  df <- df %>% 
    dplyr::select(-stdev) %>% 
    pivot_wider(names_from = parameter,
                values_from = weighted_mean) %>% 
    select(tasmax, tasmin) %>% 
    rename(`19760101` = tasmax,
           ` ` = tasmin)
  
  #round value
  df <- df %>% 
    mutate_at(vars(`19760101`, ` `),
              ~ round(., digits = 1))

  write_delim(df, file.path('upload', temp_proj[tp], paste0(collate$LakeAbbreviation[c], 'tmp.txt')), delim = ',')
  
  u_id <- drive_ls(as_id(lake_upload_id), pattern = temp_proj[tp])$id

  drive_upload(file.path('upload', temp_proj[tp], paste0(collate$LakeAbbreviation[c], 'tmp.txt')),
             path = as_id(u_id),
             name = paste0(collate$LakeAbbreviation[c], 'tmp.txt'),
             overwrite = T)
}

message('Temperature files uploaded for ', collate$LakeName[c])

