
message('Downloading files for ', collate$LakeName[c])

folder <- drive_ls(path = as_id(inter_id), recursive = F) %>% 
  filter(name == collate$LakeName[c])

down_id <- folder$id

files <- drive_ls(as_id(down_id), pattern = '.csv') 

for(f in 1:nrow(files)){
  drive_download(as_id(files$id[f]), path = file.path('download', files$name[f]), overwrite = T)
}

cmip_files <- list.files('download')

# join files
for(cf in 1:length(cmip_files)){
  df <- read.csv(file.path('download', cmip_files[cf]))
  if(cf == 1){
    collated <- df
  } else {
    collated <- full_join(collated, df)
  }
}


## create directories for all the models ----
projection_list=unique(collated$cmip_projection)

for(pl in 1:length(projection_list)) {
  drive_mkdir(name = projection_list[pl], path = as_id(lake_upload_id), overwrite = T)
}
