
message('Downloading files for ', lake_names[l])

down_id <- lake_upload_id

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

