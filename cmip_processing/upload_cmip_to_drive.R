#upload local .csvs to shared drive
upload <- list.files('toupload')

for(u in 1:length(upload)){
  lid = drive_ls(as_id(inter_id), pattern = lake_list$LakeName[l])
  drive_upload(upload[u], path = lid)
}

#delete folder of files to upload
unlink('toupload', recursive = T)