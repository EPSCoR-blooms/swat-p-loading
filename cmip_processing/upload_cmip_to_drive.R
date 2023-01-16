#upload local .csvs to shared drive
upload <- list.files('toupload')

dr_upload = function(lakeName, f_upload){
  lid <- drive_ls(as_id(inter_id), pattern = lakeName)
  if(nrow(lid) == 0){
    drive_mkdir(lakeName, path = as_id(inter_id))
    lid = drive_ls(as_id(inter_id), pattern = lakeName)
  } else {}
  drive_upload(file.path('toupload',f_upload), path = lid$id, overwrite = T)
}

map2(lake_list$LakeName[l], upload)


# for(u in 1:length(upload)){
#   lid <- drive_ls(as_id(inter_id), pattern = lake_list$LakeName[l])
#   if(nrow(lid) == 0){
#     drive_mkdir(lake_list$LakeName[l], path = as_id(inter_id))
#     lid = drive_ls(as_id(inter_id), pattern = lake_list$LakeName[l])
#     } else {}
#   drive_upload(file.path('toupload',upload[u]), path = lid$id, overwrite = T)
# }

#delete folder of files to upload
unlink('toupload', recursive = T)
