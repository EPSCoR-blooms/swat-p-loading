# script to grab shapefies for cmip processing

# grab shapefiles for watershed ----

shape_list <- SHAPE_LIST(lake_list$LakeName[l], lake_list$LakeAbbreviation[l])

#download them from drive
for(s in 1:nrow(shape_list)){
  drive_download(shape_list$id[s], 
                 path = file.path(tmp_dir, shape_list$name[s]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = paste0(tolower(lake_list$LakeName[l]), '.shp')

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))
watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files
unlink(file.path('temp', shape_list$name))
