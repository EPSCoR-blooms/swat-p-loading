#here, we use some of the architechrue of the download, process, extract to make sure we've got the right CMIP extent for all lakes

# run the top of the authorize_source.R script (through line 59, after the functions) to run this code.
for(l in 1:length(lake_list)){

  # grab shapefiles for watershed ----
  shape_list <- SHAPE_LIST(lake_list$LakeName[l], lake_list$LakeAbbreviation[l])
  
  #download them from drive
  for(i in 1:nrow(shape_list)){
    drive_download(shape_list$id[i], 
                   path = file.path(tmp_dir, shape_list$name[i]),
                   overwrite = T)
  }
  
  #create shape_name from metadata
  shape_name = paste0(tolower(lake_list$LakeName[l]), '.shp')
  
  # read in shp file and transform to WGS 84
  watershed <- st_read(file.path(tmp_dir, shape_name))

  watershed <- st_transform(watershed, 4326) #transform to WGS84
  
  #remove temp files
  unlink(file.path('temp', shape_list$name))
  
  # grab loca climate files ----
  
  clim_list <- CLIM_LIST(lake_list$LakeName[l])
  
  #download them from drive
  drive_download(clim_list$id[1], 
                 path = file.path(tmp_dir, clim_list$name[1]),
                 overwrite = T)

  tempmax <- clim_list %>% filter(grepl('max', name))

  data_max <- nc_open(file.path(tmp_dir, tempmax$name))

  #get indices
  t <- ncvar_get(data_max, 'time')
  lat <- ncvar_get(data_max, 'lat')
  lon <- ncvar_get(data_max, 'lon')
  lon = lon - 360 #convert to degrees
  
  #get array
  maxtemp.array <- ncvar_get(data_max, 'tasmax')
  
  #get na value
  fillvalue <- ncatt_get(data_max, "tasmax", "_FillValue")
  nc_close(data_max)
  
  #recode na values
  maxtemp.array[maxtemp.array == fillvalue$value] <- NA
  
  #grab a slice and check
  maxtemp.slice <- maxtemp.array[, , 50, 5]
  
  # dim(maxtemp.slice)
  
  r <- raster(t(maxtemp.slice), 
              xmn=min(lon), 
              xmx=max(lon), 
              ymn=min(lat), 
              ymx=max(lat),
              crs=4326) # reported as WGS84 deg
  # r <- flip(r, direction='y') #flip the coord for proper display
  # r <-  #rotate the coord for proper projection
  # plot(r)
  # crs(r)
  
  #reality check
  tm_shape(r) +
    tm_raster() +
    tm_shape(watershed) +
    tm_polygons() +
    tm_scale_bar() +
    tm_graticules()
  
  tmap_save(filename = file.path('test', paste0(lake_list$LakeName[l], 'clim.png')))  
  
  # grab loca hydro files ----
  
  hyd_list <- HYDRO_LIST(lake_list$LakeName[l])
  
  #download them from drive
  drive_download(hyd_list$id[1], 
                 path = file.path(tmp_dir, hyd_list$name[1]),
                 overwrite = T)
  
  rf <- hyd_list %>% filter(grepl('rain', name))
  
  data_rf <- nc_open(file.path(tmp_dir, rf$name))
  
  #get indices
  t <- ncvar_get(data_rf, 'Time')
  lat <- ncvar_get(data_rf, 'Lat')
  lon <- ncvar_get(data_rf, 'Lon')
  
  #get array
  rf.array <- ncvar_get(data_rf, 'rainfall')
  
  #get na value
  fillvalue <- ncatt_get(data_rf, "rainfall", "_FillValue")
  nc_close(data_rf)
  
  #recode na values
  rf.array[rf.array == fillvalue$value] <- NA
  
  #grab a slice and check
  rf.slice <- rf.array[, , 50, 5]
  
  dim(rf.slice)
  
  r <- raster(t(rf.slice), 
              xmn=min(lon), 
              xmx=max(lon), 
              ymn=min(lat), 
              ymx=max(lat),
              crs=4326) # reported as WGS84 deg
  # r <- flip(r, direction='y') #flip the coord for proper display
  # r <-  #rotate the coord for proper projection
  # plot(r)
  # crs(r)
  
  #reality check
  tm_shape(r) +
    tm_raster() +
    tm_shape(watershed) +
    tm_polygons() +
    tm_scale_bar() +
    tm_graticules()
  
  tmap_save(filename = file.path('test', paste0(lake_list$LakeName[l], '_hyd.png')))  
}  
  