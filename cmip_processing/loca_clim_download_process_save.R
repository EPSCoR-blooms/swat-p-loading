
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

# grab loca climate files ----

clim_fid <- COUNT_CLIM(lake_list$LakeName[l])

if(length(clim_fid) == 1) {

  clim_list <- CLIM_LIST(lake_list$LakeName[l], 1)
  
  #download them from drive
  for(c in 1:nrow(clim_list)){
    drive_download(clim_list$id[c], 
                   path = file.path(tmp_dir, clim_list$name[c]),
                   overwrite = T)
  }
  
} else {
  
  for(cf in 1:length(clim_fid)) {
    
    cf = cf
    clim_list <- CLIM_LIST(lake_list$LakeName[l], cf)
    
    #download them from drive
    for(c in 1:nrow(clim_list)){
      drive_download(clim_list$id[c], 
                     path = file.path(tmp_dir, paste0(REMOVE_EXT(clim_list$name[c]), '_', cf, '.nc')),
                     overwrite = T)
    }
  }
}

#get a list of downloaded files 
netcdf_file <- list.files(tmp_dir)

datelist = seq.Date(as.Date('1900-01-01'), as.Date('2051-01-01'), by = 'day')

# extract all dates and all projections ----

for(n in 1:length(netcdf_file)) {
  message('Starting ', netcdf_file[n])
  var <- GET_VARNAME(netcdf_file[n])
  
  data <- nc_open(file.path(tmp_dir, netcdf_file[n]))
  
  # [long, lat, day, proj] <- format
  
  #get indices
  t_c <- ncvar_get(data, 'time')
  lat_c <- ncvar_get(data, 'lat')
  lon_c <- ncvar_get(data, 'lon')
  lon_c = lon_c - 360 #convert to degrees
  
  #get array
  data.array <- ncvar_get(data, var)
  
  #get na value
  fillvalue <- ncatt_get(data, var, "_FillValue")
  
  #recode na values
  data.array[data.array == fillvalue$value] <- NA
  
  #get projection info
  projection_list = ncatt_get(data, varid = 0)$Projections
  projection_list = unlist(str_split(projection_list, ', ')) #split by comma
  projection_list <- projection_list[1:(length(projection_list)-1)] # remove final blank column
  clim_proj = seq(1, length(projection_list)) #get number of projections
  
  #get days info
  
  clim_days = seq(1:length(t_c))
  last = as.numeric(length(t_c))
  firstdate = datelist[t_c[2]]
  lastdate = datelist[(as.numeric(t_c[1])+last)]
  clim_dates = seq.Date(firstdate, lastdate, by = 'day') #create date indices
  
  #close nc file
  nc_close(data)
  
  for(p in 1:length(projection_list)){#for each projection
    message('Starting projection ', projection_list[p])
    for(d in 1:length(clim_days)){#and for each day
      #grab a slice 
      data.slice <- data.array[, , clim_days[d], clim_proj[p]]
      
      r <- raster(t(data.slice), 
                  xmn=min(lon_c), 
                  xmx=max(lon_c), 
                  ymn=min(lat_c), 
                  ymx=max(lat_c),
                  crs='+init=EPSG:4326') # reported as WGS84 deg
      r <- flip(r, direction='y') #flip the coord for proper projection
      
      #grab the weighted mean value
      r_extract <- exactextractr::exact_extract(r, watershed, 
                                                fun = c('weighted_mean', 'stdev'),
                                                weights = 'area')
      r_extract$date = clim_dates[d]
      
      if(d == 1) {
        extract_all <- r_extract
      } else {
        extract_all <- full_join(extract_all, r_extract)
      }
    }
    extract_all$parameter = var
    extract_all$cmip_projection = projection_list[p]
    r_extract$date = clim_dates[d]
    
    #write file, will make pretty later
    filename = paste0(lake_list$LakeName[l], '_loca_clim_', 'nc', n, '_', 'p', clim_proj[p], '_', Sys.Date(), '.csv')
    write.csv(extract_all, file.path('export', filename), row.names = F)
  }
}

 
  
