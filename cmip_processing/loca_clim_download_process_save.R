#script to process CMIP clim files

# grab loca climate files ----
message('Starting CLIM extraction for ', lake_list$LakeName[l])

dir.create('toupload')

clim_fid <- COUNT_CLIM(lake_list$LakeName[l])

dr_down = function(dr_id, dr_name){
  drive_download(dr_id, 
                 path = file.path(tmp_dir, dr_name),
                 overwrite = T)
}

if(length(clim_fid) == 1) {

  clim_list <- CLIM_LIST(lake_list$LakeName[l], 1)
  
  #download them from drive
  map2(clim_list$id, clim_list$name, dr_down)
    
} else {
  
  for(cf in 1:length(clim_fid)) {
    
    cf = cf
    clim_list <- CLIM_LIST(lake_list$LakeName[l], cf)
    
    #download them from drive
    map2(clim_list$id, clim_list$name, dr_down)
  }
}

#get a list of downloaded files 
netcdf_file <- list.files(tmp_dir)

datelist = seq.Date(as.Date('2021-01-01'), as.Date('2099-12-31'), by = 'day')

# extract all dates and all projections ----

for(n in 1:length(netcdf_file)){
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
  firstdate = datelist[1]
  lastdate = datelist[last]
  clim_dates = seq.Date(firstdate, lastdate, by = 'day') #create date indices
  
  #close nc file
  nc_close(data)
  
  for(p in 1:length(projection_list)){#for each projection
    message('Starting projection ', clim_proj[p])
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
        extract_all <- rbind(extract_all, r_extract)
      }
      if (d%%1000 == 0) { message(d) } 
    }
    
    extract_all$parameter = var
    extract_all$cmip_projection = projection_list[p]

    #write file, will make pretty later
    filename = paste0(lake_list$LakeName[l], '_loca_clim_', 'nc', n, '_', 'p', clim_proj[p], '_', Sys.Date(), '.csv')
    write.csv(extract_all, file.path('toupload', filename), row.names = F)
    message('Projection saved locally as ', filename)
    
  }
}



#remove net cdf files from tmp dir
unlink(file.path(tmp_dir, netcdf_file))
