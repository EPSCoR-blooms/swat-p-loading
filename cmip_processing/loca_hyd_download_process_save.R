# script to download, process and save loca hydro data

# grab loca hydrology files ----
message('Starting HYD extraction for ', lake_list$LakeName[l])
hyd_fid <- COUNT_HYDRO(lake_list$LakeName[l])

dr_down = function(dr_id, dr_name){
  drive_download(dr_id, 
                 path = file.path(tmp_dir, dr_name),
                 overwrite = T)
}

if(length(hyd_fid) == 1) {
  
  hyd_list <- HYDRO_LIST(lake_list$LakeName[l], 1)
  
  #download them from drive
  map2(hyd_list$id, hyd_list$name, dr_down)
  
} else {
  
  for(cf in 1:length(hyd_fid)) {
    
    cf = cf
    hyd_list <- HYDRO_LIST(lake_list$LakeName[l], cf)
    
    #download them from drive
    map2(hyd_list$id, hyd_list$name, dr_down)
  }
}

#get a list of downloaded files 
netcdf_file <- list.files(tmp_dir)

#remove rainfall rate files
netcdf_file <- netcdf_file[!grepl('rainfall', netcdf_file)]

datelist = seq.Date(as.Date('2021-01-01'), as.Date('2099-12-31'), by = 'day')


proc_days_hyd = function(hyd_days, hyd_dates){
  #grab a slice 
  data.slice <- data.array[, , hyd_days, h_proj]
  
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
  r_extract$date = hyd_dates
  
  r_extract  
}

for(n in 1:length(netcdf_file)) {
  message('Starting ', netcdf_file[n])
  var <- GET_VARNAME(netcdf_file[n])
  
  data <- nc_open(file.path(tmp_dir, netcdf_file[n]))
  
  #get indices
  t_c <- ncvar_get(data, 'Time')
  lat_c <- ncvar_get(data, 'Lat')
  lon_c <- ncvar_get(data, 'Lon')
  
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
  hyd_proj = seq(1, length(projection_list)) #get number of projections
  
  #get days info
  
  hyd_days = seq(1:length(t_c))
  last = as.numeric(length(t_c))
  firstdate = datelist[1]
  lastdate = datelist[last]
  hyd_dates = seq.Date(firstdate, lastdate, by = 'day') #create date indices
  
  #close nc file
  nc_close(data)

  for(p in 1:length(hyd_proj)){
    message('Starting projection ', hyd_proj[p])
    for(d in 1:length(hyd_days)){#and for each day
      #grab a slice 
      data.slice <- data.array[, , hyd_days[d], hyd_proj[p]]
      
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
      r_extract$date = hyd_dates[d]
      
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
    filename = paste0(lake_list$LakeName[l], '_loca_hyd_', 'nc', n, '_', 'p', hyd_proj[p], '_', Sys.Date(), '.csv')
    write.csv(extract_all, file.path('toupload', filename), row.names = F)
    message('Projection saved locally as ', filename)
  }

}

#remove net cdf files from tmp dir
unlink(file.path(tmp_dir, list.files(tmp_dir)))
