
# grab shapefiles for watershed ----

shape_list <- SHAPE_LIST(lake_list$LakeName[1], lake_list$LakeAbbreviation[1])

#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = paste0(tolower(lake_list$LakeName[1]), '.shp')

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))
watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files
unlink(file.path('temp', shape_list$name))

# grab loca climate files ----

clim_list <- CLIM_LIST(lake_list$LakeName[1])

#download them from drive
for(i in 1:nrow(clim_list)){
  drive_download(clim_list$id[i], 
                 path = file.path(tmp_dir, clim_list$name[i]),
                 overwrite = T)
}

netcdf_file <- clim_list %>% filter(grepl('.nc', name))

tempmin <- netcdf_file %>% filter(grepl('min', name))
tempmax <- netcdf_file %>% filter(grepl('max', name))

data_min <- nc_open(file.path(tmp_dir, tempmin$name))
data_max <- nc_open(file.path(tmp_dir, tempmax$name))

print(data_min)
print(data_max)

#get indices
t <- ncvar_get(data_min, 'time')
lat <- ncvar_get(data_min, 'lat')
lon <- ncvar_get(data_min, 'lon')
lon = lon - 360 #convert to degrees

#get array
mintemp.array <- ncvar_get(data_min, 'tasmin')

#get na value
fillvalue <- ncatt_get(data_min, "tasmin", "_FillValue")
nc_close(data_min)

#recode na values
mintemp.array[mintemp.array == fillvalue$value] <- NA

ncatt_get(data_min, 0, 'Projections')

#grab a slice and check
mintemp.slice <- mintemp.array[, , 50, 5]

dim(mintemp.slice)

r <- raster(t(mintemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs=4326) # reported as WGS84 deg
# r <- flip(r, direction='y') #flip the coord for proper projection
r <-  #rotate the coord for proper projection
plot(r)
crs(r)

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

#grab the weighted mean value
r_extract <- exactextractr::exact_extract(r, floods_shp, 
                                          fun = c('weighted_mean', 'stdev'),
                                          weights = 'area')

#set up loop indices ----
#pr.array[long, lat, day, proj] <- format
cmip_days = seq(1, 1800, by =1) # 1800 days of data in each
cmip_mo_yr = format(seq.Date(as.Date('1950-01-01'), as.Date('2099-12-01'), by = 'month'), '%m-%Y') #create date indices
cmip_proj = c(5, 9) # pull hadgem-es.1.rcp45 and hadgem-es.1.rcp85
projection_list = c('hadgem2-es.1.rcp45', 'hadgem2-es.1.rcp85')
param_list <- c('pr', 'tas', 'tasmax', 'tasmin')

# extract data iteratively
#('cmip_projection', 'cmip_mo_yr', 'value', 'parameter')

for(h in 1:length(b_contents)){ #for each parameter, 
  if(h !=2) { #we don't need the tas.nc file, so skip that one
    #open file
    data <- nc_open(file.path(tmp, b_contents[h]))
    
    #get indices
    t <- ncvar_get(data, 'time')
    lat <- ncvar_get(data, 'latitude')
    lon <- ncvar_get(data, 'longitude')
    lon = lon - 360 #convert to degrees
    
    #get array
    pr.array <- ncvar_get(data, param_list[h])
    
    #get na value
    fillvalue <- ncatt_get(data, param_list[h], "_FillValue")
    
    #recode na values
    pr.array[pr.array == fillvalue$value] <- NA
    
    #close ncdf
    nc_close(data)
    
    for(i in 1:length(cmip_proj)){#for each projection
      
      for(j in 1:length(cmip_days)){#and for each day
        #grab a slice 
        pr.slice <- pr.array[, , cmip_days[j], cmip_proj[i]]
        
        r <- raster(t(pr.slice), 
                    xmn=min(lon), 
                    xmx=max(lon), 
                    ymn=min(lat), 
                    ymx=max(lat),
                    crs=4326) # reported as WGS84 deg
        r <- flip(r, direction='y') #flip the coord for proper projection
        
        #grab the weighted mean value
        r_extract <- exactextractr::exact_extract(r, floods_shp, 
                                                  fun = c('weighted_mean', 'stdev'),
                                                  weights = 'area')
        r_extract$parameter = param_list[h]
        r_extract$cmip_projection = projection_list[i]
        r_extract$mo_yr = cmip_mo_yr[j]
        
        if(h == 1 & i == 1 & j == 1) {
          extract_all <- r_extract
        } else {
          extract_all <- full_join(extract_all, r_extract)
        }
      }
    }
  }
  else {}
}

#convert to horizontal dataset
extract_horiz = extract_all %>% 
  pivot_wider(names_from = 'parameter', values_from = c('weighted_mean', 'stdev')) %>% 
  select(mo_yr, cmip_projection, weighted_mean_pr, stdev_pr, weighted_mean_tasmin, stdev_tasmin, weighted_mean_tasmax, stdev_tasmax) %>% 
  rename_at(vars(weighted_mean_pr, stdev_pr),
            ~ paste0(., '_mm')) %>% 
  rename_at(vars(weighted_mean_tasmin, stdev_tasmin),
            ~ paste0(., '_degC')) %>% 
  rename_at(vars(weighted_mean_tasmax, stdev_tasmax),
            ~ paste0(., '_degC')) %>% 
  mutate(month = format(as.Date(paste0('01-', mo_yr), format = '%d-%m-%Y'), '%m'),
         year = format(as.Date(paste0('01-', mo_yr), format = '%d-%m-%Y'), '%Y')) %>% 
  select(year, month, mo_yr:stdev_tasmax_degC)

write.csv(extract_horiz, 'floods_cmip_projections_v2022-08-22.csv', row.names = F)