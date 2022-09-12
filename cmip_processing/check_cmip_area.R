
#this script just checks a single layer of the NC file for each CMIP extraction and saves a plot.

lake_list$LakeName

## Auburn ----
shape_list <- SHAPE_LIST('Auburn', 'aub')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'auburn.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Auburn')

clim_list <- CLIM_LIST('Auburn', 1)

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Auburn_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Auburn')

#download them from drive
drive_download(hydro_list$id[3], 
               path = file.path(tmp_dir, hydro_list$name[3]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[3]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Auburn_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)



## Barber ----
shape_list <- SHAPE_LIST('Barber', 'bar')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'barber.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Barber')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Barber_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Barber')

#download them from drive
drive_download(hydro_list$id[3], 
               path = file.path(tmp_dir, hydro_list$name[3]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[3]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Barber_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

## China ----
shape_list <- SHAPE_LIST('China', 'chi')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'china.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('China')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'china_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('China')

#download them from drive
drive_download(hydro_list$id[1], 
               path = file.path(tmp_dir, hydro_list$name[1]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[1]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'China_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Floods ----
shape_list <- SHAPE_LIST('Floods', 'fld')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'floods.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Floods')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Floods_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Floods')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Floods_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

## Great ----
shape_list <- SHAPE_LIST('Great', 'grt')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'great.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Great')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Great_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Great')

#download them from drive
drive_download(hydro_list$id[3], 
               path = file.path(tmp_dir, hydro_list$name[3]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[3]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Great_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Hadlock ----
shape_list <- SHAPE_LIST('Hadlock', 'had')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'hadlock.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Hadlock')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Hadlock_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Hadlock')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Hadlock_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Jordan ----
shape_list <- SHAPE_LIST('Jordan', 'jor')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'jordan.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Jordan')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Jordan_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Jordan')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Jordan.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Rangely ----
shape_list <- SHAPE_LIST('Rangeley', 'ran')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'rangeley.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Rangeley')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Rangeley_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Rangeley')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Rangeley_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Sabattus ----
shape_list <- SHAPE_LIST('Sabattus', 'sab')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'sabattus.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Sabattus')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Sabattus_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Sabattus')

#download them from drive
drive_download(hydro_list$id[2], 
               path = file.path(tmp_dir, hydro_list$name[2]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[2]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Sabattus_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


##  Sunapee ----
shape_list <- SHAPE_LIST('Sunapee', 'sun')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'sunapee.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Sunapee')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Sunapee_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Sunapee')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Sunapee_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)


## Yawgoo ----
shape_list <- SHAPE_LIST('Yawgoo', 'yaw')
#download them from drive
for(i in 1:nrow(shape_list)){
  drive_download(shape_list$id[i], 
                 path = file.path(tmp_dir, shape_list$name[i]),
                 overwrite = T)
}

#create shape_name from metadata
shape_name = 'yawgoo.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_list <- CLIM_LIST('Yawgoo')

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

# close netcdf
nc_close(data_max)

#recode na values
maxtemp.array[maxtemp.array == fillvalue$value] <- NA

#grab a slice and check
maxtemp.slice <- maxtemp.array[, , 50, 5]

r <- raster(t(maxtemp.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Yawgoo_clim.png'))

# clean up 
unlink(file.path('temp', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hydro_list <- HYDRO_LIST('Yawgoo')

#download them from drive
drive_download(hydro_list$id[4], 
               path = file.path(tmp_dir, hydro_list$name[4]),
               overwrite = T)


rf <- nc_open(file.path(tmp_dir, hydro_list$name[4]))

#get indices
t <- ncvar_get(rf, 'Time')
lat <- ncvar_get(rf, 'Lat')
lon <- ncvar_get(rf, 'Lon')

#get array
rf.array <- ncvar_get(rf, 'rainfall')

#get na value
fillvalue <- ncatt_get(rf, "rainfall", "_FillValue")

# close netcdf
nc_close(rf)

#recode na values
rf.array[rf.array == fillvalue$value] <- NA

#grab a slice and check
rf.slice <- rf.array[, , 50, 5]

r <- raster(t(rf.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs='+init=EPSG:4326') # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper display

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(watershed) +
  tm_polygons() +
  tm_scale_bar() +
  tm_graticules()

tmap_save(filename = file.path('test', 'Yawgoo_hydro.png'))

# clean up 
unlink(file.path('temp', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

  

  
  