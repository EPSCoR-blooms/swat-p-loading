#this script just checks a single layer of the NC file for each CMIP extraction and saves a plot.

#load libraries
library(tidyverse)
library(readxl)
library(ncdf4)
library(raster) 
# library(terra) 
library(sf)
# library(ggplot2)
library(googledrive)
library(tmap) #for sanity checks
# library(tidync)

# create temporary and export folders for Drive downloads/uploads ----
tmp_dir = 'temp2/'
dir.create(tmp_dir)

# navigate to Drive directories ----

#authorize google drive
drive_auth()
# you'll need to manually type in your account in order to move forward with the script

#point to shared drive
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id

#find the folder you're interested in 
info <- drive_ls(path = as_id(sd_id), pattern = '2100')
print(info) #to confirm

#store the id as fid
fid <- info$id

#grab the folder identity for the watershed shapefiles
geo_fid <- drive_ls(as_id(sd_id), pattern = 'Shape')$id

#grab the folder identity for the finalized delineations
folder_info <- drive_ls(as_id(fid), pattern = 'Final')
print(folder_info)
#filter out the pending files
lid <- folder_info$id

## GRAB THE METADATA FILE THAT CONTAINS THE LIST FOR PROCESSING ----
# metadata file just has LakeName and LakeAbbreviation; this is just to help with iteration later.

# get drive id
meta_id <- drive_ls(as_id(fid), pattern = 'Meta')$id

#save file locally
drive_download(meta_id, 
               path = file.path(tmp_dir, 'metadata.xlsx'))

#get list of sheets
sheets <- excel_sheets(file.path(tmp_dir, 'metadata.xlsx'))

read_sheet = function(sh){
  lake_list <- read_xlsx(file.path(tmp_dir, 'metadata.xlsx'),
                         sheet = sh) 
  lake_list$sheet = sh
  lake_list
}

all_lakes = map_dfr(sheets, read_sheet)

#remove local download
unlink(file.path(tmp_dir, 'metadata.xlsx'))


## SOURCE FUNCTION SCRIPT ----
source('cmip_functions.R')

all_lakes$LakeName

#download them from drive
dr_down = function(sh_id, sh_name){
  drive_download(sh_id,
                 path = file.path(tmp_dir, sh_name),
                                  overwrite = T)
}


## Auburn ----
#get shape list and download files locally
shape_list <- SHAPE_LIST('Auburn', 'aub')
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'auburn.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Auburn')

hydro_list <- HYDRO_LIST('Auburn', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)



## Barber ----
shape_list <- SHAPE_LIST('Barber', 'bar')
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'barber.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Barber')

clim_list <- CLIM_LIST('Barber', 1)

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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Barber')

hydro_list <- HYDRO_LIST('Barber', 1)

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
rf.array <- ncvar_get(rf, 'windspeed')

#get na value
fillvalue <- ncatt_get(rf, "windspeed", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)



## China ----
shape_list <- SHAPE_LIST('China', 'chi')
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'china.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('China')

clim_list <- CLIM_LIST('China', 1)

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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('China')

hydro_list <- HYDRO_LIST('China', 1)

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
rf.array <- ncvar_get(rf, 'relHumid')

#get na value
fillvalue <- ncatt_get(rf, "relHumid", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)




## Cranberry ----
shape_list <- SHAPE_LIST('Cranberry', 'cra')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'cranberry.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Cranberry')

clim_list <- CLIM_LIST('Cranberry', 1)

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

tmap_save(filename = file.path('test', 'Cranberry_clim.png'))

# clean up 
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Cranberry')

hydro_list <- HYDRO_LIST('Cranberry', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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

tmap_save(filename = file.path('test', 'Cranberry_hydro.png'))

# clean up 
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()



## Floods ----
shape_list <- SHAPE_LIST('Floods', 'fld')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'floods.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Floods')

clim_list <- CLIM_LIST('Floods', 1)

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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Floods')

hydro_list <- HYDRO_LIST('Floods', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




## Great ----
shape_list <- SHAPE_LIST('Great', 'grt')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'great.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Great')

clim_list <- CLIM_LIST('Great', 1)

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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Great')

hydro_list <- HYDRO_LIST('Great', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()





## Hadlock ----
shape_list <- SHAPE_LIST('Hadlock', 'had')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'hadlock.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Hadlock')

clim_list <- CLIM_LIST('Hadlock', 1)

#download them from drive
drive_download(clim_list$id[1], 
               path = file.path(tmp_dir, clim_list$name[1]),
               overwrite = T)

tempmin <- clim_list %>% filter(grepl('min', name))

data_min <- nc_open(file.path(tmp_dir, tempmin$name))

#get indices
t <- ncvar_get(data_min, 'time')
lat <- ncvar_get(data_min, 'lat')
lon <- ncvar_get(data_min, 'lon')
lon = lon - 360 #convert to degrees

#get array
mintemp.array <- ncvar_get(data_min, 'tasmin')

#get na value
fillvalue <- ncatt_get(data_min, "tasmin", "_FillValue")

# close netcdf
nc_close(data_min)

#recode na values
mintemp.array[mintemp.array == fillvalue$value] <- NA

#grab a slice and check
mintemp.slice <- mintemp.array[, , 50, 5]

r <- raster(t(mintemp.slice), 
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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_min, fillvalue, mintemp.slice, r, tempmin, lat, lon, t, mintemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Hadlock')

hydro_list <- HYDRO_LIST('Hadlock', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()





## Jordan ----
shape_list <- SHAPE_LIST('Jordan', 'jor')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'jordan.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Jordan')

clim_list <- CLIM_LIST('Jordan', 1)

#download them from drive
drive_download(clim_list$id[1], 
               path = file.path(tmp_dir, clim_list$name[1]),
               overwrite = T)

tempmin <- clim_list %>% filter(grepl('min', name))

data_min <- nc_open(file.path(tmp_dir, tempmin$name))

#get indices
t <- ncvar_get(data_min, 'time')
lat <- ncvar_get(data_min, 'lat')
lon <- ncvar_get(data_min, 'lon')
lon = lon - 360 #convert to degrees

#get array
mintemp.array <- ncvar_get(data_min, 'tasmin')

#get na value
fillvalue <- ncatt_get(data_min, "tasmin", "_FillValue")

# close netcdf
nc_close(data_min)

#recode na values
mintemp.array[mintemp.array == fillvalue$value] <- NA

#grab a slice and check
mintemp.slice <- mintemp.array[, , 50, 5]

r <- raster(t(mintemp.slice), 
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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_min, fillvalue, mintemp.slice, r, tempmin, lat, lon, t, mintemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Jordan')

hydro_list <- HYDRO_LIST('Jordan', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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

tmap_save(filename = file.path('test', 'Jordan_hydro.png'))

# clean up 
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




## Rangely ----
shape_list <- SHAPE_LIST('Rangeley', 'ran')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'rangeley.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Rangeley')

clim_list <- CLIM_LIST('Rangeley', 1)

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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Rangeley')

hydro_list <- HYDRO_LIST('Rangeley', 1)

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
rf.array <- ncvar_get(rf, 'windspeed')

#get na value
fillvalue <- ncatt_get(rf, "windspeed", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




## Sabattus ----
shape_list <- SHAPE_LIST('Sabattus', 'sab')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'sabattus.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Sabattus')

clim_list <- CLIM_LIST('Sabattus', 1)


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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Sabattus')

hydro_list <- HYDRO_LIST('Sabattus', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




## Sebago ----
shape_list <- SHAPE_LIST('Sebago', 'seb')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'sebago.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Sebago')

clim_list <- CLIM_LIST('Sebago', 1)


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

tmap_save(filename = file.path('test', 'Sebago_clim.png'))

# clean up 
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Sebago')

hydro_list <- HYDRO_LIST('Sebago', 1)

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
rf.array <- ncvar_get(rf, 'windspeed')

#get na value
fillvalue <- ncatt_get(rf, "windspeed", "_FillValue")

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

tmap_save(filename = file.path('test', 'Sebago_hydro.png'))

# clean up 
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




##  Sunapee ----
shape_list <- SHAPE_LIST('Sunapee', 'sun')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'sunapee.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Sunapee')

clim_list <- CLIM_LIST('Sunapee', 1)


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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Sunapee')

hydro_list <- HYDRO_LIST('Sunapee', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()




## Yawgoo ----
shape_list <- SHAPE_LIST('Yawgoo', 'yaw')
#download them from drive
map2(shape_list$id, shape_list$name, dr_down)

#create shape_name from metadata
shape_name = 'yawgoo.shp'

# read in shp file and transform to WGS 84
watershed <- st_read(file.path(tmp_dir, shape_name))

watershed <- st_transform(watershed, 4326) #transform to WGS84

#remove temp files/clean up
unlink(file.path('temp2', shape_list$name))
rm(shape_list, shape_name)

### grab loca climate files ----

clim_fid <- COUNT_CLIM('Yawgoo')

clim_list <- CLIM_LIST('Yawgoo', 1)


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
unlink(file.path('temp2', clim_list$name))
rm(clim_list, data_max, fillvalue, maxtemp.slice, r, tempmax, lat, lon, t, maxtemp.array)

### grab loca hydro files ----

hyd_fid <- COUNT_HYDRO('Yawgoo')

hydro_list <- HYDRO_LIST('Yawgoo', 1)

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
rf.array <- ncvar_get(rf, 'precip')

#get na value
fillvalue <- ncatt_get(rf, "precip", "_FillValue")

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
unlink(file.path('temp2', hydro_list$name))
rm(hydro_list, fillvalue, rf.slice, r, rf, lat, lon, t, rf.array)

gc()



## clean up ----
unlink('temp2', recursive = T)
  

  
  