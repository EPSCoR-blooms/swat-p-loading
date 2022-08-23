#this script processes the CMIP netCDF files for use in SWAT future modeling. 

#load libraries
library(ncdf4)
library(raster) 
library(terra) 
library(sf)
library(ggplot2)
library(googledrive)
library(tidyverse)
library(tmap) #for sanity checks
library(exactextractr)



#navigate to Drive directory ----

#authorize google drive
drive_auth()

#find the folder you're interested in
did <- drive_find(pattern = 'Future Weather Data',type = 'folder')$id

#find subfolder with gdb
floods_shape = drive_ls(as_id(did), pattern = '.gdb')$id
floods_shape = drive_ls(as_id(floods_shape), pattern = '.gdb')$id
floods_shape_content = drive_ls(as_id(floods_shape))$id
floods_shape_name = drive_ls(as_id(floods_shape))$name

#find the sub folder of interest with cmip data
floods <- drive_ls(as_id(did), pattern = 'Floods Weather')$id

#find the folders with netcdf files
floods_bcsd5 <- drive_ls(path = as_id(floods), pattern = 'bcsd5')$id
floods_bcsd5_name <- drive_ls(path = as_id(floods), pattern = 'bcsd5')$name
floods_bcsd5 <- floods_bcsd5[1]

floods_b_files <- drive_ls(path = as_id(floods_bcsd5), pattern = '.nc')$id
floods_b_names <- drive_ls(path = as_id(floods_bcsd5), pattern = '.nc')$name

#grab netCDFs and store in temp file ----
#save file to tmp folder
tmp = 'R.tmp'
dir.create(tmp)

#download upstream file to tmp folder
for(i in 1:length(floods_b_files)){
  drive_download(as_id(floods_b_files[i]),
                 path = file.path(tmp, paste0('cmip_', floods_b_names[i])),
                 overwrite = T)
}

#this isn't working because of a file lock. Manually downloading into this tmpfile location.
# #create the folder for the gdb files
# dir.create(file.path(tmp, 'floods_gdb'))
# 
# for(j in 1:length(floods_shape_content)){
#   drive_download(as_id(floods_shape),
#                  path = file.path(tmp, 'floods.gdb/', floods_shape_name[j]))
# }

# read in gdb shapefile ----

floods_shp <- st_read(file.path(tmp, 'floods.shp'))

floods_shp <- st_as_sf(floods_shp)

floods_shp <- st_transform(floods_shp, 4326) #transform to WGS84

#do a quick reality check ----
contents <- list.files(tmp)
b_contents = contents[grepl('cmip', contents)]

data <- nc_open(file.path(tmp, b_contents[1]))
print(data)

#get indices
t <- ncvar_get(data, 'time')
lat <- ncvar_get(data, 'latitude')
lon <- ncvar_get(data, 'longitude')
lon = lon - 360 #convert to degrees

#get array
pr.array <- ncvar_get(data, 'pr')

#get na value
fillvalue <- ncatt_get(data, "pr", "_FillValue")
nc_close(data)

#recode na values
pr.array[pr.array == fillvalue$value] <- NA

#grab a slice and check
pr.slice <- pr.array[, , 50, 5]

dim(pr.slice)

r <- raster(t(pr.slice), 
            xmn=min(lon), 
            xmx=max(lon), 
            ymn=min(lat), 
            ymx=max(lat),
            crs=4326) # reported as WGS84 deg
r <- flip(r, direction='y') #flip the coord for proper projection
plot(r)
crs(r)

#reality check
tm_shape(r) +
  tm_raster() +
  tm_shape(floods_shp) +
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

## TIDY UP ----

#remove temporary folder and files
unlink(tmp, recursive = T)

# suspend google authorization
drive_deauth()
