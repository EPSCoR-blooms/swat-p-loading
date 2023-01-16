# this script contains the functilidons used in the download_process_save script

SHAPE_LIST = function(lakename, lakeabb){
  lake_geo_id = drive_ls(as_id(geo_fid), pattern = tolower(lakename))$id
  lake_shape_content = drive_ls(as_id(lake_geo_id))
  print(lake_shape_content)
}

COUNT_CLIM = function(lakename) {
  #get the watershed folder
  lake_cmip_fid = drive_ls(as_id(lid), pattern = lakename)$id
  #get the climate folder
  lake_clim_fid = drive_ls(as_id(lake_cmip_fid), pattern = 'cli')$id
}

CLIM_LIST = function(lakename,num){
  lake_clim_loca = drive_ls(as_id(clim_fid[num]), pattern = 'loca5')
  #filter out the tarball and grab id
  lake_clim_loca_fid = (lake_clim_loca %>% filter(!grepl('tar', name)))$id
  loca_clim_content = drive_ls(as_id(lake_clim_loca_fid))
  loca_clim_content = loca_clim_content %>% filter(grepl('.nc', name))
}

REMOVE_EXT = function(ncfilename){
  substr(ncfilename, 1, nchar(ncfilename) - 3)
}

GET_VARNAME = function(ncfilename) {
  dimensions <- tidync(file.path(tmp_dir, ncfilename))$variable$name
  varname <- dimensions[!grepl('lat', dimensions, ignore.case = T) &
               !grepl('lon', dimensions, ignore.case = T) &
               !grepl('time', dimensions, ignore.case = T)]
}

COUNT_HYDRO = function(lakename) {
  #get the watershed folder
  lake_cmip_fid = drive_ls(as_id(lid), pattern = lakename)$id
  #get the climate folder
  lake_hyd_fid = drive_ls(as_id(lake_cmip_fid), pattern = 'hyd')$id
}

HYDRO_LIST = function(lakename,num){
  #get the loca folder
  lake_hyd_loca = drive_ls(as_id(hyd_fid[num]), pattern = 'loca_hydro5')
  #filter out the tarball and grab id
  lake_hyd_loca_fid = (lake_hyd_loca %>% filter(!grepl('tar', name)))$id
  loca_hyd_content = drive_ls(as_id(lake_hyd_loca_fid))
  loca_hyd_content = loca_hyd_content %>% filter(grepl('.nc', name))
}

# LOCA Hydrology archive - daily data
# rainfall        - Rainfall Rate (mm/day)
