# this script contains the functions used in the download_process_save script

SHAPE_LIST = function(lakename, lakeabb){
  lake_geo_id = drive_ls(as_id(geo_fid), pattern = tolower(lakename))$id
  lake_shape_content = drive_ls(as_id(lake_geo_id))
  print(lake_shape_content)
}

CLIM_LIST = function(lakename){
  #get the watershed folder
  lake_cmip_fid = drive_ls(as_id(fid), pattern = lakename)$id
  #get the climate folder
  lake_clim_fid = drive_ls(as_id(lake_cmip_fid), pattern = 'cli')$id
  #get the loca folder
  lake_clim_loca = drive_ls(as_id(lake_clim_fid), pattern = 'loca5')
  #filter out the tarball and grab id
  lake_clim_loca_fid = (lake_clim_loca %>% filter(!grepl('tar', name)))$id
  loca_clim_content = drive_ls(as_id(lake_clim_loca_fid))
  loca_clim_content = loca_clim_content %>% filter(grepl('.nc', name))
}

HYDRO_LIST = function(lakename){
  #get the watershed folder
  lake_cmip_fid = drive_ls(as_id(fid), pattern = lakename)$id
  #get the climate folder
  lake_hyd_fid = drive_ls(as_id(lake_cmip_fid), pattern = 'hyd')$id
  #get the loca folder
  lake_hyd_loca = drive_ls(as_id(lake_hyd_fid), pattern = 'loca_hydro5')
  #filter out the tarball and grab id
  lake_hyd_loca_fid = (lake_hyd_loca %>% filter(!grepl('tar', name)))$id
  loca_hyd_content = drive_ls(as_id(lake_hyd_loca_fid))
  loca_hyd_content = loca_hyd_content %>% filter(grepl('.nc', name))
}

