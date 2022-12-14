#this script authorizes your google account and then sources a script that processes the CMIP netCDF files for use in SWAT future modeling. 

# IMPORTANT #####
#before running this script, make sure that either your 'temp' folder in this directory is deleted or empty!
#best practice is to run this script in a NEW R SESSION. 
####

#load libraries
library(tidyverse)
library(readxl)
library(ncdf4)
library(raster) 
library(terra) 
library(sf)
# library(ggplot2)
library(googledrive)
# library(tmap) #for sanity checks
library(exactextractr)
library(tidync)

## create temporary and export folders for Drive downloads/uploads ----
tmp_dir = 'temp/'
dir.create(tmp_dir)

## navigate to Drive directories ----

#authorize google drive
drive_auth()

##  IMPORTANT ####
## you'll need to manually type in your account in order to move forward with the script ##
####

#point to shared drive
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id

#find the folder you're interested in 
info <- drive_ls(path = as_id(sd_id), pattern = 'Daily Weather')
print(info) #to confirm

#store the id as fid
fid <- info$id

#grab the folder identity for the watershed shapefiles
geo_fid <- drive_ls(as_id(fid), pattern = 'Shape')$id

#grab the folder identity for the finalized delineations
folder_info <- drive_ls(as_id(fid), pattern = 'Final')
print(folder_info)
#filter out the pending files
pid <- (folder_info %>% filter(!grepl('Pending', name)))$id

## grap intermediary save location ----
inter_id <- drive_ls(as_id(sd_id), 'intermediary')$id


# GRAB THE METADATA FILE THAT CONTAINS THE LIST FOR PROCESSING ----
# metadata file just has LakeName and LakeAbbreviation; this is just to help with iteration later.

# get drive id
meta_id <- drive_ls(as_id(fid), pattern = 'Meta')$id

#save file locally
drive_download(meta_id, 
               path = file.path(tmp_dir, 'metadata.xlsx'))

#get list of sheets
sheets <- excel_sheets(file.path(tmp_dir, 'metadata.xlsx'))
#remove future sheets
sheets <- sheets[!grepl('future', sheets) & ! grepl('complete', sheets)]

for(i in 1:length(sheets)) {
  lake_list <- read_xlsx(file.path(tmp_dir, 'metadata.xlsx'),
                         sheet = sheets[i]) 
  lake_list$sheet = sheets[i]
  if(i == 1) {
    alllakes <- lake_list
  } else {
    alllakes <- full_join(alllakes, lake_list)
  }
}

# remove previously-processed lakes
complete <- read_xlsx(file.path(tmp_dir, 'metadata.xlsx'),
                      sheet = 'complete')
if(nrow(complete >0)) {
  complete_lakes = complete$LakeName
  drop_list <- alllakes %>% 
    filter(LakeName %in% complete_lakes)
  lake_list <- anti_join(alllakes, drop_list)
}


# IMPORTANT: ####
# double check the lake list here and make sure it's extracting the correct lakes before moving on. ##
####

#remove local download
unlink(file.path(tmp_dir, 'metadata.xlsx'))


## SOURCE FUNCTION SCRIPT ----
source('cmip_functions.R')

## SOURCE DOWNLOAD PROCESS SAVE SCRIPT ----

for(l in 1:nrow(lake_list)) {
  source('grab_watershed.R')
  source('loca_clim_download_process_save.R')
  source('loca_hyd_download_process_save.R')
  source('upload_cmip_to_drive.R')
}


## TIDY UP ----

#remove temporary folder and files
unlink('temp', recursive = T)

# suspend google authorization
drive_deauth()
