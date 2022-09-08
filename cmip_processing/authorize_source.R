#this script authorizes your google account and then sources a script that processes the CMIP netCDF files for use in SWAT future modeling. 

#load libraries
library(tidyverse)
library(readxl)
library(ncdf4)
library(raster) 
library(terra) 
library(sf)
library(ggplot2)
library(googledrive)
library(tmap) #for sanity checks
library(exactextractr)

# create temporary folder for Drive downloads ----
tmp_dir = 'temp/'
dir.create(tmp_dir)

# navigate to Drive directories ----

#authorize google drive
drive_auth()
# you'll need to manually type in your account in order to move forward with the script

#find the folder you're interested in 
info <- drive_find(pattern = 'Daily Weather',type = 'folder')
print(info) #to confirm

#store the id as did
did <- info$id


#grab the folder identity for the watershed shapefiles
geo_fid <- drive_ls(as_id(did), pattern = 'Shape')$id

#grab the folder identity for the finalized delineations
folder_info <- drive_ls(as_id(did), pattern = 'Final')
print(folder_info)
#filter out the pending files
fid <- (folder_info %>% filter(!grepl('Pending', name)))$id


## GRAB THE METADATA FILE THAT CONTAINS THE LIST FOR PROCESSING ----
# metadata file just has LakeName and LakeAbbreviation; this is just to help with iteration later.

# get drive id
meta_id <- drive_ls(as_id(did), pattern = 'Meta')$id

#save file locally
drive_download(meta_id, 
               path = file.path(tmp_dir, 'metadata.xlsx'))

#read into r
lake_list <- read_xlsx(file.path(tmp_dir, 'metadata.xlsx'),
                       sheet = '20220908') #if you're running a different handful of lakes, you'll need to change this.
unlink(file.path(tmp_dir, 'metadata.xlsx'))

## SOURCE FUNCTION SCRIPT ----
source('cmip_functions.R')

## SOURCE DOWNLOAD PROCESS SAVE SCRIPT ----

## TIDY UP ----

#remove temporary folder and files
unlink('temp', recursive = T)

# suspend google authorization
drive_deauth()
