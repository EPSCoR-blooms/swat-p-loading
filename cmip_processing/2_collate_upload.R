# code to collate and prep files for SWAT analysis

#load libraries
library(tidyverse)
library(googledrive)
library(readxl)


## create temporary and export folders for Drive downloads/uploads ----
tmp_dir = 'temp/'
dir.create(tmp_dir)


# authorize google drive ----
drive_auth()

##  IMPORTANT ####
## you'll need to manually type in your account in order to move forward with the script ##
####

#point to shared drive
sd_id <- shared_drive_find(pattern = 'EPSCoR_SWAT')$id

#find the folder you're interested in 
fid <- drive_ls(path = as_id(sd_id), pattern = 'Daily Weather')$id

## grab upload file location ----
upload_id = drive_ls(as_id(fid), 'extracted')$id

## grap intermediary save location ----
inter_id <- drive_ls(as_id(sd_id), 'intermediary')$id

## grab metadata lists ----
#grab the folder identity for the finalized delineations
folder_info <- drive_ls(as_id(fid), pattern = 'Final')
print(folder_info)
#filter out the pending files
pid <- (folder_info %>% filter(!grepl('Pending', name)))$id


## download to local temp folder ----

# metadata file just has LakeName and LakeAbbreviation; this is just to help with iteration later.

# get drive id
meta_id <- drive_ls(as_id(fid), pattern = 'Meta')$id

#save file locally
drive_download(meta_id, 
               path = file.path(tmp_dir, 'metadata.xlsx'),
               overwrite = T)

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

# remove select for extracted, but no previously-collated data
collate <- alllakes %>% 
  filter(extracted == 'x' & is.na(collated))

#remove metadata file
unlink('temp/metadata.xlsx')

#source scripts ----

for(c in 1:nrow(collate)){
  dir.create('download')
  dir.create('upload')
  drive_mkdir(name = collate$LakeName[c], path = as_id(upload_id), overwrite = T)
  lake_upload_id = drive_ls(as_id(upload_id), pattern = collate$LakeName[c])$id
  source('download_from_drive.R')
  source('collate_precip.R')
  source('collate_temp.R')
  source('collate_relhum.R')
  source('collate_wind.R')
  unlink('upload', recursive = T)
  unlink('download', recursive = T)
}
