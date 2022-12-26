# read in data from Drive

##### Load libraries #####

library(tidyverse)
library(googledrive)
library(readxl)
library(xlsx)
library(ggpubr)
library(lubridate)

##### Load in data from Google Drive #####

# collated annual and monthly data

tmpdir = 'tmp' #point to directories

if(!dir.exists(tmpdir)){dir.create(tmpdir)} #create temporary directory - this creates a directory in your working directory called ‘tmp’

drive_auth() # authorize googledrive to access your Google Drive -- follow browser instructions or the prompts in the console.

did = shared_drive_find('EPSCoR_SWAT')$id #find shared drive id

folder1_id = drive_ls(path = as_id(did), pattern = 'SWAT_collatedFiles')$id

folder2_id = drive_ls(path = as_id(folder1_id), pattern = 'collated')$id #get folder id

csv_files <- drive_ls(folder2_id, type = "csv")

#downloads all files in the "collated" folder into temporary folder in R
csv_files %>% 
  split(csv_files$id) %>% 
  walk(~drive_download(.$id, path = file.path("tmp", .$name), overwrite = TRUE))

#reads in all collated files
swat_collatedAnnual <- list.files(path="tmp/", pattern = "yr_collate", full.names = T) %>% 
  lapply(read.csv) %>% 
  bind_rows()

swat_collatedMonthly <- list.files(path="tmp/", pattern = "mon_collate", full.names = T) %>% 
  lapply(read.csv) %>% 
  bind_rows()

# make sure that climate model, rcp, and landcover year are all factors

swat_collatedAnnual$rcp <- as.factor(swat_collatedAnnual$rcp)
swat_collatedAnnual$climatemodel <- as.factor(swat_collatedAnnual$climatemodel)
swat_collatedAnnual$landcover_year <- as.factor(swat_collatedAnnual$landcover_year)
swat_collatedAnnual$lake <- as.factor(swat_collatedAnnual$lake)

# plotting function to show sediment P in by year, coded by climatemodel and wrapped by climatemodel

output_by_climatemodel <- function(data, lakeName, rcpDes, landcover_yr, output){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lakeName) %>% 
    filter(rcp == rcpDes) %>% 
    filter(landcover_year == landcover_yr)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = {{output}}, color = climatemodel, 
                                               shape = climatemodel), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = {{output}}, color = climatemodel,
                                                  fill = climatemodel), method = "loess", formula = y ~ x) +
    theme_minimal() +
    facet_wrap(~climatemodel)
  
  return(climatemodel_plot)
}

# plotting function to show sediment P in by year, coded by climatemodel and wrapped by rcp and landcover_year

output_by_rcp_landcover_year <- function(data, lakeName, output){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lakeName)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = {{output}}, color = climatemodel, 
                                             shape = climatemodel), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = {{output}}, color = climatemodel,
                                              fill = climatemodel), method = "loess", formula = y ~ x) +
    theme_minimal() +
    facet_wrap(rcp~landcover_year)
  
  return(climatemodel_plot)
}

# plotting function to show sediment P in by year, coded by rcp and wrapped by landcover_year

output_bylandcoveryear <- function(data, lakeName, climmod, output){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lakeName) %>% 
    filter(climatemodel == climmod)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = {{output}}, color = rcp, 
                                             shape = rcp), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = {{output}}, color = rcp,
                                              fill = rcp), method = "loess", formula = y ~ x) +
    theme_minimal() +
    facet_wrap(~landcover_year)
  
  return(climatemodel_plot)
}

# plotting function to show sediment P in by year, coded by landcover_year and wrapped by rcp

output_byrcp <- function(data, lakeName, climmod, output){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lakeName) %>% 
    filter(climatemodel == climmod)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = {{output}}, color = landcover_year, 
                                             shape = landcover_year), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = {{output}}, color = landcover_year,
                                              fill = landcover_year), method = "loess", formula = y ~ x) +
    theme_minimal() +
    facet_wrap(~rcp)
  
  return(climatemodel_plot)
}

# plotting function to show sediment P in by year, coded by rcp and wrapped by climatemodel

output_by_climatemodel_rcp <- function(data, lakeName, output){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lakeName)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = {{output}}, color = rcp, 
                                             shape = rcp), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = {{output}}, color = rcp,
                                              fill = rcp), method = "loess", formula = y ~ x) +
    theme_minimal() +
    facet_wrap(~climatemodel)
  
  return(climatemodel_plot)
}

# DELETE TMP FOLDER ----
unlink(tmpdir, recursive = T)

# commit and push changes!