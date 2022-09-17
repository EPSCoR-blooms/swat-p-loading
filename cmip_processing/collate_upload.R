# code to collate and prep files for SWAT analysis

cmip_files <- list.files(export_dir)

cmip_files <- cmip_files[grepl(lake_list$LakeName[l], cmip_files)] #make sure you're only grabbing lake of interest

# join files
for(f in 1:length(cmip_files)){
  df <- read.csv(file.path(export_dir, cmip_files[f]))
  if(f == 1){
    collated <- df
  } else {
    collated <- full_join(collated, df)
  }
}

#remove X column, if it exists (early processing for barber only)
if(length(colnames(collated)[grepl('X', colnames(collated))]) ==1){
  collated <- collated %>%
    dplyr::select(-X)
}

proj_list <- unique(collated$cmip_projection)
param_list <- unique(collated$parameter)
