library(tidyverse)

# mapped network drive(\\paris\etna\Scholarship\Holly Ewing\Students\Lars Gundersen) to Z:/
dir = 'Z:/SWAT_Runs_AllLakes'
dump = 'Z:/SWAT_collatedFiles/collated/'

#save output column names
colnames_mon = c('jday', 'mon', 'day', 'yr', 'unit', 'gis_id',
             'name', 'area_ha', 'precip_ha-m', 'evap_ha-m', 'seep_ha-m', 
             'flo_stor_m3ps', 'sed_stor_tons', 'orgn_stor_kgN', 'sedp_stor_kgP', 
             'no3_stor_kgN', 'solp_stor_kgP', 'chla_stor_kg', 'nh3_stor_kgN', 
             'no2_stor_kgN', 'cbod_stor_kg', 'dox_stor_kg', 'san_stor_tons', 
             'sil_stor_tons', 'cla_stor_tons', 'sag_stor_tons', 'lag_stor_tons', 
             'grv_stor_tons', 'null_stor', 
             'flo_in_m3ps', 'sed_in_tons', 'orgn_in_kgN', 
             'sedp_in_kgP', 'no3_in_kgN', 'solp_in_kgP', 'chla_in_kg', 'nh3_in_kgN', 
             'no2_in_kgN', 'cbod_in_kg', 'dox_in_kg', 'san_in_tons', 'sil_in_tons', 
             'cla_in_tons', 'sag_in_tons', 'lag_in_tons', 'grv_in_tons', 'null_in', 
             'flo_out_m3ps', 'sed_out_tons', 'orgn_out_kgN', 'sedp_out_kgP', 'no3_out_kgN', 
             'solp_out_kgP', 'chla_out_kg', 'nh3_out_kgN', 'no2_out_kgN', 'cbod_out_kg', 
             'dox_out_kg', 'san_out_tons', 'sil_out_tons', 'cla_out_tons', 'sag_out_tons', 
             'lag_out_tons', 'grv_out_tons', 'null_out')
colnames_yr =c('jday', 'mon', 'day', 'yr', 'unit', 'gis_id', 'name', 
               'area_ha', 'precip_ha-m', 'evap_ha-m', 'seep_ha-m', 'flo_stor_m3ps', 
               'sed_stor_tons', 'orgn_stor_kgN', 'sedp_stor_kgP', 'no3_stor_kgN', 
               'solp_stor_kgP', 'chla_stor_kg', 'nh3_stor_kgN', 'no2_stor_kgN', 
               'cbod_stor_kg', 'dox_stor_kg', 'san_stor_tons', 'sil_stor_tons', 
               'cla_stor_tons', 'sag_stor_tons', 'lag_stor_tons', 'grv_stor_tons', 'null_stor',
               'flo_in_m3ps', 'sed_in_tons', 'orgn_in_kgN', 'sedp_in_kgP', 'no3_in_kgN',
               'solp_in_kgP', 'chla_in_kg', 'nh3_in_kgN', 'no2_in_kgN', 'cbod_in_kg', 
               'dox_in_kg', 'san_in_tons', 'sil_in_tons','cla_in_tons', 'sag_in_tons', 
               'lag_in_tons', 'grv_in_tons', 'null_in',
               'flo_out_m3ps', 'sed_out_tons', 'orgn_out_kgN', 'sedp_out_kgP', 'no3_out_kgN',
               'solp_out_kgP', 'chla_out_kg', 'nh3_out_kgN', 'no2_out_kgN', 
               'cbod_out_kg', 'dox_out_kg', 'san_out_tons', 'sil_out_tons', 'cla_out_tons',
               'sag_out_tons', 'lag_out_tons', 'grv_out_tons', 'null_out')

# get lake list to iterate over
lakes = list.files(dir, recursive = F)
lakes = lakes[!grepl('.xlsx', lakes)]
lakes = lakes[!grepl('.docx', lakes)]
lakes


#for monthly files
for(l in 1:length(lakes)){
  output = NULL
  #get list of years in lake folder
  years = list.files(file.path(dir, lakes[l]))
  for(y in 1:length(years)){
    #get list of models in lake folder
    models = list.files(file.path(dir, lakes[l], years[y]))
    for(m in 1:length(models)){
      #get lake-specific path of output files
      names = list.files(file.path(dir, lakes[l], years[y], models[m]))
      foldername = names[grepl('Delin', names)]
      #add reality check if the folder isn't there yet
      if(length(foldername) == 1){
        output_path = file.path(dir,lakes[l], years[y], models[m], foldername, 'Scenarios/Default/TxtInOut')
        #add reality check to see if folder path exists
        if(dir.exists(output_path)){
          file = list.files(output_path)[list.files(output_path) =='basin_res_mon.csv']
          if(length(file) == 1){
            df = read.csv(file.path(output_path, file), skip = 2, col.names = colnames_mon)
            df = df %>% 
              mutate(lake = lakes[l],
                     landcover_year = years[y],
                     climatemodel = models[m])
            if(y == 1 & m == 1){
              output = df
            } else {
              output = full_join(output, df)
            }
          }
        }
      }
    }
  }
  if(!is.null(nrow(output))){
    output = output %>% 
      mutate(rcp = substr(climatemodel, nchar(climatemodel)-2, nchar(climatemodel)),
             climatemodel = substr(climatemodel, 1, nchar(climatemodel)-3))
    writename = paste0(lakes[l], '_basin_res_mon_collate.csv')
    write.csv(output, file.path(dump, writename), row.names = F)
    rm(output)
  }
}

#for annual files
for(l in 1:length(lakes)){
  output = NULL
  #get list of years in lake folder
  years = list.files(file.path(dir, lakes[l]))
  for(y in 1:length(years)){
    #get list of models in lake folder
    models = list.files(file.path(dir, lakes[l], years[y]))
    for(m in 1:length(models)){
      #get lake-specific path of output files
      names = list.files(file.path(dir, lakes[l], years[y], models[m]))
      foldername = names[grepl('Delin', names)]
      #add reality check if the folder isn't there yet
      if(length(foldername) == 1){
        output_path = file.path(dir,lakes[l], years[y], models[m], foldername, 'Scenarios/Default/TxtInOut')
        #add reality check to see if folder path exists
        if(dir.exists(output_path)){
          file = list.files(output_path)[list.files(output_path) =='basin_res_yr.csv']
          if(length(file) == 1){
            df = read.csv(file.path(output_path, file), skip = 2, col.names = colnames_yr)
            df = df %>% 
              mutate(lake = lakes[l],
                     landcover_year = years[y],
                     climatemodel = models[m])
            if(y == 1 & m == 1){
              output = df
            } else {
              output = full_join(output, df)
            }
          }
        }
      }
    }
  }
  if(!is.null(nrow(output))){
    output = output %>% 
      mutate(rcp = substr(climatemodel, nchar(climatemodel)-2, nchar(climatemodel)),
             climatemodel = substr(climatemodel, 1, nchar(climatemodel)-3))
    writename = paste0(lakes[l], '_basin_res_mon_collate.csv')
    write.csv(output, file.path(dump, writename), row.names = F)
    rm(output)
  }
}
