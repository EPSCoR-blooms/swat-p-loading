library(tidyverse)

# mapped network drive(\\paris\etna\Scholarship\Holly Ewing\Students\Lars Gundersen) to Z:/
dir = 'Z:/SWAT_Runs_AllLakes'
dump = 'Z:/SWAT_collatedFiles'

#save output column names
colnames = c('jday', 'mon', 'day', 'yr', 'unit', 'gis_id',
             'name', 'area', 'precip', 'evap', 'seep', 
             'flo_stor', 'sed_stor', 'orgn_stor', 'sedp_stor', 
             'no3_stor', 'solp_stor', 'chla_stor', 'nh3_stor', 
             'no2_stor', 'cbod_stor', 'dox_stor', 'san_stor', 
             'sil_stor', 'cla_stor', 'sag_stor', 'lag_stor', 
             'grv_stor', 'null_stor', 
             'flo_in', 'sed_in', 'orgn_in', 
             'sedp_in', 'no3_in', 'solp_in', 'chla_in', 'nh3_in', 
             'no2_in', 'cbod_in', 'dox_in', 'san_in', 'sil_in', 
             'cla_in', 'sag_in', 'lag_in', 'grv_in', 'null_in', 
             'flo_out', 'sed_out', 'orgn_out', 'sedp_out', 'no3_out', 
             'solp_out', 'chla_out', 'nh3_out', 'no2_out', 'cbod_out', 
             'dox_out', 'san_out', 'sil_out', 'cla_out', 'sag_out', 
             'lag_out', 'grv_out', 'null_out')

# get lake list to iterate over
lakes = list.files(dir, recursive = F)
lakes = lakes[!grepl('.xlsx', lakes)]
lakes = lakes[!grepl('.docx', lakes)]
lakes


for(l in 11:length(lakes)){
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
            df = read.csv(file.path(output_path, file), skip = 2, col.names = colnames)
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
  output = output %>% 
    mutate(rcp = substr(climatemodel, nchar(climatemodel)-2, nchar(climatemodel)),
           climatemodel = substr(climatemodel, 1, nchar(climatemodel)-3))
  writename = paste0(lakes[l], '_basin_res_mon_collate.csv')
  write.csv(output, file.path(dump, writename), row.names = F)
}

