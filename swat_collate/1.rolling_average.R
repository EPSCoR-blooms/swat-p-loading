library(tidyverse)
library(zoo)

# mapped network drive(\\paris\etna\Scholarship\Holly Ewing\Students\Lars Gundersen) to Z:/
#point to 
dir = 'Z:/SWAT_collatedFiles/collated/'
dump = 'Z:/SWAT_collatedFiles/rollingave/'

#columns to roll over
columns = c('yr', "precip_ha.m", "evap_ha.m", "seep_ha.m", "flo_stor_m3ps", 
            "sed_stor_tons", "orgn_stor_kgN", "sedp_stor_kgP", 
            "no3_stor_kgN", "solp_stor_kgP", "chla_stor_kg", "nh3_stor_kgN", "no2_stor_kgN",  
            "cbod_stor_kg", "dox_stor_kg", "san_stor_tons", "sil_stor_tons", "cla_stor_tons", 
            "sag_stor_tons", "lag_stor_tons", "grv_stor_tons", "flo_in_m3ps",    
            "sed_in_tons", "orgn_in_kgN", "sedp_in_kgP", "no3_in_kgN", "solp_in_kgP",   
            "chla_in_kg", "nh3_in_kgN", "no2_in_kgN", "cbod_in_kg", "dox_in_kg",   
            "san_in_tons", "sil_in_tons", "cla_in_tons", "sag_in_tons", "lag_in_tons",  
            "grv_in_tons", "flo_out_m3ps", "sed_out_tons", "orgn_out_kgN",  
            "sedp_out_kgP", "no3_out_kgN" ,   "solp_out_kgP", "chla_out_kg", "nh3_out_kgN" ,  
            "no2_out_kgN", "cbod_out_kg", "dox_out_kg", "san_out_tons",  "sil_out_tons"  ,
            "cla_out_tons", "sag_out_tons", "lag_out_tons", "grv_out_tons")

#list files in collated directory
indiv_collated = list.files(dir)

#grab yr collated files
yr_collated = indiv_collated[grepl('yr', indiv_collated)]

read_file = function(file){
  df = read.csv(file.path(dir, yr_collated[f]))
  df$modelrcp = paste0(df$climatemodel, df$rcp)
  df
}

getmodels = function(df){
  model_list = unique(df$modelrcp)
  model_list
}

subset_mod = function(df, model){
  sub = df %>% 
    filter(modelrcp == model)
  sub
}

getyears = function(df){
  year_list = unique(df$landcover_year)
  year_list
}

subset_modyr = function(df, year){
  sub = df %>% 
    filter(landcover_year == year)
  sub
}


# 3 year rolling
for(f in 1:length(yr_collated)){
  file = read_file(yr_collated[f])
  mod_list = getmodels(file)
  for(m in 1:length(mod_list)){
    one_model = subset_mod(file, mod_list[m])
    years = getyears(one_model)
    for(y in 1:length(years)){
      one_model_year = subset_modyr(one_model, years[y])
      for(c in 2:length(columns)){
        yrs = rollmean(one_model_year[,columns[1]], 3)
        val = rollmean(one_model_year[,columns[c]], 3)
        df = data.frame(yrs, val)
        df$par = columns[c]
        if(c == 2){
          allrolled = df
        } else {
          allrolled = full_join(allrolled, df)
        }
      }
    allrolled$landcover_year = years[y]
    allrolled$modelrcp = mod_list[m]
    allrolled$lake = file$lake[1]
    allrolled_h = allrolled %>% 
      pivot_wider(names_from = 'par',
                  values_from = 'val',
                  names_glue = '{par}_3y')
    if(y == 1){
      allyears_rolled = allrolled_h
    } else {
      allyears_rolled = full_join(allyears_rolled, allrolled_h)
    }
    }
    if(m == 1) {
      allmodyears_rolled = allyears_rolled
    } else {
      allmodyears_rolled = full_join(allmodyears_rolled, allyears_rolled)
    }
  }
  write.csv(allmodyears_rolled, file.path(dump, paste0(file$lake[1], '_3yr_rolling_res_yr.csv')), row.names = F)
}

# 5 year rolling
for(f in 1:length(yr_collated)){
  file = read_file(yr_collated[f])
  mod_list = getmodels(file)
  for(m in 1:length(mod_list)){
    one_model = subset_mod(file, mod_list[m])
    years = getyears(one_model)
    for(y in 1:length(years)){
      one_model_year = subset_modyr(one_model, years[y])
      for(c in 2:length(columns)){
        yrs = rollmean(one_model_year[,columns[1]], 5)
        val = rollmean(one_model_year[,columns[c]], 5)
        df = data.frame(yrs, val)
        df$par = columns[c]
        if(c == 2){
          allrolled = df
        } else {
          allrolled = full_join(allrolled, df)
        }
      }
      allrolled$landcover_year = years[y]
      allrolled$modelrcp = mod_list[m]
      allrolled$lake = file$lake[1]
      allrolled_h = allrolled %>% 
        pivot_wider(names_from = 'par',
                    values_from = 'val',
                    names_glue = '{par}_5y')
      if(y == 1){
        allyears_rolled = allrolled_h
      } else {
        allyears_rolled = full_join(allyears_rolled, allrolled_h)
      }
    }
    if(m == 1) {
      allmodyears_rolled = allyears_rolled
    } else {
      allmodyears_rolled = full_join(allmodyears_rolled, allyears_rolled)
    }
  }
  write.csv(allmodyears_rolled, file.path(dump, paste0(file$lake[1], '_5yr_rolling_res_yr.csv')), row.names = F)
}
      
# 10 year rolling
for(f in 1:length(yr_collated)){
  file = read_file(yr_collated[f])
  mod_list = getmodels(file)
  for(m in 1:length(mod_list)){
    one_model = subset_mod(file, mod_list[m])
    years = getyears(one_model)
    for(y in 1:length(years)){
      one_model_year = subset_modyr(one_model, years[y])
      for(c in 2:length(columns)){
        yrs = rollmean(one_model_year[,columns[1]], 10)
        val = rollmean(one_model_year[,columns[c]], 10)
        df = data.frame(yrs, val)
        df$par = columns[c]
        if(c == 2){
          allrolled = df
        } else {
          allrolled = full_join(allrolled, df)
        }
      }
      allrolled$landcover_year = years[y]
      allrolled$modelrcp = mod_list[m]
      allrolled$lake = file$lake[1]
      allrolled_h = allrolled %>% 
        pivot_wider(names_from = 'par',
                    values_from = 'val',
                    names_glue = '{par}_10y')
      if(y == 1){
        allyears_rolled = allrolled_h
      } else {
        allyears_rolled = full_join(allyears_rolled, allrolled_h)
      }
    }
    if(m == 1) {
      allmodyears_rolled = allyears_rolled
    } else {
      allmodyears_rolled = full_join(allmodyears_rolled, allyears_rolled)
    }
  }
  write.csv(allmodyears_rolled, file.path(dump, paste0(file$lake[1], '_10yr_rolling_res_yr.csv')), row.names = F)
}
