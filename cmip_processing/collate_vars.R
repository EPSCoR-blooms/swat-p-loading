## make intermediary variable tables per projection ----

collated_horizontal = collated %>% 
  select(-stdev) %>% 
  pivot_wider(names_from = 'parameter',
              values_from = 'weighted_mean')

for(proj in 1:length(projection_list)){
  df <- collated_horizontal[collated_horizontal$cmip_projection == projection_list[proj],]
  
}

