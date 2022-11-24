library(tidyverse)
library(ggthemes)

# mapped network drive(\\paris\etna\Scholarship\Holly Ewing\Students\Lars Gundersen) to Z:/
#point to 
dir = 'Z:/SWAT_collatedFiles/collated/'
dump = 'Z:/SWAT_figs/quickvis/'

#list files in collated directory
indiv_collated = list.files(dir)

#grab yr collated files
yr_collated = indiv_collated[grepl('yr', indiv_collated)]

#collate together for plots
for(c in 1:length(yr_collated)){
  df = read.csv(file.path(dir, yr_collated[c]))
  if(c == 1){
    all = df
  } else {
    all = full_join(all, df)
  }
}

# look at sedP_out_kgP at Barber
bar<-all %>% 
  filter(lake == 'Barber') %>% 
  mutate(rcp = as.factor(rcp))
ggplot(bar, aes(x = yr, y = sedp_out_kgP, lty = rcp)) +
  geom_point(color = 'lightgrey') +
  geom_smooth(method = 'loess') +
  facet_grid(landcover_year ~ .) +
  theme_bw()
ggsave(file.path(dump, 'sedP_out_barber_quick.jpg'))

#look at sedPout across all lakes 4.5 rcp, 2019 lu
all_4.5_2019 = all %>% 
  filter(rcp == 4.5 & landcover_year == 2019)

ggplot(all_4.5_2019, aes(x = yr, y = sedp_out_kgP)) +
  geom_point() +
  geom_smooth(method = 'loess', aes(color = lake))+
  theme_bw() +
  scale_color_ordinal()
ggsave(file.path(dump, 'sedP_out_crosslake_rcp4.5_2019_quick.jpg'))
