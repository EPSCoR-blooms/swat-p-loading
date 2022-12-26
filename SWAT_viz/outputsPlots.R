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

#downloads all collated files
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

# plotting function for any SWAT output by climate model

output_by_climatemodel <- function(data, lake, rcp, landcover_year){ 
  
  climatemodel_data <- data %>% 
    filter(lake == lake) %>% 
    filter(rcp == rcp) %>% 
    filter(landcover_year == landcover_year)
  
  climatemodel_plot <- ggplot() +
    geom_point(data = climatemodel_data, aes(x = yr, y = sedp_in_kgP, color = climatemodel, 
                                               shape = climatemodel), alpha = 0.5) +
    stat_smooth(data = climatemodel_data, aes(x = yr, y = sedp_in_kgP, color = climatemodel,
                                                  fill = climatemodel), method = "loess", formula = y ~ x) +
    theme_minimal() +
    xlab("Year") +
    ylab("Sediment P In (kg/P)") +
    facet_wrap(~climatemodel)
  
  return(climatemodel_plot)
}

swatAuburn_collatedAnnual %>% 
  ggplot() +
  geom_point(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = climatemodel, 
                                                   shape = climatemodel), alpha = 0.5) +
  stat_smooth(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = climatemodel,
                                                    fill = climatemodel), method = "loess", formula = y ~ x) +
  theme_minimal() +
  xlab("Year") +
  ylab("Sediment P In (kg/P)") +
  facet_wrap(rcp~landcover_year)

swatAuburn_collatedAnnual %>% 
  filter(climatemodel == "giss") %>% 
  ggplot() +
  geom_point(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = rcp), alpha = 0.5) +
  stat_smooth(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = rcp, fill = rcp), method = "loess", formula = y ~ x) +
  theme_minimal() +
  xlab("Year") +
  ylab("Sediment P In (kg/P)") +
  facet_wrap(~landcover_year)

swatAuburn_collatedAnnual %>% 
  ggplot() +
  geom_point(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = landcover_year), alpha = 0.5) +
  stat_smooth(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = landcover_year, fill = landcover_year), method = "loess", formula = y ~ x) +
  theme_minimal() +
  xlab("Year") +
  ylab("Sediment P In (kg/P)")

swatAuburn_collatedAnnual %>% 
  ggplot() +
  geom_point(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = rcp), alpha = 0.5) +
  stat_smooth(data = swatAuburn_collatedAnnual, aes(x = yr, y = sedp_in_kgP, color = rcp, fill = rcp), method = "loess", formula = y ~ x) +
  theme_minimal() +
  xlab("Year") +
  ylab("Sediment P In (kg/P)") +
  facet_wrap(~climatemodel)

# collated rolling average 3, 5, and 10 yr data

tmpdir = 'tmp' #point to directories

if(!dir.exists(tmpdir)){dir.create(tmpdir)} #create temporary directory - this creates a directory in your working directory called ‘tmp’

drive_auth() # authorize googledrive to access your Google Drive -- follow browser instructions or the prompts in the console.

did = shared_drive_find('EPSCoR_SWAT')$id #find shared drive id

folder1_id = drive_ls(path = as_id(did), pattern = 'SWAT_collatedFiles')$id

folder2_id = drive_ls(path = as_id(folder1_id), pattern = 'rollingave')$id #get folder id

csv_files <- drive_ls(folder2_id, type = "csv")

#downloads all files in the "collated" folder into temporary folder in R
csv_files %>% 
  split(csv_files$id) %>% 
  walk(~drive_download(.$id, path = file.path("tmp", .$name), overwrite = TRUE))

swatAuburn_collatedAnnual <- read.csv(file.path(tmpdir, "Auburn_basin_res_yr_collate.csv"))
#while all files are downloaded to R, only one file has now been read in





asv_plotter <- function(map_bounds, deployment, lake_all, zoom){ #where deployment is the dataframe of that particular deployment date
  #and lake_all is a combined dataframe that contains all deployments for a single lake
  #and map_bounds is a set boundary of the spatial extent to be mapped
  # and zoom is either 12 for SAB or 14 for AUB, CHN
  
  a <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = pH), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(pH, na.rm = T)-min(pH, na.rm = T))/2)+min(pH, na.rm = T),
                          name="pH", data = lake_all) +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          legend.text = element_text(angle = 45, hjust = 0, vjust = 0, size = 6), axis.text=element_text(colour="black"),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  aa <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = pH, color = pH), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(pH, na.rm = T)-min(pH, na.rm = T))/2)+min(pH, na.rm = T),
                          name="pH", data = lake_all) +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, pH, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("pH") +
    labs(tag="A")
  
  b <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = temperatureWater_degC), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(temperatureWater_degC, na.rm = T)-min(temperatureWater_degC, na.rm = T))/2)+min(temperatureWater_degC, na.rm = T),
                          name="Temperature (C)", data = lake_all) +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12, color = "black"), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.text = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 6),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  bb <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = temperatureWater_degC, color = temperatureWater_degC), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$temperatureWater_degC)-min(lake_all$temperatureWater_degC))/2)+min(lake_all$temperatureWater_degC),
                          name="Temperature (C)") +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, temperatureWater_degC, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("Temperature (C)") +
    labs(tag="B")
  
  c <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = specificConductance_uscm), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$specificConductance_uscm, na.rm = T)-min(lake_all$specificConductance_uscm, na.rm = T))/2)+min(lake_all$specificConductance_uscm, na.rm = T),
                          name="SpC (uS/cm)") +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12, color = "black"), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.text = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 6),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  cc <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = specificConductance_uscm, color = specificConductance_uscm), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$specificConductance_uscm, na.rm = T)-min(lake_all$specificConductance_uscm, na.rm = T))/2)+min(lake_all$specificConductance_uscm, na.rm = T),
                          name="Specific conductance (uS/cm)") +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, specificConductance_uscm, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("Specific conductance (uS/cm)") +
    labs(tag="C")
  
  d <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = chlorophyll_a_RFU), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$chlorophyll_a_RFU)-min(lake_all$chlorophyll_a_RFU))/2)+min(lake_all$chlorophyll_a_RFU),
                          name="Chl a (RFU)") +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12, color = "black"), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.text = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 6),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  dd <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = chlorophyll_a_RFU, color = chlorophyll_a_RFU), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$chlorophyll_a_RFU)-min(lake_all$chlorophyll_a_RFU))/2)+min(lake_all$chlorophyll_a_RFU),
                          name="Chlorophyll a (RFU)") +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, chlorophyll_a_RFU, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("Chlorophyll a (RFU)") +
    labs(tag="D")
  
  e <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = oxygenDissolved_mgl), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$oxygenDissolved_mgl)-min(lake_all$oxygenDissolved_mgl))/2)+min(lake_all$oxygenDissolved_mgl),
                          name="DO (mg/L)") +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12, color = "black"), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.text = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 6),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  ee <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = oxygenDissolved_mgl, color = oxygenDissolved_mgl), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$oxygenDissolved_mgl)-min(lake_all$oxygenDissolved_mgl))/2)+min(lake_all$oxygenDissolved_mgl),
                          name="Dissolved oxygen (mg/L)") +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, oxygenDissolved_mgl, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("Dissolved oxygen (mg/L)") +
    labs(tag="E")
  
  f <- get_stamenmap(map_bounds, zoom = zoom, maptype = "toner-background") %>% ggmap()+
    geom_point(aes(x = longitude_gps_deg, y = latitude_gps_deg, color = turbidity_NTU), size=1, shape = 10,
               data = deployment) +
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$turbidity_NTU, na.rm = T)-min(lake_all$turbidity_NTU, na.rm = T))/2)+min(lake_all$turbidity_NTU, na.rm = T),
                          name="Turb (NTU)") +
    theme(legend.position = "none", axis.title=element_blank(), text = element_text(size=12, color = "black"),
          legend.key.width = unit(3, "mm"), plot.margin = margin(1, 1, 1, 1),
          legend.title = element_text(size=12, color = "black"), axis.text = element_blank(), axis.ticks = element_blank(),
          legend.text = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 6),
          panel.border = element_rect(color = "black", fill = NA, size = 2))
  
  ff <- ggplot() + 
    geom_point(data = deployment, aes(x = as.POSIXct(timestamp_sonde_sec, origin="1970-01-01", tz = "EST"), y = turbidity_NTU, color = turbidity_NTU), size = 3)+
    scale_color_gradient2(low = 'yellow', mid = 'purple', high = 'blue', 
                          midpoint = ((max(lake_all$turbidity_NTU, na.rm = T)-min(lake_all$turbidity_NTU, na.rm = T))/2)+min(lake_all$turbidity_NTU, na.rm = T),
                          name="Turbidity (NTU)") +
    theme_bw()+
    xlab("Timestamp") + 
    geom_line(data = asv_window(deployment, turbidity_NTU, 120), aes(x = as.POSIXct(date, origin="1970-01-01", tz="EST"), y = mean), color = 'black', size=1)+
    theme(legend.position = "none", panel.background=element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(), text=element_text(size=12, color = "black"),
          axis.text=element_text(colour="black"), legend.key = element_rect(fill = NA),
          plot.tag.position = c(0.95, 0.95))+
    scale_x_datetime(breaks = scales::date_breaks("20 mins"), date_labels = "%H:%M")+
    xlab("Time (EDT)")+
    ylab("Turbidity (NTU)") +
    labs(tag="F")
  
  plot <- ggarrange(aa, bb, cc, dd, ee, ff,
                    a, b, c, d, e, f,
                    ncol=6, nrow = 2)
  
  return(plot)
}





## write and save to drive ----
write.csv(sun, file.path(tmpdir,'CrossSiteGloeo_Count_Sunapee_v22Jul2022.csv'), row.names = F)

drive_upload(media = file.path(tmpdir,'CrossSiteGloeo_Count_Sunapee_v22Jul2022.csv'),
             path = as_id(format_id),
             overwrite = T)


# DELETE TMP FOLDER ----
unlink(tmpdir, recursive = T)