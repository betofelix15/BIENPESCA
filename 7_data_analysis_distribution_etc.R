# bubble map for fish distribution
# dynamic
# created: Feb 7, 2025
# Author: Jesus Felix

##### 1. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", "gganimate",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata", "leaflet","fontawesome","dygraphs","shiny","xts")

# This line says" check and see which of the packages you want is already
# installed.  Those note loadded are ones you "need".
need <- want[!(want %in% installed.packages()[,"Package"])]

# This line says, if a package is "needed", install it.
if (length(need)) install.packages(need)

# This line "requires" all the packages you want.  Meaning that in order 
# to use a function that is loaded, you need to "activate" it, by "requiring" it.
sapply(want, function(i) require(i, character.only = TRUE))

# This line removes the vectors "want" and "need".
rm(want, need)


### Working directories

# This line creates an empty list, which the next few lines populate with 
# a set of file paths.
dir <- list()

# This line uses getwd() to find where your .Rproj file is.
getwd()

# This line takes this location as the "root" directory for the project.
#dir$root <- dirname(getwd()) # root directory of the project 
# getwd() is actually the directory we need (dependent on where you open project)******discuss with Steve*****


dir$root <- getwd()

# Observe the output in the console:
dir$root 

# shapefiles
dir.create(paste0(dir$root, "/shapefiles/"))
dir$shp <- paste0(dir$root, "/shapefiles/")

# figures directory (these line create a new folder for you to export figures)
dir.create(paste0(dir$root, "/figures/"))
dir$fig <- paste0(dir$root, "/figures/")


# unprocessed data folder (this line creates a new folder for "raw" data)
dir.create(paste0(dir$root, "/data/raw data/"))
dir$rawdata <- paste0(dir$root, "/data/raw data/")

# clean data folder (this line creates a new folder for "clean" data)
dir.create(paste0(dir$root, "/data/clean data/"))
dir$cleandata <- paste0(dir$root, "/data/clean data/")

# how to make not in
`%not_in%` <- purrr::negate(`%in%`)

# remove scientific notation
options(scipen = 999)

##### 2. Choose the fish ------------------------------------------------------------------

# choose the fish category
fish <- "CAMARON"

# other choices: 
# [1] "BAGRE"   -> catfish                   
# [2] "CARPA"   -> Carp                    
# [3] "LOBINA"                      
# [4] "MOJARRA"  -> its like a tilapia                   
# [5] "ALMEJA"    -> clam                  
# [6] "BAQUETA"                     
# [7] "BERRUGATA"                   
# [8] "BONITO"                      
# [9] "CABRILLA"                    
# [10] "CALAMAR"    -> squid                 
# [11] "CAPTURA SIN REGISTRO OFICIAL"  -> no official record
# [12] "CARACOL" -> snail                 
# [13] "CAZON"                       
# [14] "CORVINA"                     
# [15] "GUACHINANGO"                 
# [16] "JUREL"                       
# [17] "LENGUADO"                    
# [18] "LISA"                        
# [19] "MERO Y SIMILARES"            
# [20] "OTRAS"                       
# [21] "PAMPANO"                     
# [22] "PARGO"                       
# [23] "PULPO"                       
# [24] "RAYA Y SIMILARES"            
# [25] "SIERRA"                      
# [26] "TIBURON" --> shark!                    
# [27] "ALGAS"                       
# [28] "ERIZO"                       
# [29] "JAIBA" -> blue, freshwater crab                  
# [30] "LANGOSTA"                    
# [31] "PEPINO DE MAR"               
# [32] "SARGAZO"                     
# [33] "ABULON"                      
# [34] "ANCHOVETA"                   
# [35] "ATUN"                        
# [36] "BARRILETE"                   
# [37] "CAMARON"  -> shrimp                   
# [38] "FAUNA"                       
# [39] "MACARELA"                    
# [40] "OSTION"                      
# [41] "SARDINA"                     
# [42] "PIERNA"                      
# [43] "RONCO"                       
# [44] "BANDERA"                     
# [45] "ROBALO"                      
# [46] "BESUGO"                      
# [47] "CINTILLA"                    
# [48] "LEBRANCHA"                   
# [49] "PETO"                        
# [50] "RUBIA Y VILLAJAIBA"          
# [51] "TRUCHA"                      
# [52] "ESMEDREGAL"                  
# [53] "RUBIO"                       
# [54] "LANGOSTINO"                  
# [55] "CHARAL"                      
# [56] "OTRAS SIN REGISTRO OFICIAL"  
# [57] "PECES DE ORNATO"             
# [58] "TILAPIA"                     
# [59] "MERO"                        
# [60] "ORNATO"                      
# [61] "PEPINO "                     
# [62] "SARGAZO "                    
# [63] "FAUNA DE ACOMPANAMIENTO" 


##### 3. Choose monthly or annual ---------------------------------
# Temporal choices: annual or monthly
t <- "annually"
##### 4. Load/transform data -----------------------------------------------------


if(t == "monthly"){
  
  pd <- read.csv(paste0(dir$cleandata, 'production_nodup_feb2025.csv'))
  
  # summarize by month and category, (sometimes catch is repeated in one month)
  pd1 <- pd %>% group_by(year,month,state,state_id,office,office_id,
                         category) %>%
    summarise(unload_wt_kg = sum(unload_wt)/1000, # 1k kg
              live_wt_kg = sum(live_wt)/1000, # 1k kg
              value_1k_mxn = sum(value_mx)/1000 # 1k Pesos
    )
  pd <- pd1
  
  # create a date column
  pd$date <- as.Date(paste(pd$year,pd$month,"28", sep = "-"))

  
}else{ # if annual
  
  pd <- read.csv(paste0(dir$cleandata,"production_nodup_feb2025.csv"))
  # summarize by year and category
  pd1 <- pd %>% group_by(year,state,state_id,office,office_id,
                        category) %>%
    summarise(unload_wt_1m_kg = sum(unload_wt)/1000000, # 1 million kg
              live_wt_1m_kg = sum(live_wt)/1000000,
              value_mx_1m_mxn = sum(value_mx)/1000000 # 1 million pesos
              )
  pd <- pd1
  
  # create a date column
  pd$date <-pd$year
  
}

# filter by fish category and year (must be 2006 and above)
pd_fish <- pd[pd$category == fish & pd$year >= 2006,]

# # # load the sf files for the map and office points # # # # # # # # # # # # 
# Mexico boundaries
mex_shp <- read_sf(paste0(dir$shp,"MEX_adm0.shp"))
# plot(mex_shp$geometry)

# Office points (no need to do the next 7 lines again)
# office_sf <- read_sf(paste0(dir$shp,"office_points_nov_21_2024.shp"))
# # make Ciudad de Mexico , id = 1510
# office_sf[office_sf$office == "CD DE MEXICO",]$office_id <- 1510
# office_sf[office_sf$office == "CD DE MEXICO",]$state_id <- 15
# 
# # save this file to use as latest
# write_sf(office_sf, paste0(dir$shp,"office_points_7_feb_2025.shp"))

office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))
colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")

# check crs
crs(office_sf$geometry)
crs(mex_shp$geometry) 

# change the crs for the mex_shp
mex_shp <- st_transform(mex_shp, crs = crs(office_sf))

# check crs
crs(office_sf$geometry)
crs(mex_shp$geometry) 

# make an sf object from the production and office points 
pd_sf <- left_join(pd_fish,office_sf[c("office","office_id","geometry")],
                   by = c("office","office_id"))
pd_sf <- st_as_sf(pd_sf)

# for some reason its better to have the lat and long identified
coords_pd <- st_coordinates(pd_sf)
pd_sf$lat <- coords_pd[,2]
pd_sf$long <- coords_pd[,1]

##### 5. Bubble Map Weight------------------------------------------------------------

# define weight by annual or monthly
if(t == "monthly"){
  wt_title <- "Harvest (in 1000 kg)"
}else{ # t== "annual"
  wt_title <- "Harvest (in 1 million kg)"
}


fish_gif <-ggplot() +
  geom_sf(data = mex_shp, fill = "lightgrey", alpha = 0.2) +
  geom_point(data = pd_sf, 
             aes(x = long,
                 y = lat,
                 size = unload_wt,
                 color = unload_wt))+
  scale_size_continuous(name = wt_title,
                        range = c(1,15))+
  scale_alpha_continuous(name = wt_title,
                         range = c(0.1,.9))+
  scale_color_viridis_c(name = wt_title)+
  guides(colour = guide_legend()) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())+
  # here is the gganimate bits
  labs(title = paste0(fish,' harvest in {frame_time}'))+
  transition_time(date)+
  enter_fade()+
  exit_fade()+
  ease_aes("linear",
           interval = 0.001)

# duration depending on t
if(t == "annual"){
  dur <- 22
}else{
  dur <- 200
}

# animate it (if you want to see it)
animate(fish_gif,
        duration = dur,
        fps = 1,
        units = "in",
        width = 5,
        height = 4.5,
        start_pause = 3,
        res = 300,
        end_pause = 3)

# Save as gif:
anim_save(paste0(dir$fig,fish,'_',t,".gif"),
          animation = last_animation(),
          fps = 1)




###### DISTRIBUTION OF FUEL SUBSIDIES (GAS RIBERENO) ---------------------------


# load the fuel subsidy data sf object
gas_sf <- st_read(
  paste0(dir$shp,"gas_ribereno_sf_summarized_to_localities.shp")
)

colnames(gas_sf) <- c("year","state_id","state","mun_id","municipio","local_id","locality", "total_gas_liters", 
 "obs_id","geometry")

colnames(gas_sf)
# load geographic info
mun.pol <- st_read(paste0(dir$shp,"mun_polygons.shp"))
state.sf <- st_read(paste0(dir$shp,"state_sf.shp"))
office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))
colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")


# use mapview to see how offices and municipios distributed
gas_temp <- gas_sf %>% filter(year == 2016)
pd_temp <- pd_sf %>% filter(year == 2016)
pal <- mapviewPalette("mapviewSpectralColors")

mapview(gas_temp, 
          zcol = "total_gas_liters",
          cex = "total_gas_liters") +
  mapview(pd_temp,
          cex = "unload_wt",
          col.regions = pal(100))



# Calculate center of map (roughly Mexico)
mex_center <- c(lng = -102, lat = 23)

# Create leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # clean basemap of Mexico
  setView(lng = mex_center["lng"], lat = mex_center["lat"], zoom = 5) %>%  # center on Mexico
  addCircleMarkers(data = gas_temp,
                   radius = ~sqrt(total_gas_liters)/40,
                   color = "blue",
                   fillOpacity = 0.25,
                   label = ~paste0(total_gas_liters," liters")) %>%
  addAwesomeMarkers(data = pd_temp,
                    icon = awesomeIcons(icon = "fa-fish",
                                        library = "fa",
                                        markerColor = "orange"),
                    label = ~paste0( round(unload_wt_1m_kg,3), " M kg"))



###### PP/BP Subsidy by state gif ----------------------------------------------
# load bp data
bp_df <- read.csv(paste0(dir$cleandata,"agg_bp_data_with_office.csv"))

# load state sf
state_sf <- st_read(paste0(dir$shp,"state_sf.shp"))

#filter bp_df to 2013 to 2024, aggregate to state, add a cumulative
bp_df <- bp_df %>%
  group_by(year,state_id,state) %>%
  summarize(
    total_supplied = sum(total_supplied, na.rm = T),
    n_participants = sum(n_participants, na.rm = T)
  )

# empty_2013 <- bp_df %>%
#   dplyr::mutate(
#     year = 2013,
#     total_supplied = 0,
#     n_participants = 0
#   )
# 
# # bind to bp_df
# bp_df <- rbind(empty_2013,bp_df)

bp_df$cmltv_exposure <- ave(bp_df$total_supplied, bp_df$state_id, FUN=cumsum)

# scale it by 1000 pesos
bp_df$cmltv_1k_mxn <- bp_df$cmltv_exposure/1000

# join bp with sf
bp_state_join <- left_join(bp_df,state_sf, by = c("state_id" = "st_id"))

# make bp an sf object
bp_sf <- st_as_sf(bp_state_join)
bp_sf$year <- factor(bp_sf$year, levels = 2014:2024)


state_gif <- ggplot() +
  geom_sf(data = state_sf, 
          fill = "lightgrey", 
          alpha = 0.2)+
  geom_sf(data = bp_sf, 
          aes(fill = cmltv_1k_mxn)) +
  # scale_fill_viridis_c(name = "Cumulative Exposure (1,000 MXN)",
  #                      option = "plasma",
  #                      direction = -1) +
  scale_fill_gradientn(
    name = "Cumulative Exposure (1,000 MXN)",
    colors = c("beige", "darkgreen")
  )+
  theme_void() +
  labs(title = 'PROPESCA/BIENPESCA Distribution {current_frame}') +
  # transition_states(year, 
  #                   transition_length = 0,
  #                   state_length = 1)
  transition_manual(year)


# animate it (if you want to see it)
animate(state_gif,
        duration = 20,
        fps = 1,
        units = "in",
        width = 6,
        height = 5,
        start_pause = 2,
        res = 300,
        end_pause = 2)

# Save as gif:
anim_save(paste0(dir$fig,"state_bp_pp_distribution_2014_2014_1k_scaled.gif"),
          animation = last_animation(),
          fps = 1)


# now do participants


state_gif <- ggplot() +
  geom_sf(data = state_sf, 
          fill = "lightgrey", 
          alpha = 0.2)+
  geom_sf(data = bp_sf, 
          aes(fill = n_participants)) +
  # scale_fill_viridis_c(name = "N participants",
  #                      option = "plasma",
  #                      direction = -1) +
  scale_fill_gradientn(
    name = "N Participants",
    colors = c("beige", "darkgreen")
  )+
  theme_void() +
  labs(title = 'PROPESCA/BIENPESCA Distribution {current_frame}') +
  # transition_states(year, 
  #                   transition_length = 0,
  #                   state_length = 1)
  transition_manual(year)


# animate it (if you want to see it)
animate(state_gif,
        duration = 20,
        fps = 1,
        units = "in",
        width = 6,
        height = 5,
        start_pause = 2,
        res = 300,
        end_pause = 2)

# Save as gif:
anim_save(paste0(dir$fig,"state_bp_pp_distribution_2014_2014_n_participants.gif"),
          animation = last_animation(),
          fps = 1)

##### Distribution of Shrimp and Lisa by state ----------------------------------

# Mexico boundaries
mex_shp <- read_sf(paste0(dir$shp,"MEX_adm0.shp"))

# load bp data
df <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset_13_nov_2025.csv"))

# load office st
office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")

#filter bp_df to 2013 to 2024, aggregate to state, add a cumulative
df_sum <- df %>%
  group_by(year,office,office_id) %>%
  summarize(
    shrimp_kg = sum(agg_unld_wt_kg_imp_shrimp, na.rm = T),
    lisa_kg = sum(agg_unld_wt_kg_imp_lisa, na.rm = T)
    )


df_sum$agg_kg <- df_sum$shrimp_kg+df_sum$lisa_kg
df_sum$agg_10k_kg <- df_sum$agg_kg/10000


pd_sf <- left_join(df_sum,office_sf[c("office","office_id","geometry")],
                   by = c("office","office_id"))
pd_sf <- st_as_sf(pd_sf)


## gif with ggplot
shrimp_lisa_gif <- ggplot() +
  geom_sf(data = mex_shp, fill = "lightgrey", alpha = 0.2) +
  geom_sf(data = pd_sf,
  aes(
    size = agg_10k_kg,
    alpha = agg_10k_kg,
    color = agg_10k_kg
  )) +
  
  # SIZE SCALE
  scale_size_continuous(
    name = "Catch 10,00 kg",
    breaks = c(100,250,500,750,1000),
    range = c(1, 15)
  ) +
  
  # COLOR SCALE (change colors here)
  scale_color_gradient(
    name = "Catch 10,00 kg",
    low = "lightblue",
    high = "darkred",
    guide = "none"
  ) +
  
  # ALPHA (transparency)
  scale_alpha_continuous(
    range = c(0, 0.95),
    guide = "none"   # hide alpha legend
  ) +
  
  theme_void() +
  
  # here is the gganimate bits
  labs(title = paste0('Shrimp & Lisa Catch in {current_frame}'))+
  transition_manual(year)


# animate it (if you want to see it)
animate(shrimp_lisa_gif,
        duration = 20,
        fps = 1,
        units = "in",
        width = 6,
        height = 5,
        start_pause = 2,
        res = 300,
        end_pause = 2)

# Save as gif:
anim_save(paste0(dir$fig,"shrimp_lisa_bubble_map_2006_2024.gif"),
          animation = last_animation(),
          fps = 1)

# by state ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_sum <- df %>%
  group_by(year,state_id,state) %>%
  summarize(
    shrimp_kg = sum(agg_unld_wt_kg_imp_shrimp, na.rm = T),
    lisa_kg = sum(agg_unld_wt_kg_imp_lisa, na.rm = T),
    agg_catch = sum(agg_unld_wt_kg_imp_non_ac, na.rm = T)
  )






df_sum$agg_kg <- df_sum$shrimp_kg+df_sum$lisa_kg
df_sum$agg_10k_kg <- df_sum$agg_kg/10000



state_sf <- st_read(paste0(dir$shp,"state_sf.shp"))


pd_sf <- left_join(df_sum,state_sf,
                   by = c("state_id" = "st_id"))
pd_sf <- st_as_sf(pd_sf)


## gif with ggplot
state_gif <- ggplot() +
  geom_sf(data = state_sf, 
          fill = "lightgrey", 
          alpha = 0.2)+
  geom_sf(data = pd_sf, 
          aes(fill = agg_10k_kg)) +
  # scale_fill_viridis_c(name = "Cumulative Exposure (1,000 MXN)",
  #                      option = "plasma",
  #                      direction = -1) +
  scale_fill_gradientn(
    name = "10,000 kg.",
    colors = c("beige", "darkred")
  )+
  theme_void() +
  labs(title = 'Shrimp & Lisa Catch in {current_frame}') +
  # transition_states(year, 
  #                   transition_length = 0,
  #                   state_length = 1)
  transition_manual(year)


# animate it (if you want to see it)
animate(state_gif,
        duration = 20,
        fps = 1,
        units = "in",
        width = 6,
        height = 5,
        start_pause = 2,
        res = 300,
        end_pause = 2)

# Save as gif:
anim_save(paste0(dir$fig,"state_shrimp_lisa_2006_2014.gif"),
          animation = last_animation(),
          fps = 1)



###### Fish and Lisa Catch data analysis -------------------------------------------------

# load bp data
df <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset_13_nov_2025.csv"))

# summarize by year

df_sum <- df %>%
  group_by(year) %>%
  summarize(
    shrimp_100k_kg = sum(agg_unld_wt_kg_imp_shrimp, na.rm = T)/100000,
    lisa_kg_100k_kg = sum(agg_unld_wt_kg_imp_lisa, na.rm = T)/100000,
    agg_catch_100k_kg = sum(agg_unld_wt_kg_imp_non_ac, na.rm = T)/100000,
    
    ln_shrimp = log(sum(agg_unld_wt_kg_imp_shrimp, na.rm = T)),
    ln_lisa =  log(sum(agg_unld_wt_kg_imp_lisa, na.rm = T)),
    ln_agg = log(sum(agg_unld_wt_kg_imp_non_ac, na.rm = T))
  )


## ggplot with line chart
ggplot() +
  # geom_line(data = df_sum,
  #           color = "blue",
  #           aes(x = year,
  #               y = agg_catch_100k_kg)
  #           )+
  geom_line(data = df_sum,
            color = "darkred",
            linetype = "dashed",
            aes(x = year,
                y = shrimp_100k_kg))+
  geom_line(data = df_sum,
            color = "purple",
            linetype = 3,
            aes(x = year,
                y = lisa_kg_100k_kg)) +
  labs(title = "Shrimp & Lisa Catch 2006-2024")+
  xlab("Year")+
  ylab("100,000 kg.")+
  legend(legend = c("Shrimp","Lisa"),
         col = c("darkred","purple"),
         lty = c("dashed",3))+
  theme_minimal()

# export the df_sum
write.csv(df_sum,
          paste0(dir$cleandata,"shrimp_lisa_and_agg_catch.csv"),
          row.names = F)



#### TIME SERIES ENVIRONMENTAL ------------------------------------------------------------------

# Load the office data
df <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset.csv"))

# load the data 
chl <- read.csv(paste0(dir$cleandata,"chlorophyll_concentration_mg_m3.csv"))

# load the sst
sst <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024.csv"))

# load the coastal precip
precip <- read.csv(paste0(dir$cleandata,"coastal_precip_2006_2024_all.csv"))


# load wind
wind <- read.csv(paste0(dir$cleandata,"avg_wind_speed_2005_2023.csv"))


# add a date column to the sst and precip
sst$date <- as.Date(paste(sst$year,sst$month,"01", sep = "-"))
precip$date <- as.Date(paste(precip$year,precip$month,"01", sep = "-"))

# change sst to celsius and faranheit
sst$avg_sst_c <- sst$avg_sst * 0.01
sst$avg_sst_f <- (sst$avg_sst_c * 1.8) + 32

# office list with state
office_df <- df %>% select(state_id,state,office_id,office) %>%
  unique()

pacific <- c('BAJA CALIFORNIA SUR','BAJA CALIFORNIA','SONORA',
             'SINALOA','NAYARIT','JALISCO','COLIMA','MICHOACAN DE OCAMPO',
             'GUERRERO','OAXACA','CHIAPAS')

atlantic <- c('QUINTANA ROO','YUCATAN','CAMPECHE','TABASCO','VERACRUZ DE IGNACIO DE LA LLAVE',
              'TAMAULIPAS')


office_df$coast <- ifelse(office_df$state %in% pacific,"PACIFIC",
                          ifelse(office_df$state %in% atlantic,"ATLANTIC",
                                 'INLAND'))
sst <- left_join(sst %>% select(office_id,date,avg_sst_f),
                 office_df,
                 by = 'office_id')



# summarize
sst_monthly <- sst %>%
  group_by(date) %>%
  summarize(
    National = mean(avg_sst_f, na.rm = T),
    Atlantic = mean(avg_sst_f[coast == "ATLANTIC"], na.rm = TRUE),
    Pacific = mean(avg_sst_f[coast == "PACIFIC"], na.rm = TRUE)
  )




sst_long <- sst_monthly %>%
  tidyr::pivot_longer(
    cols = c(National,
             Atlantic,
             Pacific),
    names_to = "region",
    values_to = "sst_f"
  ) 


ggplot(sst_long, aes(x = date, y = sst_f, color = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 0) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Monthly Average Sea Surface Temperature in Mexico",
    x = "Date",
    y = "SST (°F)",
    color = "Region"
  ) +
  theme_bw()


### Precipitation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~````


precip <- left_join(precip %>% select(office_id,date,avg_prec),
                 office_df,
                 by = 'office_id')

# summarize
precip_monthly <- precip %>%
  group_by(date) %>%
  summarize(         # convert to inches by multiplying 39.3700787402 
    National = mean(avg_prec, na.rm = T)*39.3700787402 ,
    Atlantic = mean(avg_prec[coast == "ATLANTIC"], na.rm = TRUE)*39.3700787402 ,
    Pacific = mean(avg_prec[coast == "PACIFIC"], na.rm = TRUE)*39.3700787402 
  )


prec_long <- precip_monthly %>%
  tidyr::pivot_longer(
    cols = c(National,
             Atlantic,
             Pacific),
    names_to = "region",
    values_to = "prec_in"
  )


ggplot(prec_long, aes(x = date, y = prec_in, color = region)) +
  geom_line(size = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(
    title = "Monthly Average Coastal Precipitation in Mexico",
    x = "Date",
    y = "Precipitation (in)",
    color = "Region"
  ) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,   # 0 = left, 0.5 = center, 1 = right
      vjust = 2,     # Positive moves up, negative moves down
      size = 16,     # Font size
      face = "bold"  # Bold text
    )
  )


### chlorophyll conc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``



chl <- left_join(chl %>% select(office_id,date,chlorophyll_conc),
                    office_df,
                    by = 'office_id')


chl$date <- as.Date(chl$date)

# summarize
chl_monthly <- chl %>%
  group_by(date) %>%
  summarize(
    National = mean(chlorophyll_conc, na.rm = T),
    Atlantic = mean(chlorophyll_conc[coast == "ATLANTIC"], na.rm = TRUE),
    Pacific = mean(chlorophyll_conc[coast == "PACIFIC"], na.rm = TRUE)
  )

chl_long <- chl_monthly %>%
  tidyr::pivot_longer(
    cols = c(National,
             Atlantic,
             Pacific),
    names_to = "region",
    values_to = "mg_m3"
  ) 


ggplot(chl_long, aes(x = date, y = mg_m3, color = region)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(
    title = "Monthly Average Chlorophyll Concentration in Mexico",
    x = "Date",
    y = expression("Concentration (mg/m"^3*")"),
    color = "Region"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,   # 0 = left, 0.5 = center, 1 = right
      vjust = 2,     # Positive moves up, negative moves down
      size = 16,     # Font size
      face = "bold"  # Bold text
    )
  )

summary(chl$chlorophyll_conc
        )

### wind speed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``


# wind speed (m/s v is vertical wind and u is eastward wind)
ws <- NULL
for(y in 2006:2024){
  
  w <- read.csv(paste0(dir$cleandata,"wind_speed/wind_speed_",y,".csv"))
  
  ws <- rbind(ws,w)
  
}

# remove unnecessary columns
ws <- ws %>% select(-.geo,-system.index)

# calculate general wind speed (refer to this: https://sgichuki.github.io/Atmo/)
ws$avg_wind_speed <- sqrt(ws$avg_east_wind_speed^2 + ws$avg_north_wind_speed^2)

# add a date column
ws$date <- as.Date(paste(ws$year,ws$month,"01", sep = "-"))


ws <- left_join(ws %>% select(office_id,date,avg_wind_speed),
                 office_df,
                 by = 'office_id')


# summarize
ws_monthly <- ws %>%
  group_by(date) %>%
  summarize(
    National = mean(avg_wind_speed, na.rm = T),
    Atlantic = mean(avg_wind_speed[coast == "ATLANTIC"], na.rm = TRUE),
    Pacific = mean(avg_wind_speed[coast == "PACIFIC"], na.rm = TRUE)
  )

ws_long <- ws_monthly %>%
  tidyr::pivot_longer(
    cols = c(National,
             Atlantic,
             Pacific),
    names_to = "region",
    values_to = "m_s"
  ) 


ggplot(ws_long, aes(x = date, y = m_s, col  = region)) +
  geom_line(size = .75) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(
    title = "Monthly Average Wind Speed in Mexico",
    x = "Date",
    y = "speed (m/s)",
    color = "Region"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,   # 0 = left, 0.5 = center, 1 = right
      vjust = 2,     # Positive moves up, negative moves down
      size = 16,     # Font size
      face = "bold"  # Bold text
    )
  )
