# Step 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi","KableExtra",
          "mgsub","zoo","plotly","gdata","gmapsdistance","data.table","geosphere","ggspatial",
          "rnaturalearth","rnaturaleathdata")

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


dir$root <- str_remove(getwd(),"/R Code")

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


##### Load Data/Transform------------------------------------------------------------------

# load the fuel subsidy data from CausaNatura
fuel <- read.csv(paste0(dir$rawdata,"subsidio_combustibles_marinos_2011_2019.csv"))

#load the location data
loc_geo <- read.csv(paste0(dir$cleandata,"locality_location_data.csv"))

# keep the important columns
loc_ids <- loc_geo %>% select(state_id,state,mun_id,municipio, local_id,locality,long_decimal,lat_decimal) 
# Remove periods from CD. (Ciudad)
loc_ids$locality <- gsub("\\.", "", loc_ids$locality)

loc_ids %>% filter(municipio == "PUEBLO VIEJO") 

# load permit data with rnp info
permits <- read.csv(paste0(dir$cleandata,"commercial_permits_processed.csv"))
permits$RNP <- as.numeric(permits$RNP)

# only choose necesary columns
p_loc <- permits %>% select(state,state_id,municipio,mun_id,locality,local_id,RNP)


# load propez data with rnp info
propez <- read.csv(paste0(dir$cleandata,"propez_data_unfinished.csv"))

pro_loc <- propez %>% select(RNP,vessel_RNP,state_id, state,mun_id,municipio,local_id,locality, full_name) %>%
  unique()


# load other datasets with RNPS in the 2011's
modernacion <- read.csv(paste0(dir$rawdata,"subsidio_modernizacion_de_embarcaciones_2011_2019.csv"))

capacitacion <- read.csv(paste0(dir$rawdata,"subsidios_capapictacion_2011_2019.csv"))

inspeccion <- read.csv(paste0(dir$rawdata,"subsidio_inspeccion_vigilacia_2011_2018.csv"))

ordenamiento <- read.csv(paste0(dir$rawdata,"subsidio_ordenamiento_2011_2019.csv"))

estudios <- read.csv(paste0(dir$rawdata,"subsidio_obras_estudios_2011_2019.csv"))

cadenas <- read.csv(paste0(dir$rawdata,"subsidio_cadenas_productivas_2011_2019.csv"))

##### Translating, matching to loc_ids -----------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# rename columns
colnames(fuel) <- c("year","program","component","fuel_type",
                    "activity","subsidy","recepient_full_name","sex",
                    "coverage","zone","state","state_id",
                    "municipio","mun_id","locality","cve_loc",
                    "local_id","n_mayor_emb","n_minor_emb","n_aquaculture_inst",
                    "fishery","RNPA_disponible","econ_unit","rnpa",
                    "classification","ID")




# add an obs id
# make an obs_id
fuel$obs_id <- 1:nrow(fuel)


# for all character columns, make sure you remove accentos, make upper case, and enies
fuel[c(2:11,13,14,21)] <- lapply(fuel[c(2:11,13,14,21)],  function(x) stri_trans_general(x, "Latin-ASCII"))
fuel[c(2:11,13,14,21)] <- lapply(fuel[c(2:11,13,14,21)],  function(x) gsub("[Ññ]", "n", x))
fuel[c(2:11,13,14,21)] <- lapply(fuel[c(2:11,13,14,21)], toupper)
fuel$locality <- gsub("\\.", "", fuel$locality) # remove periods in (CD.)


# there are some locality with parenthesis, the parenthesis is the correct,
# remove everything outside of parenthesis and remove the parenthesis themselves

fuel %>% filter(obs_id == 300)

fuel$locality <- str_extract(fuel$locality, "\\([^()]*\\)") |>     # extract "( ... )"
  str_replace_all("[()]", "")   


# if locality or mun is "", "LOCALIDAD SIN NOMBRE", make into NA
fuel$locality <- ifelse(fuel$locality %in% c("","LOCALIDAD SIN NOMBRE","NINGUNO"),
                        NA,fuel$locality)
fuel$municipio <- ifelse(fuel$municipio == "", NA, fuel$municipio)



fuel %>% filter(obs_id == 430)


#  ~~~~~~~~~~~~~ FIX STATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check state names
unique(fuel$state)
unique(fuel %>% select(state,state_id))

# check state names (ids) in loc_ids
unique(loc_ids$state)
unique(loc_ids %>% select(state,state_id))


# fix veracruz, Michoacan, 
fuel$state <- ifelse(fuel$state == "VERACRUZ",
                             "VERACRUZ DE IGNACIO DE LA LLAVE",
                             fuel$state)

fuel$state <- ifelse(fuel$state == "MICHOACAN",
                     "MICHOACAN DE OCAMPO",
                     fuel$state)

# match the state_ids to loc_ids
state_ids <- unique(loc_ids %>% select(state,state_id))

fuel <- left_join(fuel,state_ids, by = "state")

fuel <- fuel %>% select(-state_id.x) %>% rename("state_id" = "state_id.y")

# make mun_id a numeric
fuel$mun_id <- as.numeric(fuel$mun_id)

# select the proper order to best see comparison
fuel <- fuel %>%
  select(year,state_id,state,mun_id,municipio,local_id,locality,rnpa, obs_id, everything()) %>%
  arrange(year)

# ~~~~~~~~~~~~~~~~~~~~~~~ IDENTIFY THOSE WITHOUT MUN/LOC IDENTIFIERS/  ~~~~~~~~~~~~~~~~~~~~

##### MATCH TO A GEOGRAPHIC LOCATION ----------------------------------------------------


### match 1, use the loc_ids to match by state and locality (munis can change over time) ###

match1 <- left_join(fuel, loc_ids, by = c("state","locality"))

# detected many-to-many
match1_good <- match1 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate( state_id =  state_id.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id = local_id.y) %>% # keep the new
  group_by(obs_id) %>%
  slice_tail(n = 1) %>% # keep the last combo (typically latest muni)
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

unmatch <- fuel[fuel$obs_id %not_in% match1_good$obs_id,] # work with unmatched

### match 2: match by RNP from permit (p_loc) then to loc_ids #~~~~~~~~~~~~~~~~~~~~~~~~

match2 <- left_join(unmatch, p_loc, by = c("rnpa"= "RNP"))

# detected many-to-many
match2 <- match2 %>%
  filter(state.x == state.y) %>%
  mutate(
    state     = state.y,
    state_id  = state_id.y,
    mun_id    = mun_id.y,
    municipio = municipio.y,
    local_id  = local_id.y,
    locality  = locality.y
  ) %>%
  group_by(obs_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
select(year, state_id, state, mun_id, municipio,
       local_id, locality, rnpa, obs_id, everything())

# join by state and locality for this one (the issue is that municipios can change over time)
match2 <- left_join(match2, loc_ids, by = c("state","locality"))

# detected many-to-many
match2_good <- match2 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate( state_id =  state_id.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id = local_id.y) %>% # keep the new
  group_by(obs_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns 
select(year, state_id, state, mun_id, municipio,
       local_id, locality, rnpa, obs_id, everything())


unmatch <- unmatch[unmatch$obs_id %not_in% match2_good$obs_id,] # work with unmatched # work with the ids not in match2_good

nrow(fuel) - nrow(match1_good) - nrow(match2_good) == nrow(unmatch)

# ## match 3: match by local_id  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

match3 <- left_join(unmatch, loc_ids, by = c("local_id"))

# no many-to-many detected
match3_good <- match3 %>%
  filter(!is.na(locality.y)) %>% # keep the good matches by locality
  mutate( state =  state.y ,
          state_id = state_id.y,
          mun_id = mun_id.y,
          municipio = municipio.y,
          locality = locality.y
  ) %>%
  select(colnames(match1_good)) # reorganize 

unmatch <- unmatch[unmatch$obs %not_in% match3_good$obs_id,] # work with the ids not in match2_good


# match 4: match by rnp in prior (match1_good,match2_good, match3_good)  ~~~~~~~~~~~~~~~~~~~~~~~~~~

full_match <- rbind(match1_good,match2_good,match3_good)

match4 <- left_join(unmatch, unique(full_match %>% 
                      select(state_id, state, mun_id, municipio,
                             local_id, locality, rnpa,long_decimal,lat_decimal)),
                    by = "rnpa")

# detected many-to-many
match4_good <- match4 %>%
  filter(!is.na(long_decimal)) %>%
  mutate( state =  state.y ,
          state_id = state_id.y,
          mun_id = mun_id.y,
          municipio = municipio.y,
          locality = locality.y,
          local_id = local_id.y
  ) %>% # keep the municipio that is new
  group_by(obs_id) %>%
  slice_tail(n = 1) %>% # keep the last join in many-to-many relationship
  ungroup()  %>%
  select(colnames(match1_good)) # reorganize 

unmatch <- unmatch[unmatch$obs %not_in% match4_good$obs_id,] # work with the ids not in match2_good


# match 5: match by rnp in propez then join to loc_ids  ~~~~~~~~~~~~~~~~~~~~~~~~~~

match5 <- left_join(unmatch, pro_loc, by = c("rnpa" = "RNP"))

# detected many-to-many
match5 <- match5 %>%
  filter(state.x == state.y) %>%
  mutate(
    state     = state.y,
    state_id  = state_id.y,
    mun_id    = mun_id.y,
    municipio = municipio.y,
    local_id  = local_id.y,
    locality  = locality.y
  ) %>%
  group_by(obs_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

# join by state and locality for this one (the issue is that municipios can change over time)
match5 <- left_join(match5, loc_ids, by = c("state","locality"))

# detected many-to-many
match5_good <- match5 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate( state_id =  state_id.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id = local_id.y) %>% # keep the new
  group_by(obs_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns 
  select(colnames(match1_good)) # reorganize 


unmatch <- unmatch[unmatch$obs_id %not_in% match5_good$obs_id,] # work with unmatched 

# match 6: match by recipient from previous obs  ~~~~~~~~~~~~~~~~~~~~~~~~~~
nrow(unmatch[unmatch$recepient_full_name == "",])

full_match <- rbind(match1_good,match2_good,match3_good,match4_good,match5_good)

match6 <- left_join(unmatch, 
                    unique(full_match %>% 
                            select(state_id, state, mun_id, municipio,
                                   local_id, locality, recepient_full_name,long_decimal,lat_decimal)),
                    by = "recepient_full_name")

# detected many-to-many
match6_good <- match6 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          locality = locality.y,
          local_id = local_id.y) %>% # keep the new
  group_by(obs_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns 
  select(colnames(match1_good)) # reorganize 



nrow(unmatch[unmatch$obs %not_in% match6_good$obs_id,])

unmatch <- unmatch[unmatch$obs %not_in% match6_good$obs_id,] # work with the ids not in match2_good

# match 7: match by municipio and assign cabezero (fist city in the municipio)  ~~~~~~~~~~~~~~~~~~~~~~~~~~

# at this point any locality info is causing a mismatch, so imputing the cabezero is the best course
nrow(unmatch[unmatch$rnpa %in% p_loc$RNP,])
nrow(unmatch[unmatch$rnpa %in% pro_loc$RNP,])
nrow(unmatch[unmatch$municipio %in% loc_ids$municipio,])
nrow(unmatch[unmatch$municipio %in% p_loc$municipio,])

# match by municipio


match7 <- left_join(unmatch, loc_ids, by = c("state","municipio"))


# detected many-to-many
match7_good <- match7 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate(
         state_id = state_id.y,
         mun_id = mun_id.y,
         locality = locality.y,
         local_id = local_id.y) %>% # keep the new locality
  group_by(obs_id) %>%
  slice_tail(n= 1) %>% # keep the smallest local_id (usually the cabezero)
  ungroup() %>%
  select(colnames(match1_good)) # reorganize 


nrow(unmatch[unmatch$obs %not_in% match7_good$obs_id,])

unmatch <- unmatch[unmatch$obs %not_in% match7_good$obs_id,] # work with the ids not in match2_good

### match 8: whatever to match these last 1059 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nrow(unmatch[unmatch$rnpa %in% modernacion$rnpa,])
nrow(unmatch[unmatch$rnpa %in% capacitacion$rnpa,])
nrow(unmatch[unmatch$rnpa %in% ordenamiento$rnpa,])
nrow(unmatch[unmatch$rnpa %in% cadenas$rnpa,])
nrow(unmatch[unmatch$rnpa %in% estudios$rnpa,])

# use modernacion for a match in RNP
modernacion$rnpa <- as.numeric(modernacion$rnpa)
match8 <- left_join(unmatch, modernacion, by =c("rnpa"))

# detected many-to-many
match8_good <- match8 %>%
  filter(!is.na(municipio.y)) %>% # filter out any non match by missing mun_id
  mutate(
         mun_id = cve_mun,
         municipio = municipio.y,
         locality = localidad,
         local_id = cve_loc.y,
         cve_loc = cve_loc.x,
         RNPA_disponible = RNPA_disponible.x) %>% # keep the new locality
  group_by(obs_id) %>%
  slice_tail(n= 1) %>% # keep the smallest local_id
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  select(colnames(unmatch))


match8 <- left_join(match8_good, loc_ids, by = c("state","locality"))


# detected many-to-many
match8_good <- match8 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate(
    state_id = state_id.x,
    municipio = municipio.y,
    mun_id = mun_id.y,
    local_id = local_id.y) %>% # keep the new locality
  group_by(obs_id) %>%
  slice_tail(n= 1) %>% # keep the smallest local_id (usually the cabezero)
  ungroup() %>%
  select(colnames(match1_good)) # reorganize 



unmatch <- unmatch[unmatch$obs %not_in% match8_good$obs_id,] # work with the ids not in match8_good

#### match 9 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# 410 obs without a municipio or locality
full_match <- rbind(match1_good,match2_good,match3_good,match4_good,match5_good,match6_good,match7_good,match8_good)


nrow(unmatch[unmatch$year %in% c(2011,2012,2013),]) # majority of missing data is years before 2014
# USE ID?
nrow(unmatch[unmatch$ID %in% full_match$ID,])
nrow(unmatch[unmatch$recepient_full_name %in% full_match$recepient_full_name,])
nrow(unmatch[unmatch$recepient_full_name %in% modernacion$beneficiario,])
nrow(unmatch[unmatch$recepient_full_name %in% modernacion$beneficiario,])


match9 <- left_join(unmatch, modernacion, by =c("recepient_full_name" = "beneficiario"))

# detected many-to-many
match9_good <- match9 %>%
  filter(!is.na(municipio.y)) %>% # filter out any non match by missing mun_id
  mutate(
    mun_id = cve_mun,
    municipio = municipio.y,
    locality = localidad,
    local_id = cve_loc.y,
    cve_loc = cve_loc.x,
    rnpa = rnpa.x,
    RNPA_disponible = RNPA_disponible.x) %>% # keep the new locality
  group_by(obs_id) %>%
  slice_tail(n= 1) %>% # keep the smallest local_id
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  select(colnames(unmatch))


match9 <- left_join(match9_good, loc_ids, by = c("state","locality"))


# detected many-to-many
match9_good <- match9 %>%
  filter(!is.na(long_decimal)) %>% # filter out any non match by missing long_decimal
  mutate(
    state_id = state_id.x,
    municipio = municipio.y,
    mun_id = mun_id.y,
    local_id = local_id.y) %>% # keep the new locality
  group_by(obs_id) %>%
  slice_tail(n= 1) %>% # keep the smallest local_id (usually the cabezero)
  ungroup() %>%
  select(colnames(match1_good)) # reorganize 



unmatch <- unmatch[unmatch$obs %not_in% match9_good$obs_id,] # work with the ids not in match9_good


# only 385 obs without any form of location identifier other than the state
nrow(unmatch)

#### combine the good observations 

match_good <- rbind(match1_good,match2_good,match3_good,match4_good,match5_good,match6_good,match7_good, match8_good,match9_good)

# export
write.csv(match_good,
          paste0(dir$cleandata, "gas_diesel_subsidies_w_coords.csv"),
          row.names = F)



# divide by fuel type
gas_r <- match_good %>% filter(fuel_type == 'GASOLINA RIBERENA')
diesel_m <- match_good %>% filter(fuel_type == 'DIESEL MARINO')


# summarize by locality and year
gas_sum <- gas_r %>%
  group_by(year,state_id,state,mun_id,municipio,local_id,locality, long_decimal,lat_decimal) %>%
  summarize(
    subsidy_mxn_value = sum(as.numeric(subsidy), na.rm = T)
  ) 

diesel_sum <- diesel_m %>%
  group_by(year,state_id,state,mun_id,municipio,local_id,locality, long_decimal,lat_decimal) %>%
  summarize(
    subsidy_mxn_value = sum(as.numeric(subsidy), na.rm = T)
  ) 

# export the summaries
write.csv(gas_sum,
          paste0(dir$cleandata, "gas_ribereno_summarized_year_locality_w_coords.csv"),
          row.names = F)

write.csv(diesel_sum,
          paste0(dir$cleandata, "diesel_marino_summarized_year_locality_w_coords.csv"),
          row.names = F)


##### Match the fuel subsidy to an office  ----------------------------------------------------------------
# load the gas and diesel data with coords
gas_df <- read.csv(paste0(dir$cleandata,"gas_ribereno_summarized_year_locality_w_coords.csv"))
diesel_df <- read.csv(paste0(dir$cleandata,"diesel_marino_summarized_year_locality_w_coords.csv"))


# load the office sf 
office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")


# make gas and diesel an sf object
gas_sf <- st_as_sf(gas_df, coords = c("long_decimal","lat_decimal"), crs = crs(office_sf))
diesel_sf <- st_as_sf(diesel_df, coords = c("long_decimal","lat_decimal"), crs = crs(office_sf))

# to save computational power, join them, then use the observations unique by locality
fuel_sf <- rbind(gas_sf,diesel_sf) %>%
  select(state_id,state,mun_id,municipio,local_id,locality,geometry) %>%
  unique()

# add an identifier
fuel_sf$fuel_id <- 1:nrow(fuel_sf)

############################################################################################################################~
## put fuel_sf through the office matching machine ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vectorized approach. go from 10, 20, 30 km radius. Anything that doesn't match beyond this would use gmapdistance

# Define buffers
buffer_distances <- c(0, 10000, 20000, 30000)  # in meters

# Create list of buffered bp_unique_sf for each distance
buffers_list <- lapply(buffer_distances, function(dist) {
  if(dist == 0) {
    fuel_sf %>%
      mutate(buffer_id = dist)
  } else {
    fuel_sf %>%
      st_buffer(dist) %>%
      mutate(buffer_id = dist)
  }
})

# Combine all buffers into one big sf object
buffers_all <- do.call(rbind, buffers_list)

# Do a spatial join: offices joined to buffered locations
joined <- st_join(buffers_all, office_sf, join = st_intersects, left = TRUE, suffix = c("","_gas"))

# Remove rows with no office found
matched <- joined %>% filter(!is.na(office_id))

# If multiple matches, keep only the smallest buffer distance per location_id
matched <- matched %>%
  group_by(state_id, state, mun_id,municipio,local_id,locality, fuel_id) %>%
  slice_min(buffer_id, with_ties = F) %>% # keeps the row with smallest buffer radius, breaking ties automatically.
  ungroup()


# keep only the gas location and office id, no need to keep geometry
office_match <- st_drop_geometry(matched[c("state_id","state","mun_id","municipio","local_id","locality","office_id","office","fuel_id")])

# using gmapdistance for those beyond 30 km

# Subset unmatched locations
unmatched <- fuel_sf %>% filter(fuel_id %not_in% office_match$fuel_id)

# Prepare office coordinates
office_coords <- st_coordinates(office_sf)
office_sf <- office_sf %>%
  mutate(lon = office_coords[,1], lat = office_coords[,2])

# Loop through unmatched bp locations 
for(i in 1:nrow(unmatched)) { # i <- 2
  
  origin_geom <- unmatched$geometry[i]
  origin_coords <- st_coordinates(origin_geom)
  origin_lon <- origin_coords[1]
  origin_lat <- origin_coords[2]
  
  # Calculate Euclidean distance to all offices
  dists <- st_distance(origin_geom, office_sf$geometry)
  #
  
  # Subset offices within 75 km radius
  within_75km <- which(as.numeric(dists)  <= 75000)

  if(length(within_75km) == 0){
    # No offices within 75 km — assign NA
    unmatched$office[i] <- NA
    unmatched$office_id[i] <- NA
  } else {
    # Prepare vectors for gmapdistance call
    destinations <- paste(office_sf$lat[within_75km], office_sf$lon[within_75km], sep = ",")
    origin <- paste(origin_lat, origin_lon, sep = ",")

    # Apply gmapdistance for all nearby offices
    dist_vec <- numeric(length(destinations))
  }
    for(j in seq_along(destinations)){ # j <- destinations
      # Call Google API
      result <- gmapsdistance(origin = origin,
                              destination = destinations[j],
                              mode = "driving",
                              key = "AIzaSyBSDg3nhqvSazvXjfSrye-UgZyjZb3VkAs") # use wisely!!!
      dist_vec[j] <- result$Distance
    }

    # remove the NA's from dist_vec (those that were unmappable)
    dist_vec <- dist_vec[!is.na(dist_vec)]

    # When the location is unmappable make the unmatched office NA
    if(length(dist_vec) == 0){
      unmatched$office[i] <- NA
      unmatched$office_id[i] <- NA
    } else {

  # Select office with minimum driving distance
  min_idx <- which.min(dists)
  
  unmatched$office[i] <- office_sf$office[min_idx]
  unmatched$office_id[i] <- office_sf$office_id[min_idx]
   }
}
 
# identify those that were mathched by gmap distance
gmap_matched <- unmatched[!is.na(unmatched$office_id),] # those with office id that is not NA


# verify if any missing, put them in unmatched
unmatched <- unmatched[is.na(unmatched$office_id),]


# these will match by cardinal 
# Loop through unmatched bp locations 
for(i in 1:nrow(unmatched)) { # i <- 2
  
  origin_geom <- unmatched$geometry[i]
  origin_coords <- st_coordinates(origin_geom)
  origin_lon <- origin_coords[1]
  origin_lat <- origin_coords[2]
  
  # Calculate Euclidean distance to all offices
  dists <- st_distance(origin_geom, office_sf$geometry)

  # Select office with minimum euclidian distance
  min_idx <- which.min(dists)
  
  unmatched$office[i] <- office_sf$office[min_idx]
  unmatched$office_id[i] <- office_sf$office_id[min_idx]

}


# Combine everything into one 
final_match <- bind_rows(office_match ,st_drop_geometry(gmap_matched),st_drop_geometry(unmatched))

# join the gas_final_match to fuel_sf

fuel_office_final <- left_join(fuel_sf,
                              final_match %>% select(-fuel_id),
                              by = c("state","state_id","municipio","mun_id","locality","local_id")) %>%
  st_drop_geometry()


# check that all the obs match
fuel_office_final[is.na(fuel_office_final$office_id),]


# use fuel_office_final to give an office to gas_df 
gas_office <- left_join(gas_df,
                        fuel_office_final %>% select(-fuel_id),
                        by = c("state_id","state","mun_id","municipio","local_id","locality"))


# save this 
write.csv(gas_office,
          paste0(dir$cleandata,"gas_ribereno_summarized_with_office.csv"),
          row.names = F)


# use fuel_office_final to give an office diesel_df
diesel_office <- left_join(diesel_df,
                        fuel_office_final %>% select(-fuel_id),
                        by = c("state_id","state","mun_id","municipio","local_id","locality"))


# save this 
write.csv(diesel_office,
          paste0(dir$cleandata,"diesel_marino_summarized_with_office.csv"),
          row.names = F)





#####
#####
#####
#####
