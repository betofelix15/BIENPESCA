# Purpose: Map exposure to production
# Author: Jesus Felix
# Start date: 10 June 2025
################################################################################
#### 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","raster","foreign","RColorBrewer","rgdal", "tidygeocoder",
          "maptools","scales","mapview", "sf","readxl","sqldf","geojsonio",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi",
          "zoo","gmapsdistance","rmapshaper", "gridExtra","tigris","rgee","reticulate")

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




# 1. Load data -----------------------------------------------------------------------------

# load production data
prod <- read.csv(paste0(dir$cleandata,"final_production_no_duplicates.csv"))

# load bp data
bp <- read.csv(paste0(dir$cleandata,"bp_propez_aggregated.csv"))

# load location data for localities
loc_geo <- read.csv(paste0(dir$cleandata,"locality_location_data.csv"))

# load the bienes_inmuebles (data that holds the office address)
inmuebles <- read.csv(paste0(dir$rawdata,"Bienes_Inmuebles_CONAPESCA.csv"),
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      blank.lines.skip = TRUE,
                      fileEncoding = "Latin1")

# load the office location data (if this proves difficult)
#office_sf <- read_sf(paste0(dir$shp,"office_points_7_feb_2025.shp")) # created in old_clean_production_data_v2

# for starting at 4. connect bp to offices
office_sf <- st_read(paste0(dir$shp, "office_points_latest.shp"))
bp_sf <- st_read(paste0(dir$shp, "bp_locations.shp"))
office_loc_df <- read.csv(paste0(dir$cleandata, "office_points_clean.csv"))
bp_loc_df <- read.csv(paste0(dir$cleandata, "bp_locations_clean.csv"))

# 2. Fix inmuebles ----------------------------------------------------------------------------------------------
inmuebles <- rename(inmuebles,
                    "year"    = 1,
                    "office_name"    = 2,
                    "office_type"   = 3,
                    "address"    = 4,
                    "mun_id"   = 5,
                    "municipio"   = 6,
                    "state_id"  = 7,
                    "state"   = 8)

# make inmuebles uppercase
inmuebles$office_name <- toupper(inmuebles$office_name)
inmuebles$municipio <- toupper(inmuebles$municipio)
inmuebles$state <- toupper(inmuebles$state)
inmuebles$office_type <- toupper(inmuebles$office_type)

# remove any accents in inmuebles
inmuebles <- inmuebles %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
inmuebles <- inmuebles %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

# extract the office
patterns <- c("OFICINA FEDERAL DE PESCA ", "CENTRO ACUICOLA ", "OFICINA DE PESCA FEDERAL ", "OFICINA  FEDERAL DE PESCA ",
              "SUBDELEGACION DE PESCA ", "OFICINA DE PESCA PIP ", "TERRENO PARA OFICINA FEDERAL DE PESCA EN ",
              "OFICINA DE PESCA ", "OFICINA REGIONAL DE PESCA ", "OFICINA DEL COMISIONADO ",", ESTADO DE MEXICO","CENTRO ACU!COLA ",
              "OFICINA DE PESCA EN ","(EN LITIGIO) (INVADIERON EN LITIGIO JUZGADO FEDERAL)","SUBDELEGACION ",
              "OFICINA ","OFIDCINA DE PESCA ","OFIICNA DE PESCA EN ","OFINA DE PESCA EN ", "  EN LITIGIO","OFICINA REGIONAL ")

pattern_combined <- paste(patterns, collapse = "|")

inmuebles$office <- gsub(pattern_combined, "", inmuebles$office_name)

# change CD. to CIUDAD
inmuebles$office <- gsub("\\bCD\\.\\s+(?=\\w)", "CIUDAD ", inmuebles$office, perl = TRUE)

# change PTO. to PUERTO
inmuebles$office <- gsub("\\bPTO\\.\\s+(?=\\w)", "PUERTO ", inmuebles$office, perl = TRUE)

# REMOVE WHITE SPACE
inmuebles$office <- trimws(inmuebles$office)
inmuebles$municipio <- trimws(inmuebles$municipio)

# FIX PENASCO
inmuebles <- inmuebles %>%
  mutate(across(where(is.character), ~ str_replace(.,"¤","N")))
inmuebles <- inmuebles %>%
  mutate(across(where(is.character), ~ str_replace(.,"","")))

# fix mun_id
inmuebles$mun_id <- paste0(inmuebles$state_id, sprintf("%03d", inmuebles$mun_id)) # add three zero padding

# slight changes to office
df <- inmuebles %>%
  mutate(office = case_when(
    office == "ESTADO DE MEXICO" ~ "ZINACANTEPEC",
    office == "ZINANCANTEPEC" ~ "ZINACANTEPEC",
    office %in% c("ACAPULCO, GUERRERO","ACAPULCO GUERRERO") ~ "ACAPULCO",
    office == "BAHIA DE TORTUGAS" ~ 'BAHIA TORTUGAS',
    office == "EL PATASTE (NO HAY CENTRO)" ~ "EL PATASTE",
    office == "TULUM (EN LITIGIO) (INVADIERON EN LITIGIO JUZGADO FEDERAL)" ~ "TULUM",
    office == "AGUSCALIENTES" ~ "AGUASCALIENTES",
    TRUE ~ office
  ))
inmuebles <- df
# make a unique list that uses the last year
inmuebles_unique <- inmuebles %>%
  group_by(state_id,state,office) %>%
  slice_max(order_by = year, n = 1) %>%
  ungroup()

# 
# # see the offices in prod
#  
# 
# # if the office_id ends in 01, then the locality will be the one first one per municipio/state
# office_id$last_4 <- str_extract(office_id$office_id, "\\d{2}$")
# office_id$last_4 <- str_pad(office_id$last_4, width = 4, pad = "0")
# 
# # try to match the municipios
# office_join <- left_join(office_id, inmuebles_unique, by = c("state_id","state","office"))
# office_join <- office_join[!is.na(office_join$municipio),]
# office_join$local_id <- as.numeric(paste0(office_join$mun_id,office_join$last_4))
# office_join$mun_id <- as.numeric(office_join$mun_id)
# 

# 2. Locations for offices --------------------------------------------------------------------------------

# check that states are the same
# unique(prod$state)
# unique(loc_geo$state)

# MAKE A UNIQUE LIST FOR PROD
office_id <- unique(prod[c("state_id","state","office_id","office")])

# give these a name for the locality using office names
office_id$locality <- office_id$office

# match umatched with loc_geo by state and locality == MUNICIPIO, its more common that the municipio name matches the office than the locality
mun_matched <- NULL
for(i in as.list(office_id$office_id)){ # i <- 101
  
  temp <- office_id[office_id$office_id == i,c(1:5)]
  
  match <- left_join(temp, loc_geo, by = c("state_id","state","locality" = "municipio"), suffix = c("_a",""))
  
  if(nrow(match) == 0){next} # if there is no match go next
  
  match <- match[1,] # the first observation is the main city in the municipio by the same name but with slight variations
  
  match$municipio <- match$office
  
  mun_matched <- rbind(mun_matched,match)
}

unmatched <- mun_matched[is.na(mun_matched$local_id),c(1:5)] # identify the unmatched

unmatched$locality <- unmatched$office

mun_matched <- mun_matched[mun_matched$office_id %not_in% unmatched$office_id,]

# match with loc_geo by state and locality == locality
office_match <- left_join(unmatched, loc_geo, by = c("state_id","state","locality"))

# use the ones that are not na
office_match <- office_match[!is.na(office_match$local_id),]

# fix the remaining unmatched manually
list_matched <- c(mun_matched$office_id,unique(office_match$office_id))
unmatched <- office_id[office_id$office_id %not_in% list_matched,]

unmatched$locality <- c("SANTO DOMINGO","CATAZAJA","EL ENCANTO","SABINAS","ACAPULCO DE JUAREZ","MARQUELIA","PACHUCA DE SOTO","COJUMATLAN DE REGULES",
                        "NUEVA ITALIA DE RUIZ","LA PENITA DE JALTEMBA","HEROICA CIUDAD DE JUCHITAN DE ZARAGOZA","SANTIAGO PINOTEPA NACIONAL",
                        "KINO","VILLAHERMOSA","CUAUHTEMOC","JUAREZ","CHILTEPEC (PUERTO CHILTEPEC)")

fix_match <- left_join(unmatched,loc_geo, by = c("state_id","state","locality"))

# join all of the matches
all_match <- rbind(mun_matched,office_match,fix_match)

# find duplicate offices
dup_match <- all_match %>%
  group_by(state_id, state, office_id, office) %>%
  mutate(num_sim_obs = n(), 
         dup_id = row_number())%>% 
  ungroup() %>% 
  mutate(is_duplicate = dup_id > 1)

final_match <- dup_match[dup_match$is_duplicate == F,]

dup_match <- dup_match[dup_match$is_duplicate == T,]

dups <- final_match[final_match$office_id %in% dup_match$office_id,]

final_match <- final_match[final_match$office_id %not_in% dups$office_id,]

dup_match <- rbind(dup_match,dups)

unique(dup_match$office)

# use the municipios in inmuebles

inmuebles_match <- left_join(dup_match,inmuebles_unique, by = c("state_id","state","office"), suffix = c("","_real"))

inmuebles_match <- inmuebles_match[inmuebles_match$municipio == inmuebles_match$municipio_real,]

inmuebles_match <- inmuebles_match[!is.na(inmuebles_match$office_type),c(1:23)]

# join with final_match
final_match <- final_match[c(1:23)]

final_match <- rbind(final_match,inmuebles_match)

# select the first obs for the rest
dup_match <- dup_match[dup_match$office_id %not_in% final_match$office_id,]
dup_match <- dup_match[dup_match$dup_id == 1,]
dup_match <- dup_match[c(1:23)]

final_match <- rbind(final_match, dup_match) # final match has all the offices in office_id

# arrange
final_match <- arrange(final_match, office_id)

# save this
write.csv(final_match,
          paste0(dir$cleandata, "office_points_clean.csv"),
          row.names = F)

# turn to sf object, crs = 4269
office_sf <- st_as_sf(final_match, coords = c("long_decimal","lat_decimal"), crs = 4269)

plot(office_sf$geometry)

# save this
st_write(office_sf,
         paste0(dir$shp, "office_points_latest.shp"))

# 
rm(list = c("all_match","df","dup_match","dups","fix_match","inmuebles_match","office_match","match","matched","mun_matched","SQ","unmatched"))
# 3. Locations for bp_localities------------------------------------------------

# for bp, if the locality is missing, use the municipio name and add 0001 to the mun_id for local_id
bp$locality <- ifelse(is.na(bp$locality), bp$municipio, bp$locality)
bp$local_id <- ifelse(is.na(bp$local_id), as.numeric(paste0(bp$mun_id,"0001")),bp$local_id)

# FIX A FEW IDS
bp$state <- ifelse(bp$mun_id == 28003,"TAMAULIPAS",bp$state)
bp$state <- ifelse(bp$mun_id == 14022,"JALISCO",bp$state)
bp$state <- ifelse(bp$mun_id == 6002,"COLIMA",bp$state)
bp$state <- ifelse(bp$mun_id == 4001,"CAMPECHE",bp$state)

bp$state_id <- ifelse(bp$mun_id == 28003,28,bp$state_id)
bp$state_id <- ifelse(bp$mun_id == 14022,14,bp$state_id)
bp$state_id <- ifelse(bp$mun_id == 6002,6,bp$state_id)
bp$state_id <- ifelse(bp$mun_id == 4001,4,bp$state_id)


# make a unique list of localities
bp_unique <- unique(bp[c("state_id","state","mun_id","municipio","local_id","locality")])
bp_unique$obs_id <- 1:nrow(bp_unique)

# match bp to loc_geo up to local_id
id_match <- left_join(bp_unique,loc_geo, by = c("state_id","state","mun_id","municipio","local_id","locality"),
                      suffix = c("","_loc"))

id_match <- id_match[!is.na(id_match$obs_id_loc),] # remove NA's

matched_list <- id_match$obs_id

unmatched <- bp_unique[bp_unique$obs_id %not_in% matched_list,] # identify the unmatched

# match up to locality name
local_match <- left_join(unmatched,loc_geo, by = c("state_id","state","mun_id","municipio","locality"),
                         suffix = c("","_loc"))

local_match <- local_match[!is.na(local_match$cvegeo),] # remove NA's

matched_list <- unique(c(matched_list, local_match$obs_id)) # add the matched to matched_list

unmatched <- bp_unique[bp_unique$obs_id %not_in% matched_list,] # identify the unmatched

# match with local_id but without locality
id_only_match <- left_join(unmatched,loc_geo, by = c("state_id","state","mun_id","municipio","local_id"),
                         suffix = c("","_loc"))

id_only_match <- id_only_match[!is.na(id_only_match$cvegeo),] # remove NA's

matched_list <- unique(c(matched_list, id_only_match$obs_id)) # add the matched to matched_list

unmatched <- bp_unique[bp_unique$obs_id %not_in% matched_list,] # identify the unmatched

# match with municipio name only selecting the first city in the municipio, Cabezero de municipio
rownames(unmatched) <- 1:nrow(unmatched)
mun_matched <- NULL
for(i in 1:nrow(unmatched)){ # i <- 1
  
  temp <- unmatched[i,]
  
  temp_match <- left_join(temp, loc_geo, by = c("state_id","state","municipio"),
                          suffix =  c("","_loc"))
  
  # check to see if the locality matches locality_loc
  temp_loc_match <- temp_match[temp_match$locality == temp_match$locality_loc,]
  
  if(length(temp_loc_match$local_id) == 1){
    
    mun_matched <- rbind(mun_matched,temp_loc_match)
    
  }else{
  
  top_id <- as.numeric(paste0(unique(temp_match$mun_id),"0001"))
    
  temp_match <- temp_match[temp_match$local_id_loc == top_id,]  
  
  rownames(temp_match) <- 1:nrow(temp_match)
  
  temp_match[1,] # select the first observation
  
  mun_matched <- rbind(mun_matched,temp_match) # save
  
  }
  
}

mun_matched <- mun_matched[!is.na(mun_matched$local_id_loc),] # remove the NAs

matched_list <- unique(c(matched_list, mun_matched$obs_id)) # add the matched to matched_list

unmatched <- bp_unique[bp_unique$obs_id %not_in% matched_list,] # identify the unmatched

# match with mun_id only selecting the first city in the municipio, Cabezero de municipio
rownames(unmatched) <- 1:nrow(unmatched)
mun_id_match <- NULL
for(i in 1:nrow(unmatched)){ # i <- 5
  
  temp <- unmatched[i,]
  
  temp_match <- left_join(temp, loc_geo, by = c("state_id","state","mun_id"),
                          suffix =  c("","_loc"))
  
  # check to see if the locality matches locality_loc
  temp_loc_match <- temp_match[temp_match$locality == temp_match$locality_loc,]
  
  if(length(temp_loc_match$local_id) == 1){
   
     mun_id_match <- rbind(mun_id_match,temp_loc_match)
    
  }else{
  
  top_id <- as.numeric(paste0(unique(temp_match$mun_id),"0001"))
  
  temp_match <- temp_match[temp_match$local_id_loc == top_id,]
  
  mun_id_match <- rbind(mun_id_match,temp_match)
  
  }
}

mun_id_match <- mun_id_match[!is.na(mun_id_match$local_id_loc),] # remove the NAs

matched_list <- unique(c(matched_list, mun_id_match$obs_id)) # add the matched to matched_list

unmatched <- bp_unique[bp_unique$obs_id %not_in% matched_list,] # identify the unmatched


# there should be zero unmatched and now we can combine!
colnames(id_match)
colnames(local_match)
colnames(id_only_match)
colnames(mun_matched)
colnames(mun_id_match)

local_match <- local_match %>% dplyr::select(-local_id_loc)
id_only_match <- id_only_match %>% dplyr::select(-locality_loc)
mun_matched <- mun_matched %>% dplyr::select(-local_id_loc,-locality_loc,-mun_id_loc)
mun_id_match <- mun_id_match %>% dplyr::select(-local_id_loc,-locality_loc,-municipio_loc)

bp_match <- rbind(id_match,local_match,id_only_match,mun_matched,mun_id_match)

bp_match <- arrange(bp_match, local_id)

rownames(bp_match) <- 1:nrow(bp_match)

unmatched <- bp_unique[bp_unique$obs_id %not_in% bp_match$obs_id,] # confirmed none missing

# combine with bp
bp_location <- left_join(bp,bp_match, by = c("state_id","state","mun_id","municipio","local_id","locality"))

# # save this
# write.csv(bp_location,
#           paste0(dir$cleandata, "bp_locations_clean.csv"),
#           row.names = F)

# turn to sf object, crs = 4269
bp_sf <- st_as_sf(bp_location, coords = c("long_decimal","lat_decimal"), crs = 4269)

plot(bp_sf$geometry)

# # save this
# st_write(bp_sf,
#          paste0(dir$shp, "bp_locations.shp"))


# 4. Match BP locations to offices ----------------------------------------------------------------------

# fix the colnames for the sf (this always happens when you save sf objects for some reason)
colnames(bp_sf) <- c("program","year", "month",  "state_id","state"  ,"mun_id", "municipio","local_id","locality",
                     "n_male", "n_female","n_participants"  ,"total_supplied"  ,"obs_id" ,  "cvegeo"   ,"status",   "stat_abr",  "climate",
                     "latitud" , "longitud",  "altitud",  "lettr_d",  "population",  "male_pop",  "female_pop",  "opccupied_households",  "obs_id_loc",
                     "geometry")

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")

# make bp_unique_sf
bp_unique_sf <- unique(bp_sf[c("state_id","state","mun_id","municipio","local_id","locality","geometry")])

bp_unique_sf$bp_id <- 1:nrow(bp_unique_sf)


# vectorized approach. go from 10, 20, 30 km radius. Anything that doesn't match beyond this would use gmapdistance

# Define buffers
buffer_distances <- c(0, 10000, 20000, 30000,)  # in meters

# Create list of buffered bp_unique_sf for each distance
buffers_list <- lapply(buffer_distances, function(dist) {
  if(dist == 0) {
    bp_unique_sf %>%
      mutate(buffer_id = dist)
  } else {
    bp_unique_sf %>%
      st_buffer(dist) %>%
      mutate(buffer_id = dist)
  }
})

# Combine all buffers into one big sf object
buffers_all <- do.call(rbind, buffers_list)

# Do a spatial join: offices joined to buffered locations
joined <- st_join(buffers_all, office_sf, join = st_intersects, left = TRUE, suffix = c("","_pp"))

# Remove rows with no office found
matched <- joined %>% filter(!is.na(office_id))

# If multiple matches, keep only the smallest buffer distance per location_id
matched <- matched %>%
  group_by(state_id, state, local_id, locality,bp_id) %>%
  slice_min(buffer_id, with_ties = F) %>% # keeps the row with smallest buffer radius, breaking ties automatically.
  ungroup()

length(unique(matched$bp_id))

# keep only the bp location and office id, no need to keep geometry
bp_office_match <- st_drop_geometry(matched[c("state_id","state","mun_id","municipio","local_id","locality","bp_id","office_id","office")])

# using gmapdistance for those beyond 30 km

# Subset unmatched locations
unmatched <- bp_unique_sf %>% filter(bp_id %not_in% bp_office_match$bp_id)

# Prepare office coordinates
office_coords <- st_coordinates(office_sf)
office_sf <- office_sf %>%
  mutate(lon = office_coords[,1], lat = office_coords[,2])

# Loop through unmatched bp locations ( I had to start from 64 to start again after an error in loop)
for(i in 1:nrow(unmatched[c(64:nrow(unmatched)),])) { # i <- 2
  
  origin_geom <- unmatched$geometry[i]
  origin_coords <- st_coordinates(origin_geom)
  origin_lon <- origin_coords[1]
  origin_lat <- origin_coords[2]
  
  # Calculate Euclidean distance to all offices
  dists <- st_distance(origin_geom, office_sf$geometry)
  
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
    
    for(j in seq_along(destinations)){
      # Call Google API
      result <- gmapsdistance(origin = origin, 
                             destination = destinations[j], 
                             mode = "driving", 
                             key = "AIzaSyBHuXDlKFSWuLc6j7vSodMZSF8UHH4SJeQ") # use wisely!!!
      dist_vec[j] <- result$Distance
    }
    
    # remove the NA's from dist_vec (those that were unmappable)
    dist_vec <- dist_vec[!is.na(dist_vec)]
    
    # When the location is unmappable make the unmatche office NA
    if(length(dist_vec) == 0){
      unmatched$office[i] <- NA
      unmatched$office_id[i] <- NA
    } else {
    
    # Select office with minimum driving distance
    min_idx <- which.min(dist_vec)
    
    unmatched$office[i] <- office_sf$office[within_75km[min_idx]]
    unmatched$office_id[i] <- office_sf$office_id[within_75km[min_idx]]
  }
 }
}  

gmap_match <- unmatched[!is.na(unmatched$office),]

unmatched <- bp_unique_sf[bp_unique_sf$bp_id %not_in% c(gmap_match$bp_id,bp_office_match$bp_id),]


# these final unmatched will be connected by shortest euclidian distance. These were unmappable
rownames(unmatched) <- 1:nrow(unmatched)
last_match <- NULL

for(i in 1:nrow(unmatched)){ # i <- 1
  
  temp <- unmatched[i,]
  
  dist_vec <- st_distance(temp$geometry,office_sf$geometry) # get a euclidian distance vector for all the offices to this one
  
  closest_office <- which.min(dist_vec) # select the smallest distance
  
  temp$office <- office_sf[closest_office,]$office # save its office and office id into temp
  
  temp$office_id <- office_sf[closest_office,]$office_id
  
  last_match <- rbind(last_match,temp) # save in last_match
  
}


# Combine everything into one 
bp_office_final <- bind_rows(bp_office_match ,st_drop_geometry(gmap_match),st_drop_geometry(last_match))

# save this 
write.csv(bp_office_final,
          paste0(dir$cleandata,"bp_locations_matched_to_office.csv"),
          row.names = F)


# can we combine with the main bp data?
bp_full_office <- left_join(bp, bp_office_final, by = c("state_id","state","mun_id","municipio","local_id","locality"))

bp_full_office$obs_id <- 1:nrow(bp_full_office)
# for those without a local_id or locality data, use the municipio's main city and location
main_mun_loc <- bp_office_final %>%
  group_by(state_id,state,mun_id,municipio)%>%
  slice_min(local_id,with_ties = F) %>% # select the first local id (the smallest ending in 0001)
  ungroup()


bp_mun_only <- bp_full_office[is.na(bp_full_office$locality)|is.na(bp_full_office$local_id),]

bp_mun_only <- left_join(bp_mun_only,main_mun_loc, by = c("state_id","state","mun_id","municipio"), suffix = c("_bp",""))

# remove the columns that end in _bp
bp_mun_only <- bp_mun_only %>% select(!ends_with("_bp"))
bp_mun_only[is.na(bp_mun_only$office),]

bp_full_office <- bp_full_office[bp_full_office$obs_id %not_in% bp_mun_only$obs_id,]

bp_full_office <- rbind(bp_full_office,bp_mun_only)

# check unique offices and states
unique_office_bp <- unique(bp_full_office[c("state_id","state","office_id","office")])

# this is because the locations of bp can be outside of the office state and we have mismatching states. this is ok

# arrange
bp_full_office <- arrange(bp_full_office, year,month,state_id,state,mun_id, municipio, office_id, office)

# save this 
write.csv(bp_full_office,
          paste0(dir$cleandata,"agg_bp_data_with_office.csv"),
          row.names = F)

#bp_full_office <- read.csv(paste0(dir$cleandata,"agg_bp_data_with_office.csv"))



