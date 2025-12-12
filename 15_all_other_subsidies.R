# all other subsidies
# Dec 1 2025
# Jesus Felix De Los Reyes
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

#### LOAD DATA ------ ---------------------------------------------------------------------


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


# load all the subsidies that are not gas/propesca/bienpesca
modernacion <- read.csv(paste0(dir$rawdata,"subsidio_modernizacion_de_embarcaciones_2011_2019.csv"))

capacitacion <- read.csv(paste0(dir$rawdata,"subsidios_capapictacion_2011_2019.csv"))

# Inspections are not really subsidies or help, it was just money for investments in inspections
inspeccion <- read.csv(paste0(dir$rawdata,"subsidio_inspeccion_vigilacia_2011_2018.csv"))

# ordenamiento are not subsidies either
ordenamiento <- read.csv(paste0(dir$rawdata,"subsidio_ordenamiento_2011_2019.csv"))

# estudios 
estudios <- read.csv(paste0(dir$rawdata,"subsidio_obras_estudios_2011_2019.csv"))

cadenas <- read.csv(paste0(dir$rawdata,"subsidio_cadenas_productivas_2011_2019.csv"))

transformacion <- read.csv(paste0(dir$rawdata,"subsidio_transformacion_comercializacion_2014_2019.csv"))

acuacultura <- read.csv(paste0(dir$rawdata,"subsidio_desarollo_acuacultura_2014_2019.csv"))


####  FISHERIES TABLE ---------------------------------------------------------------

# the number the type of fish occured in a fishery for modernacion, amount of money 
mod_table <- sqldf("
  SELECT pesqueria AS fishery,
         COUNT(*) AS modern_n_obs,
         SUM(
           COALESCE(monto_conapesca,0) +
           COALESCE(monto_gob_edo,0) +
           COALESCE(monto_productor,0)
         ) AS modern_subsidy_total
  FROM modernacion
  GROUP BY fishery
  ORDER BY modern_n_obs DESC
")


acua_table <- sqldf("               
  SELECT pesqueria AS fishery,
         COUNT(*) AS acua_n_obs,
         SUM(
           COALESCE(monto_conapesca,0)
         ) AS acua_subsidy_total
  FROM acuacultura
  GROUP BY fishery
  ORDER BY acua_n_obs DESC
")


cadenas_table <- sqldf("               
  SELECT pesqueria AS fishery,
         COUNT(*) AS cadenas_n_obs,
         SUM(
           COALESCE(monto_conapesca,0)
         ) AS cadenas_subsidy_total
  FROM cadenas
  GROUP BY fishery
  ORDER BY cadenas_n_obs DESC
") 


cap_table <- sqldf("               
  SELECT pesqueria AS fishery,
         COUNT(*) AS cap_n_obs,
         SUM(
           COALESCE(monto_conapesca,0)
         ) AS cap_subsidy_total
  FROM capacitacion
  GROUP BY fishery
  ORDER BY cap_n_obs DESC
") 

tran_table <- sqldf("               
  SELECT pesqueria AS fishery,
         COUNT(*) AS tran_n_obs,
         SUM(
           COALESCE(monto_conapesca,0)
         ) AS tran_subsidy_total
  FROM transformacion
  GROUP BY fishery
  ORDER BY tran_n_obs DESC
") 

# join the tables
fishery_table <- full_join(mod_table,acua_table, by = "fishery")
fishery_table <- full_join(fishery_table, cadenas_table, by = "fishery")
fishery_table <- full_join(fishery_table, cap_table, by = "fishery")
fishery_table <- full_join(fishery_table, tran_table, by = "fishery")

# export
write.csv(fishery_table,
          paste0(dir$cleandata,"fishery_table_other_subsidies.csv"),
          row.names = F)

#### TRANSFORM DATA ----------------------------------------------------------------------------------

# keep columns that are necessary (summarize if necessary) and rename for full bind ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# modernacion will be m1
m1 <- modernacion %>%
  mutate(
    subsidy_mxn = rowSums(
      select(., monto_conapesca, monto_gob_edo, monto_productor),
      na.rm = TRUE
    ),
   rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, subsidy_mxn) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn
  )
  
  
# capacitacion will be c1
c1 <- capacitacion %>%
  mutate(
    subsidy_mxn = rowSums(
      select(., monto_conapesca, monto_gob_edo),
      na.rm = TRUE
    ),
    rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, subsidy_mxn) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn
  )  
  
# inspeccion will be in1
in1 <- inspeccion %>%
  mutate(
    rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_entidad,entidad,cve_mun_ue,municipio_ue,cve_loc_ue,localidad_ue,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_entidad,
    state = entidad,
    mun_id = cve_mun_ue,
    municipio = municipio_ue,
    local_id = cve_loc_ue,
    locality = localidad_ue,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  


# ordenamiento will be ord1
ord1 <- ordenamiento %>%
  mutate(
    rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_loc,localidad,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_loc,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  
  
  

# estudios will be e1
e1 <- estudios %>%
  mutate(
    rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  

# cadenas will be cad1
cad1 <- cadenas %>%
  mutate(
    rnpa = as.numeric(rnpa),
    unidadeconomica = NA
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconomica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconomica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  


# transformations will be t1
t1 <- transformacion %>%
  mutate(
    subsidy_mxn = rowSums(
      select(., monto_conapesca, monto_gob_edo),
      na.rm = TRUE
    ),
    rnpa = as.numeric(rnpa),
    unidadeconomica = NA
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconomica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconomica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  

# acuacultura will be a1
a1 <- acuacultura %>%
  mutate(
    rnpa = as.numeric(rnpa)
  ) %>%
  select(año,cve_ent,entidad,cve_mun,municipio,cve_inegi,localidad,
         rnpa,beneficiario,unidadeconmica,ID_ben,clasificacion_cn, monto_conapesca) %>%
  rename(
    year = año,
    state_id = cve_ent,
    state = entidad,
    mun_id = cve_mun,
    local_id = cve_inegi,
    locality = localidad,
    recipient_name = beneficiario,
    econ_unit = unidadeconmica,
    id_recipient = ID_ben,
    program = clasificacion_cn,
    subsidy_mxn = monto_conapesca
  )  


# now bind them together to make the cleaning process faster
other <- rbind(a1,c1,cad1,e1,in1,m1,ord1,t1)

# arrange
other <- arrange(other, year,state_id,mun_id,local_id)


# make an obs_id
other$obs_id <- 1:nrow(other)


# for all character columns, make sure you remove accentos, make upper case, and enies
other[c(3,5,7,9:12)] <- lapply(other[c(3,5,7,9:12)],  function(x) stri_trans_general(x, "Latin-ASCII"))
other[c(3,5,7,9:12)] <- lapply(other[c(3,5,7,9:12)],  function(x) gsub("[Ññ]", "n", x))
other[c(3,5,7,9:12)] <- lapply(other[c(3,5,7,9:12)], toupper)
other$locality <- gsub("\\.", "", other$locality) # remove periods in (CD.)


# there are some locality with parenthesis, the parenthesis is the correct,
# remove everything outside of parenthesis and remove the parenthesis themselves

other$locality <- str_extract(other$locality, "\\([^()]*\\)") |>     # extract "( ... )"
  str_replace_all("[()]", "")   


# if locality or mun is "", "LOCALIDAD SIN NOMBRE", make into NA
other$locality <- ifelse(other$locality %in% c("","LOCALIDAD SIN NOMBRE","NINGUNO"),
                        NA,other$locality)
other$municipio <- ifelse(other$municipio == "", NA, other$municipio)

other$state <- ifelse(other$state == "", NA, other$state)

#  ~~~~~~~~~~~~~ FIX STATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check state names
unique(other$state)
unique(other %>% select(state,state_id))

# check state names (ids) in loc_ids
unique(loc_ids$state)
unique(loc_ids %>% select(state,state_id))

# NICHE FIXES
# fix veracruz, Michoacan, coahuila, and a space in sinaloa
other$state <- ifelse(other$state == "VERACRUZ",
                     "VERACRUZ DE IGNACIO DE LA LLAVE",
                     other$state)

other$state <- ifelse(other$state == "MICHOACAN",
                     "MICHOACAN DE OCAMPO",
                     other$state)

other$state <- ifelse(other$state == "COAHUILA",
                      "COAHUILA DE ZARAGOZA",
                      other$state)

other$state <- ifelse(other$state == "SINALOA ",
                      "SINALOA",other$state)

# THERE IS A LOT OF STATES WITH MISMATCHING STATE_IDS
# need to confirm with locality if state is correct

# create a state_id from local_id
other$state_id_loc <- gsub('.{7}$', '', other$local_id)

# identify those that have the wrong state (always side with the locality, its typically correct)
st_mismatch <- other %>% filter(state_id != state_id_loc)
# just remove the one that has more than 2 values in the string
st_mismatch <- st_mismatch %>% filter(state_id_loc != 26071)

st_mismatch$state_id <- st_mismatch$state_id_loc

# fill them back in
other <- other  %>% filter(obs_id %not_in% st_mismatch$obs_id)

other <- rbind(other,st_mismatch)

# check state names
other$state_id <- as.numeric(other$state_id)
unique(other$state)
unique(other %>% select(state,state_id)) %>% arrange(state_id)

# the wrong state ids can be fixed through the matching up next

#### Match obs to geographic location---------------------------------------------------------------

other <- other %>% select(-state_id_loc)

#### if other has a local_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

match1 <- left_join(other, loc_ids, by = c("local_id"))

# no many-to-many detected
match1_good <- match1 %>% 
  filter(!is.na(long_decimal)) %>%
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          locality = locality.y) %>% # keep the new
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

# identify the ones that did not have a match
unmatch <- other %>% filter(obs_id %not_in%  match1_good$obs_id)

# see what is needed in unmatch
nrow(unmatch[!is.na(unmatch$locality),]) # so matching by locality may not be a good idea
nrow(unmatch[!is.na(unmatch$rnpa),]) # RNPA is a good candidate
nrow(unmatch[unmatch$rnpa %in% permits$RNP,]) # <- has the most matches
nrow(unmatch[unmatch$rnpa %in% propez$RNP,])
nrow(unmatch[unmatch$rnpa %in% match1_good$rnpa,]) 

#### match by RNPA in permits and propez ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

full_rnpa <- rbind(permits %>% select(state_id,state,mun_id,municipio,local_id,locality,RNP),
                   propez %>% select(state_id,state,mun_id,municipio,local_id,locality,RNP)
                   ) %>% 
  unique()


match2 <- left_join(unmatch %>% filter(!is.na(rnpa)),
                    full_rnpa,
                    by = c ("rnpa" = 'RNP'))

# many-to-many detected
match2 <- match2 %>% 
  filter(!is.na(locality.y)) %>%
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id= local_id.y,
          locality = locality.y) %>% # keep the new
  group_by(obs_id) %>% # select the first match
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

# now match to loc_ids for geographic location by state and locality (local_id's are missing)
match2 <- left_join(match2,loc_ids, by = c("state","locality"))

# many-to-many detected
match2_good <- match2 %>% 
  filter(!is.na(long_decimal)) %>%
  mutate( state_id =  state_id.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id= local_id.y) %>%
  group_by(obs_id) %>% # select the first match
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

# identify the ones that did not have a match
unmatch <- unmatch %>% filter(obs_id %not_in%  match2_good$obs_id)

# see what is needed in unmatch
nrow(unmatch[!is.na(unmatch$locality),]) # so matching by locality may not be a good idea
nrow(unmatch[!is.na(unmatch$rnpa),]) 
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% permits$RNP,])
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% propez$RNP,])
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% match1_good$rnpa,]) 
nrow(unmatch[!is.na(unmatch$municipio),])


#### Match by RNPA in match1_good & match2_good ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

full_match <- rbind(match1_good,match2_good)

full_match <- full_match %>% select(state_id, state, mun_id, municipio,
                                           local_id, locality, rnpa,long_decimal,lat_decimal) %>%
  filter(!is.na(rnpa)) %>%
  unique()


match3 <- left_join(unmatch,full_match,
                    by = "rnpa")

# many-to-many detected
match3_good <- match3 %>% 
  filter(!is.na(long_decimal)) %>%
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id= local_id.y,
          locality = locality.y) %>% # keep the new
  group_by(obs_id) %>% # select the first match
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

# identify the ones that did not have a match
unmatch <- unmatch %>% filter(obs_id %not_in%  match3_good$obs_id)


# lets check those state names again
full_match <- rbind(match1_good,match2_good,match3_good)
unique(full_match %>% select(state,state_id)) %>% arrange(state_id)
unique(unmatch %>% select(state,state_id)) %>% arrange(state_id)


# see what is needed in unmatch
nrow(unmatch[!is.na(unmatch$locality),]) # so matching by locality may not be a good idea
nrow(unmatch[!is.na(unmatch$rnpa),]) 
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% permits$RNP,])
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% propez$RNP,])
nrow(unmatch[!is.na(unmatch$rnpa) & unmatch$rnpa %in% match1_good$rnpa,]) 
nrow(unmatch[!is.na(unmatch$municipio),])

#### Add the cabezero for the locality (51 localities are obscure without match, replace those too) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cabezero <- unmatch

# some municipios are there without mun_id
miss_id <- cabezero %>% filter(is.na(mun_id) & !is.na(municipio))

miss_id$state <- ifelse(miss_id$municipio == "LA PAZ", "BAJA CALIFORNIA SUR", miss_id$state)
miss_id$state <- ifelse(miss_id$municipio == "ENSENADA", "BAJA CALIFORNIA", miss_id$state)
miss_id$mun_id <- ifelse(miss_id$municipio == "MEXICALI", 2002, miss_id$mun_id)
miss_id <- left_join(miss_id,
                     loc_ids %>% select(state,mun_id,municipio) %>% unique(), 
                     by = c("state","municipio"))

miss_id <- miss_id %>%
  filter(!is.na(mun_id.y)) %>%
  mutate( 
          mun_id = mun_id.y) %>% # keep the new
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())

cabezero <- cabezero %>% filter(obs_id %not_in% miss_id$obs_id) # add the new ids to cabezero
cabezero <- rbind(cabezero,miss_id)
cabezero$mun_id <- ifelse(cabezero$municipio == "MEXICALI", 2002, cabezero$mun_id)

# use the 0001 id for the cabezero
cabezero$local_id <- as.numeric(paste0(cabezero$mun_id,"0001"))

cabezero %>% filter(is.na(local_id)) %>% nrow()
cabezero %>% filter(is.na(mun_id)) %>% nrow()
cabezero %>% filter(is.na(municipio)) %>% nrow()
cabezero %>% filter(is.na(locality)) %>% nrow()

# now match to loc_ids for geographic location by local_id
match4 <- left_join(cabezero,loc_ids, by = "local_id")

# no many-to-many detected
match4_good <- match4 %>% 
  filter(!is.na(long_decimal)) %>%
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          locality = locality.y) %>% # keep the new
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())


# identify the ones that did not have a match
unmatch <- unmatch %>% filter(obs_id %not_in%  match4_good$obs_id)


full_match <- rbind(match1_good,match2_good,match3_good,match4_good)

# see what is needed in unmatch
nrow(unmatch[unmatch$recipient_name %in% full_match$recipient_name,])
nrow(unmatch[unmatch$id_recipient %in% full_match$id_recipient,])
nrow(unmatch[unmatch$rnpa %in% full_match$rnpa & !is.na(unmatch$rnpa),])



#### MATCH BY RECEPIENT ID in priors~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

match5 <- left_join(unmatch,full_match %>% 
                       select(id_recipient,long_decimal,lat_decimal,state,state_id,mun_id,municipio,local_id,locality) %>%
                       filter(!is.na(id_recipient)) %>%
                       unique(),
                     by = "id_recipient") 

# many-to-many detected
match5_good <- match5 %>% 
  filter(!is.na(long_decimal)) %>%
  mutate( state_id =  state_id.y,
          state = state.y,
          municipio = municipio.y,
          mun_id = mun_id.y,
          local_id= local_id.y,
          locality = locality.y) %>% # keep the new
  group_by(obs_id) %>% # select the first match
  slice_tail(n = 1) %>%
  ungroup() %>%
  # ---- remove columns first 
  select(-ends_with(".x"), -ends_with(".y")) %>%
  # ---- now reorder columns
  select(year, state_id, state, mun_id, municipio,
         local_id, locality, rnpa, obs_id, everything())


full_match <- rbind(full_match,match5_good)

unmatch <- unmatch %>% filter(obs_id %not_in% match5_good$obs_id)


# 642 observations did not match in any way.

##### MATCH FULL MATCH TO OFFICE --------------------------------------------------------------------------------


# load the office sf 
office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")


# make gas and diesel an sf object
temp_sf <- st_as_sf(full_match, coords = c("long_decimal","lat_decimal"), crs = crs(office_sf))

# to save computational power, join them, then use the observations unique by locality
temp_sf <- rbind(temp_sf) %>%
  select(state_id,state,mun_id,municipio,local_id,locality,geometry) %>%
  unique()

# create a temp_id
temp_sf$temp_id <- 1:nrow(temp_sf)

############################################################################################################################~
## put fuel_sf through the office matching machine ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vectorized approach. go from 10, 20, 30 km radius. Anything that doesn't match beyond this would use gmapdistance

# Define buffers
buffer_distances <- c(0, 10000, 20000, 30000)  # in meters

# Create list of buffered bp_unique_sf for each distance
buffers_list <- lapply(buffer_distances, function(dist) {
  if(dist == 0) {
    temp_sf %>%
      mutate(buffer_id = dist)
  } else {
    temp_sf %>%
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
  group_by(state_id, state, mun_id,municipio,local_id,locality, temp_id) %>%
  slice_min(buffer_id, with_ties = F) %>% # keeps the row with smallest buffer radius, breaking ties automatically.
  ungroup()


# keep only the gas location and office id, no need to keep geometry
office_match <- st_drop_geometry(matched[c("state_id","state","mun_id","municipio","local_id","locality","office_id","office","temp_id")])

# using gmapdistance for those beyond 30 km

# Subset unmatched locations
unmatched <- temp_sf %>% filter(temp_id %not_in% office_match$temp_id)

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

# join the gas_final_match to temp_sf

match_office_final <- left_join(temp_sf,
                               final_match %>% select(-temp_id),
                               by = c("state","state_id","municipio","mun_id","locality","local_id")) %>%
  st_drop_geometry()


# check that all the obs match
match_office_final[is.na(match_office_final$office_id),]


# use match_office_final to give an office to full_match 
all_sub_office <- left_join(full_match,
                        match_office_final %>% select(-temp_id),
                        by = c("state_id","state","mun_id","municipio","local_id","locality"))


# verify each obs has an office
all_sub_office %>% filter(is.na(office_id))




#### SUMMARIZE AND SAVE --------------------------------------------------------------------------------------------------
# summarize by locality
full_sum <- all_sub_office %>%
  group_by(year,state_id,state, mun_id, municipio,office_id,office) %>%
  summarize(
    total_mxn_modernacion = sum(subsidy_mxn[program == "MODERNACION"], na.rm = TRUE),
    total_mxn_inspeccion = sum(subsidy_mxn[program == "INSPECCION Y VIGILANCIA" ], na.rm = TRUE),
    total_mxn_adiestramiento = sum(subsidy_mxn[program == "ADIESTRAMIENTO" ], na.rm = TRUE),
    total_mxn_cadenas = sum(subsidy_mxn[program == "CADENAS PRODUCTIVAS" ], na.rm = TRUE),
    total_mxn_acuacultura = sum(subsidy_mxn[program == "DESARROLLO DE LA ACUACULTURA" ], na.rm = TRUE),
    total_mxn_transformacion = sum(subsidy_mxn[program == "TRANSFORMACION Y COMERCIALIZACION" ], na.rm = TRUE),
    total_mxn_estudios = sum(subsidy_mxn[program == "OBRAS Y ESTUDIOS" ], na.rm = TRUE)
  ) 

full_sum$all_other_sub_mxn <- rowSums(full_sum[c(8:14)])



# save this 
write.csv(full_sum,
          paste0(dir$cleandata,"other_subsidies_summarized_w_office.csv"),
          row.names = F)


