# Title: Clean Bienpesca Data (LATEST VERSION)
# Purpose: Clean and organize Bienpesca/propesca data
# Author: Jesus Felix
# Start date: 29 MAY 2025;
################################################################################
# We want the BP/Propesca exposure to be attributed to conapesca offices and identify any key indicators of variance in this data that can inform the study: What is the effect of the propesca/bienpesca exposure to catch data?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi","KableExtra",
          "mgsub","zoo","plotly","gdata","gmapsdistance","data.table")

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

# 1. load data, basic analysis, drop unnecessary columns, add obs_id -----------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# propesca_2014_2019  contains some bp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# link:  https://historico.datos.gob.mx/busca/dataset/propesca--incentivo-economico-directo-al-pescador-y-acuacultor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
propez <- read.csv(paste0(dir$rawdata, "PROPESCA_2014_2019.csv"),
                             stringsAsFactors = FALSE,
                             check.names = FALSE,
                             blank.lines.skip = TRUE,
                             fileEncoding = "Latin1") # helps identify any spanish articles

# check unique values and structure
str(propez)
summary(propez)
length(unique(propez$Folio)) # looks like folio works like an obs ID

# remove completely unnecessary/redundant columns
unique(propez$Programa)
unique(propez$Componente)
unique(propez$`Clave Presupuestaria`)
unique(propez$Incentivo) # this will be important dont drop
unique(propez$`Unidad Administrativa`)
unique(propez$`Modalidad o concepto de apoyo`)
unique(propez$Estatus) # good for identifying in propesca but doesnt go beyond this
unique(propez$`Tipo Persona de la Organizacion`) # not really helpful 

propez <- propez[, !names(propez) %in% c("Programa", "Componente","Clave Presupuestaria",
                                         "Unidad Administrativa","Modalidad o concepto de apoyo",
                                         "Tipo Persona de la Organizacion")]
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BIENPESCA (BP) from 2020 to 2024
# https://historico.datos.gob.mx/busca/dataset/bienpesca--incentivo-economico-directo-al-pescador-y-acuacultor
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp <- read_excel(paste0(dir$rawdata, "BIENPESCA_2020_2024.xlsx"))
str(bp)
summary(bp)
colnames(bp)
# remove unnecessary columns
bp <- bp[, !names(bp) %in% c("PROGRAMA", "COMPONENTE","UNIDAD ADMINISTRATIVA","TIPO PERSONA")]

# add an ID
bp$obs_id <- 1:length(bp$EJERCICIO)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## BIENPESCA recipient data, includes locality data

# 2021: https://historico.datos.gob.mx/busca/dataset/programa-de-fomento-a-la-agricultura-ganaderia-pesca-y-acuicultura-2021/resource/b21ca19a-0b72-4b4a-bcf5-112aaeb02921
# 2022: https://historico.datos.gob.mx/busca/dataset/programa-de-fomento-a-la-agricultura-ganaderia-pesca-y-acuicultura-2022/resource/4a66ff74-9430-45f7-b023-3f74c7dbc8f3 
# 2023: https://historico.datos.gob.mx/busca/dataset/programa-de-fomento-a-la-agricultura-ganaderia-pesca-y-acuicultura-cierre-preliminar-2023/resource/d257bd49-890b-4e11-bbb2-eeacf9e30c3e
# 2024: https://historico.datos.gob.mx/busca/dataset/programa-de-fomento-a-la-agricultura-ganaderia-pesca-y-acuicultura-2024/resource/c10ce92d-3a88-45cb-8a06-98b4cbb7a3ce
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make a loop for 2021 to 2024 (2020 missing for some reason)
bp_rec <- NULL

for(i in 2021:2024){ # p <- 2024
  
  if(i == 2024){ # 2024 is a csv
  p <- read.csv(paste0(dir$rawdata, "bienpesca_2024_recipients.csv"),
                header = TRUE, check.names = FALSE, 
                fileEncoding = "windows-1252",
                nrows = 193046) # anything after this row is bad  
  
  # adjust the FECHA so R can recognize
  p$FECHA <- as.Date(p$FECHA, format = "%d/%m/%Y")
  
    
  }else{
  
  p <- read_excel(paste0(dir$rawdata, "bienpesca_",i,"_recipients.xlsx"))
  
  p$FECHA <- as.Date(p$FECHA)
  
  }
  
  # remove rows with NA in REGION, this tells us the whole row is bad
  p <- p[!is.na(p$REGIÓN),]

  
  # bind and combine
  bp_rec <- rbind(bp_rec,p)
  
  
}

# check unique names and structure
str(bp_rec)
summary(bp_rec)

# remove unnecessary columns

unique(bp_rec$ESTRATIFICACIÓN)
unique(bp_rec$PROGRAMA)
unique(bp_rec$COMPONENTE)
unique(bp_rec$SUBCOMPONENTE)
unique(bp_rec$PRODUCTO)
unique(bp_rec$APOYO)
unique(bp_rec$ACTIVIDAD)
unique(bp_rec$ESLABÓN)
unique(bp_rec$`CICLO AGRÍCOLA`)

bp_rec <- bp_rec[, !names(bp_rec) %in% c("ESTRATIFICACIÓN","PROGRAMA", "COMPONENTE","SUBCOMPONENTE","PRODUCTO",
                                         "APOYO","ACTIVIDAD","ESLABÓN","CICLO AGRÍCOLA")]

# add an ID
bp_rec$obs_id <- 1:length(bp_rec$BENEFICIARIO)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Permit data 
# link: https://historico.datos.gob.mx/busca/dataset/permisos-y-concesiones-de-pesca-comercial-para-embarcaciones-mayores-y-menores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

com_permits <- read_xlsx(paste0(dir$rawdata, "permisos_pesca_comercial.xlsx"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Production data (for reference)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prod <- read.csv(paste0(dir$cleandata,"final_production_no_duplicates.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# locality geolocation data 
# https://www.inegi.org.mx/app/ageeml/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

loc_geo <- read_xlsx(
  path = paste0(dir$shp,"catalogo_localidades_mexico_mayuscula_sin_accento_2020/AGEEML_20255131425791.xlsx"),
  sheet = "Consulta",
  skip = 3,
  col_names = T)

# 2.Transform columns:  Translate columns to English, remove accents, ñ,other Spanish specific articles. Add month year columns -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2.1 location (loc_geo), Clean location data first so it can be used to reference the subsequent data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove any accents in character columns
loc_geo <- loc_geo %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
loc_geo <- loc_geo %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

# column names
colnames(loc_geo)
new_names <- c("cvegeo","status","state_id","state","state_abr","mun_id","municipio",
               "local_id","locality","climate","latitude","longitude","lat_decimal","long_decimal",
               "altitude","letter_id","population","male_pop","female_pop","opccupied_households")

colnames(loc_geo) <- new_names

# make an obs_id
loc_geo$obs_id <- 1:length(loc_geo$cvegeo)

# check for duplicates
# make a df for duplication analysis
dup_df <- loc_geo %>% 
  group_by(state_id,state,mun_id,municipio,locality) %>% 
  mutate(num_sim_obs = n(), 
         dup_id = row_number(),
         original_obs_id = first(obs_id))%>% 
  ungroup() %>% 
  mutate(is_duplicate = dup_id > 1)


dup_df <- dup_df[dup_df$is_duplicate == T,]

# remove the duplicates
loc_geo <- loc_geo[loc_geo$obs_id %not_in% dup_df$obs_id,]

# create ids in similar format as propez
loc_geo$state_id <- as.numeric(loc_geo$state_id)
loc_geo[is.na(loc_geo$state_id),]

df <- loc_geo
df$mun_id <- as.numeric(paste0(df$state_id, df$mun_id))
df$local_id <- as.numeric(paste0(df$mun_id,df$local_id))

loc_geo <- df


# save 
#write.csv(loc_geo,
#          paste0(dir$cleandata,"locality_location_data.csv"),
#          row.names = F)
#loc_geo <- read.csv(paste0(dir$cleandata,"locality_location_data.csv"))

rm(list = c("df","dup_df","p","unique_name_p","unique_local_p"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2.2 permits ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# translate names
colnames(com_permits)

new_names <- c("vessel_type","state_id","state","mun_id","municipio","local_id","locality",
               "RNP","business_name","zone_of_operation","base_port","vessel_or_n_vessels","fishery",
               "permit_number","start_date","end_date","equipment","equipment_description","notes","permit_type")

colnames(com_permits) <- new_names

# check unique values
unique(sort(com_permits$local_id))
unique(sort(com_permits$state))
unique(sort(com_permits$municipio))
unique(sort(com_permits$permit_type))


# remove any accents in character columns
com_permits <- com_permits %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
com_permits <- com_permits %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

# change date columns
com_permits$start_date <-  as.Date(com_permits$start_date)
com_permits$end_date <-  as.Date(com_permits$end_date)

# make empty/missing localities into NA
com_permits$locality <- ifelse(com_permits$locality == "*",NA,com_permits$locality)

# rename states
# check the state names
perm_id <- unique(com_permits[c("state","state_id")])
loc_ids <- unique(loc_geo[c("state","state_id")])

df <- com_permits %>%
  mutate(state = case_when(
    state_id == 9 ~ "CIUDAD DE MEXICO",
    state_id == 5 ~ "COAHUILA DE ZARAGOZA",
    state_id == 15 ~ "MEXICO",
    state_id == 16 ~ "MICHOACAN DE OCAMPO",
    state_id == 30 ~ "VERACRUZ DE IGNACIO DE LA LLAVE",
    TRUE ~ state
  ))

df$state_id <- ifelse(df$state == "EXTRANJERO",0,df$state_id)
perm_id <- unique(df[c("state","state_id")])
loc_ids <- unique(loc_geo[c("state","state_id")])

com_permits <- df

# municipios
df <- com_permits # make a temp df
df$mun_id <- paste0(df$state_id, sprintf("%03d", df$mun_id)) # add three zero padding
df$mun_id <- as.numeric(df$mun_id)
com_permits <- df # turn back to com_permits once everything looks good

# locality
com_permits$local_id <- as.numeric(com_permits$local_id)
perm_id <- unique(com_permits[c("state","state_id","municipio","mun_id","locality","local_id")])
loc_ids <- unique(loc_geo[c("state","state_id","municipio","mun_id","locality","local_id")])

# check_names <- left_join(perm_id,loc_ids, by = c("state","state_id","municipio","mun_id","local_id"))
# check_ids <- left_join(perm_id,loc_ids, by = c("state","state_id","municipio","mun_id","locality"))

# keep ids in loc_ids
df <- com_permits
df <- df %>%
  left_join(loc_ids, by = c("state", "state_id", "mun_id","municipio","locality")) %>%
  select(-local_id.x) %>%
  rename("local_id" = "local_id.y")
  
com_permits <- df

# save
# write.csv(com_permits,
#           paste0(dir$cleandata,"commercial_permits_processed.csv"),
#           row.names = F)

#com_permits <- read.csv(paste0(dir$cleandata,"commercial_permits_processed.csv"))
rm(list = c("loc_ids","perm_ids","check_ids","check_names","df","temp","names"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2.3 bp 2020 to 2024 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### translation ~~~~~~~~~~~~~~~~~~~~
colnames(bp)

new_names <- c("year","program","state_id","state","mun_id","municipio","first_name","paternal_lastname",
               "maternal_lastname","pay_date","federal_amount","age","gender","obs_id")

colnames(bp) <- new_names

# check unique values
# unique(sort(bp$state))
# unique(sort(bp$municipio))
# unique(sort(bp$federal_amount))


# remove any accents in character columns
bp <- bp %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
bp <- bp %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

### DATES ~~~~~~~~~~~~~~~~~~
# add month column
bp$month <- month(bp$pay_date)


### LOCATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check the state names
bp_ids <- unique(bp[c("state","state_id")])
loc_ids <- unique(loc_geo[c("state","state_id")])
check_ids <- cbind(bp_ids,rename(loc_ids, "state_loc" = "state", "state_id_loc"= "state_id"))
check_ids$state == check_ids$state_loc
check_ids$state_id == check_ids$state_id_loc

# check municipios and municipio ids

# bp mun_ids should have the state with them, as is in propez and loc_geo
df <- bp # make a temp df
df$mun_id <- paste0(df$state_id, sprintf("%03d", df$mun_id)) # add three zero padding
bp <- df # turn back to bp once everything looks good

bp_ids <- unique(bp[c("state","state_id","municipio","mun_id")])
loc_ids <- unique(loc_geo[c("state","state_id","municipio","mun_id")])

check_ids <- left_join(bp_ids, loc_ids, by = c("state","state_id","municipio"))
check_ids_na <- check_ids[is.na(check_ids$mun_id.y),] # see the ones with mismatching municipios

# from the na's see if there is match by ids
df <- loc_geo[loc_geo$mun_id %in% check_ids_na$mun_id.x,]
df <- unique(df[c("state","state_id","municipio","mun_id")]) # these are the correct names

# Make a temp copy of bp (optional but smart for rollback)
temp <- bp # make a temp df (to reverse mistakes easily)
temp$mun_id <- as.numeric(temp$mun_id)

# Join the correct municipio names into temp
temp <- full_join(temp, df, by = c("state", "state_id", "mun_id"))

# replace with correct municipio names
temp <- temp %>%
  mutate(municipio = ifelse(!is.na(municipio.y), municipio.y, municipio.x)) %>%
  select(-municipio.x, -municipio.y)

# now it should have the right municipio names
bp <- temp

# check unique values
# unique(sort(bp$state))
# unique(sort(bp$municipio))

### BENEFICIARY NAMES ~~~~~~~~~~~~~~~~~~~~~~~~~~
# full_name

names <- bp[bp$paternal_lastname == bp$maternal_lastname,]
names <- unique(names[c("first_name","paternal_lastname","maternal_lastname","mun_id")])
# too many to have the same last paternal and maternal last name
bp$maternal_lastname <- ifelse(bp$paternal_lastname == bp$maternal_lastname, NA,bp$maternal_lastname) # turn into na for maternal lastname

# make a full name
bp$full_name <- paste(bp$first_name,bp$paternal_lastname,bp$maternal_lastname, sep = " ")

# fix the names that had NA for maternal last name
df <- bp
df$full_name  <- ifelse(is.na(df$maternal_lastname),paste(df$first_name,df$paternal_lastname, sep = " "),
                        df$full_name) 
bp <- df

# save
# write.csv(bp,
#           paste0(dir$cleandata,"bienpesca_data_unfinished.csv"),
#           row.names = F)

rm(list = c("bp_ids","check_ids","check_ids_na","df","temp","names"))


#bp <- read.csv(paste0(dir$cleandata,"bienpesca_data_unfinished.csv"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2.4 PROPESCA 2014 TO 2019 (includes some BP in 2019) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### TRANSLATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# translate names
colnames(propez)

new_names <- c("year","program","state_id","state","mun_id","municipio","local_id","locality",
               "start_date","end_date","folio_id","full_name","gender","fishery","status","amount_requested",
               "federal_supplied","state_supplied","RNP","vessel_RNP","organization_coop")

colnames(propez) <- new_names

# check unique values
# unique(sort(propez$locality))
# unique(sort(propez$state))
# unique(sort(propez$fishery))

# remove any accents in character columns
propez <- propez %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
propez <- propez %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

propez <- propez %>%
  mutate(across(where(is.character), ~ str_replace(.,"ñ","n")))

### QUANTITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# removed those that were not approved and remove that column
propez <- propez[propez$status == 'APROBADO',]
propez <- propez %>% select(-status)


# check there are no NA is amount
propez[is.na(propez$federal_supplied),]
propez[is.na(propez$state_supplied),]
propez[propez$federal_supplied == " ",]

# make a column that sums the amount supplied
propez$total_supplied <- propez$state_supplied+propez$federal_supplied
propez[is.na(propez$total_supplied),]


### DATES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# change date columns
propez$start_date <-  as.Date(propez$start_date, format = "%d/%m/%Y")
propez$end_date <-  as.Date(propez$end_date, format = "%d/%m/%Y")

# add month
propez$month <- month(propez$start_date)



### LOCATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check state names and ids
pp_ids <- unique(propez[c("state","state_id")])
loc_ids <- unique(loc_geo[c("state","state_id")])
check_ids <- left_join(pp_ids,loc_ids, by = "state_id")
df <- propez %>%
  mutate(state = case_when(
    state_id == 9 ~ "CIUDAD DE MEXICO",
    state_id == 5 ~ "COAHUILA DE ZARAGOZA",
    state_id == 16 ~ "MICHOACAN DE OCAMPO",
    state_id == 30 ~ "VERACRUZ DE IGNACIO DE LA LLAVE",
    TRUE ~ state
  ))
pp_ids <- unique(df[c("state","state_id")])
check_ids <- left_join(pp_ids,loc_ids, by = "state_id")
propez <- df

##  municipalities, consider the latest municipalites (san felipe and comondu) and use those
# check for empty municipalites
propez[is.na(propez$municipio),]
empty_mun <- propez[propez$municipio == " ",] 

# use rnps from permit data to give proper municipio
rnp_match <- com_permits[com_permits$RNP %in% empty_mun$RNP,]
rnp_match <- unique(rnp_match[c("state_id","state","mun_id","municipio","local_id","locality","RNP")])

# fill in missing munis from RNP_match

df <- empty_mun %>%
  left_join(rnp_match, by = c("state", "state_id","RNP")) %>%
  select(-local_id.x,-municipio.x,-mun_id.x,-locality.x) %>%
  rename("local_id" = "local_id.y",
         "mun_id" = "mun_id.y",
         "municipio" = "municipio.y",
         "locality" = "locality.y")

# remove the second row
df <- df[-2,]

# add to the rest of the data
propez <- propez[propez$folio_id %not_in% df$folio_id,]
propez <- rbind(propez,df)
propez[propez$municipio == " ",]  # these are the empty municipios, all empty
propez <- propez[propez$municipio != " ",] 


# there are municipalites that do not fit into the state. 
# the folio id also tells us that the municipality is wrong (check abbreviation)
# use the folio to identify the states and use permit data to identify the municipipality/locality
# Extract parts from the folio and add as new columns, extract states from mun_id
df <- propez %>%
  mutate(
    folio_st_id = as.numeric(str_match(folio_id, "^(\\d{2})-([A-Z]{2})(\\d{3})")[, 2]),
    folio_st  = str_match(folio_id, "^(\\d{2})-([A-Z]{2})(\\d{3})")[, 3],
    folio_other = str_match(folio_id, "^(\\d{2})-([A-Z]{2})(\\d{3})")[, 4],
    mun_st_id = as.numeric(sub("\\d{3}$", "",mun_id)) # keeps the first two digits of mun_id (which would be the state)
                           )


df$folio_unmatch <- ifelse(df$mun_st_id != df$folio_st_id, 1,0)

# check unmatch df
unmatch_df <- df[df$folio_unmatch == 1,]
unmatch_df[unmatch_df$municipio == " ",] # a bumch of NA's
unmatch_df <- unmatch_df[!is.na(unmatch_df$year),] # remove the NA's

# use the organization name to find the true location
org_loc <- unique(df[c("state_id","state","mun_id","municipio","local_id","locality","mun_st_id","organization_coop","folio_unmatch")])

org_loc <- org_loc[org_loc$folio_unmatch == 0,] # use only the organizations that have a good folio

org_match_df <- NULL
for(i in 1:length(row.names(unmatch_df))){ # i <- 1
  
  temp <- unmatch_df[i,] # select the observation in unmatch_df
  
  org <- temp$organization_coop # select the organization
  st_id <- temp$folio_st_id # use the folio_state_id as the state identifier (in case our state is wrong)
  
  match <- org_loc[org_loc$state_id == st_id & org_loc$organization_coop == org,]
  
  if(length(match$organization_coop) == 0){
    
    next
  }
  
  temp$mun_id <- match[1,]$mun_id # choose first observation in match
  temp$municipio <- match[1,]$municipio # choose first observation in match
  temp$local_id <- match[1,]$local_id # choose first observation in match
  temp$locality <- match[1,]$locality # choose first observation in match
  
  # save
  org_match_df <- rbind(org_match_df,temp)
  
}


# save the ones that are not na
org_match_df <- org_match_df[!is.na(org_match_df$municipio),]

# find the ones that are not in here from un_match_df
unmatch_df <- unmatch_df[unmatch_df$folio_id %not_in% org_match_df$folio_id,]
unmatch_df <- unmatch_df[!is.na(unmatch_df$year),] # remove the ones with all NA, should be 1567 obs

# use rnps from permit data to give proper state
# for these, the folio state does not match the state. We know because the organizations are located correctly in the municipio/locality
rnp_match <- com_permits[com_permits$RNP %in% unmatch_df$RNP,]
rnp_match <- unique(rnp_match[c("state_id","state","mun_id","municipio","local_id","locality","RNP")])

# fill in correct location info from RNP_match

rnp_match_df <- NULL
for(i in 1:length(row.names(unmatch_df))){ # i <- 1
  
  temp <- unmatch_df[i,] # select the observation in unmatch_df
  
  rnp <- temp$RNP # select the RNP
  
  match <- rnp_match[rnp_match$RNP == rnp,]
  
  if(length(match$RNP) == 0){
    
    next
  }
  
  temp$state_id <- match[1,]$state_id # choose first observation in match
  temp$state <- match[1,]$state # choose first observation in match
  temp$mun_id <- match[1,]$mun_id # choose first observation in match
  temp$municipio  <- match[1,]$municipio # choose first observation in match
  temp$local_id <- match[1,]$local_id # choose first observation in match
  temp$locality <- match[1,]$locality # choose first observation in match

  
  # save
  rnp_match_df <- rbind(rnp_match_df,temp)
  
}

# there is one observation in unmatch_df
unmatch_df <- unmatch_df[unmatch_df$folio_id %not_in% rnp_match_df$folio_id, ]

# for this one, use the folio_other to match the municipio/locality
# use bp for the name match to see the municipio
name_match <- bp[bp$full_name == unmatch_df$full_name,]

folio_match <- unique(df[c("folio_st_id","folio_other","municipio","mun_id","local_id","locality")])
folio_match <- folio_match[folio_match$mun_id == name_match$mun_id &
                             folio_match$folio_other == unmatch_df$folio_other,]
# use the second observation, it matches the organization location
unmatch_df$mun_id <- folio_match[2,]$mun_id
unmatch_df$municipio <- folio_match[2,]$municipio
unmatch_df$local_id <- folio_match[2,]$local_id
unmatch_df$locality <- folio_match[2,]$locality

# now all the municipios are in the correct location

# bind the datasets
final_match_bind <- rbind(org_match_df,rnp_match_df,unmatch_df)
length(final_match_bind$year) # 1567 obs like before

# remove the additional rows
final_match_bind <-  final_match_bind %>% select(-folio_st_id,-folio_st,-folio_other,-folio_unmatch,-mun_st_id)

# add these to propez
df <- propez # 196918
df <- df[df$folio_id %not_in% final_match_bind$folio_id,] #195351
df <- rbind(df,final_match_bind) # 196918
propez <- df


### Localities have new municipio names to be updated, once combined can be done


# final removal of empty rows
propez <- propez[!is.na(propez$year),]

# # check unique values
# unique(sort(propez$locality))
# unique(sort(propez$state))
# unique(sort(propez$fishery))

rm(list = c("bad_mun","df","empty_mun","ids","ids_p","loc_ids","match","name_match","name_match_df",
            "org_loc","org_match_df","perm_id","rnp_match","rnp_match_df","temp","unmatch_df",
            "folio_check","folio_match","check_ids","check_names","no_match","pp_ids","final_match_bind"))

# # save this version of propez
# write.csv(propez,
#           paste0(dir$cleandata,"propez_data_unfinished.csv"),
#           row.names = F)

#propez <- read.csv(paste0(dir$cleandata,"propez_data_unfinished.csv"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2.5 bp recipient data  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TRANSLATION: BP_REC~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colnames(bp_rec)

new_names <- c("full_name","region","state","municipio","locality","date","federal_amount","obs_id")

colnames(bp_rec) <- new_names

# check unique values
# unique(sort(bp_rec$locality))
# unique(sort(bp_rec$state))
# unique(sort(bp_rec$municipio))


# remove any accents in character columns
bp_rec <- bp_rec %>%
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

# replace any ñ
bp_rec <- bp_rec %>%
  mutate(across(where(is.character), ~ str_replace(.,"Ñ","N")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUANTITIES: BP_REC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove dollar sign in federal amount and remove commas
bp_rec$federal_amount <- str_replace(bp_rec$federal_amount, "\\$","")
bp_rec$federal_amount <- sub(",","",bp_rec$federal_amount)

# change to numeric
bp_rec$federal_amount <- as.numeric(bp_rec$federal_amount)

# check there are no NA is amount
bp_rec[is.na(bp_rec$federal_amount),]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATES: BP_REC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

# add a year
bp_rec$year <- year(bp_rec$date)

# add a month
bp_rec$month <- month(bp_rec$date)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ NAMES: BP_REC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# bp_rec$full_name has the first name at the end
df <- bp_rec

df$first_name <- sub("^\\S+\\s+\\S+\\s*", "", df$full_name) # remove the first two names (paternal and maternal)

df$full_lastname <- sub("^((\\S+\\s+\\S+)).*", "\\1", df$full_name) # select the first two names and remove the rest

df$full_name_fix <- paste(df$first_name,df$full_lastname, sep = " ")

# check the names that match
name_match <- df[df$full_name_fix %in% bp$full_name,]
length(name_match$full_name)
length(df$full_lastname)

# it looks like the majority of names are in there
# drop the firstname, full-lastname and rename fullnamefix
df <- df %>%
  select(-first_name,-full_lastname,-full_name) %>%
  rename("full_name" = "full_name_fix")
bp_rec <- df

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LOCATION: BP_REC match to BP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# keep df as bp_rec
# BP_REC: STATES

# check state names and ids
ids <- unique(bp_rec[c("state")])
loc_ids <- unique(loc_geo[c("state","state_id")])

# what is NACIONAL?
nac_loc <- df[df$state == 'NACIONAL',]

# look for the name in propez
nac_pro <- propez[propez$full_name %in% nac_loc$full_name,]
nac_pro <- unique(nac_pro[c("full_name","state","state_id","municipio","mun_id","locality","local_id","year")])
nac_pro_join <- left_join(nac_loc,nac_pro, by = c("full_name"))
nac_pro_join <- nac_pro_join[!is.na(nac_pro_join$municipio.y),]

# look for the name in bp
nac_bp <- bp[bp$full_name %in% nac_loc$full_name,]
nac_bp <- unique(nac_bp[c("full_name","state","state_id","municipio","mun_id","year")])
nac_bp_join <- left_join(nac_loc,nac_bp, by = c( "full_name"))
nac_bp_join <- nac_bp_join[!is.na(nac_bp_join$municipio.y),]

# are all the names between these two?
all_names <- unique(c(nac_bp_join$full_name,nac_pro_join$full_name))
nac_loc$full_name %in% all_names # yes!

# combine the nac_locs together
nac_join <- full_join(nac_bp_join[-c(1:6)],nac_pro_join[-c(1:6)], by = "full_name")

nac_join$obs_id.x <- ifelse(is.na(nac_join$obs_id.x),nac_join$obs_id.y,nac_join$obs_id.x)
nac_join$year.x.x <- ifelse(is.na(nac_join$year.x.x),nac_join$year.x.y,nac_join$year.x.x)
nac_join$month.x <- ifelse(is.na(nac_join$month.x),nac_join$month.y,nac_join$month.x)

# remove the redundant
nac_join <- nac_join %>% select(-obs_id.y,-year.x.y,-month.y)

colnames(nac_join) # rename cols

new_names <- c("obs_id","year_bprec","month_bprec","full_name","state_bp","state_id_bp","municipio_bp","mun_id_bp","year_bp"
               ,"state_pp","state_id_pp","municipio_pp","mun_id_pp","locality_pp","local_id_pp","year_pp")

colnames(nac_join) <- new_names # rename cols

nac_loc_df <- NULL
name_list <- as.list(nac_loc$full_name) # create  a list for all the names

# loop to find the location closest to year
for(i in name_list){ # i <- "OLGA SALAMANCA MORALES"
  
  temp <- nac_join[nac_join$full_name == i,] # select the name from name list
  
  target <- unique(temp$year_bprec) # select the target year, the year from bprec
  
  closest_row_bp <- temp[which.min(abs(temp$year_bp - target)), ] # get the row with the year closest to the target in bp
  closest_row_pp <- temp[which.min(abs(temp$year_pp - target)), ] # get the row with the year closest to the target in pp
  
  # bind them
  c_rows <- rbind(closest_row_bp,closest_row_pp)
  
  # save this
  nac_loc_df <- rbind(nac_loc_df,c_rows)
  
}

# remove these rows
nac_loc_df <- nac_loc_df[-c(14,7,19,27,37,11,25,21,16,30,3,22),]

# select the columns that have the right information
nac_loc_df$state <- ifelse(is.na(nac_loc_df$state_pp),nac_loc_df$state_bp,nac_loc_df$state_pp)
nac_loc_df$municipio <- ifelse(is.na(nac_loc_df$municipio_pp),nac_loc_df$municipio_bp,nac_loc_df$municipio_pp)
nac_loc_df$locality <- ifelse(is.na(nac_loc_df$locality_pp),NA,nac_loc_df$locality_pp)

nac_loc_good <- left_join(nac_loc_df[c(4,17:19)], nac_loc[-c(2:4)], by = "full_name")

# add these 
bp_rec <- bp_rec[bp_rec$obs_id %not_in% nac_loc_good$obs_id,]
bp_rec <- rbind(bp_rec,nac_loc_good)


# return to states and state names
df <- bp_rec %>%
  mutate(state = case_when(
    state == "COAHUILA" ~ "COAHUILA DE ZARAGOZA",
    state == "ESTADO DE MEXICO" ~ "MEXICO",
    state == "MICHOACAN" ~ "MICHOACAN DE OCAMPO",
    state == "VERACRUZ" ~"VERACRUZ DE IGNACIO DE LA LLAVE",
    TRUE ~ state
  ))


ids <- unique(df[c("state")])
loc_ids <- unique(loc_geo[c("state","state_id")])

df <- left_join(df,loc_ids, by = "state")

bp_rec <- df

### MUNICIPALITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##  municipalities, consider the latest municipalites (san felipe and comondu) and use those
# check for empty municipalites
bp_rec[is.na(bp_rec$municipio),]
empty_mun <- bp_rec[bp_rec$municipio == " ",] 

# get the ids
ids <- unique(loc_geo[c("state","state_id","municipio","mun_id")])
df <- left_join(bp_rec,ids, by = c("state","state_id","municipio"))

# check the duplicates
ids_df <- unique(df[c("state","state_id","municipio","mun_id")])

dup_df <- ids_df %>% 
  group_by(state,state_id,municipio) %>% 
  mutate(num_sim_obs = n(), 
         dup_id = row_number())%>% 
  ungroup() %>% 
  mutate(is_duplicate = dup_id > 1)
ids_df <- dup_df[dup_df$is_duplicate == F,]

df <- left_join(bp_rec,ids_df[-c(5:7)], by = c("state","state_id","municipio"))
bp_rec <- df
### LOCALITIES 

# fix the localities with the state name attached
df <- bp_rec
df$locality <- sub("-.*", "", df$locality)
bp_rec <- df

# get the ids
ids <- unique(loc_geo[c("state","state_id","municipio","mun_id","locality","local_id")])
df <- left_join(bp_rec,ids, by = c("state","state_id","municipio","mun_id","locality"))

bp_rec <- df

# remove region
bp_rec <- bp_rec %>% select(-region)

# # check unique values
# unique(sort(bp_rec$locality))
# unique(sort(bp_rec$state))
# unique(sort(bp_rec$municipio))
# 
# # save
# write.csv(bp_rec,
#           paste0(dir$cleandata,"bp_recepient_data_unfinished.csv"),
#           row.names = F)
# bp_rec <- read.csv( paste0(dir$cleandata,"bp_recepient_data_unfinished.csv"))

rm(list = c("c_rows","closest_row_bp","closest_row_pp","df","dup_df","empty_mun","ids","ids_df",
            "ids_df_bad","ids_loc","nac_bp","nac_bp_join","nac_loc","nac_loc","nac_loc_df","nac_loc_good",
            "nac_pro","nac_pro_join","name_match","name_list","temp","loc_ids","nac_join"))

# 3.Combine sets -----------------------------------------------------------------------------------------------


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### recipients/propez_data to BP to provide a locality for years 2020 and 2024 

# using data.table we can run this much faster than a df due to its optimized in-memory operations
# Convert to data.table if not already
bp_dt <- bp
bp_rec_dt <- bp_rec
propez_dt <- propez


setDT(bp_dt)
setDT(bp_rec_dt)
setDT(propez_dt)

# Create an empty list to store results
results <- vector("list", nrow(bp_dt))

# Loop efficiently
for (i in seq_len(nrow(bp_dt))) { # i <- 2
  temp <- bp_dt[i] # select the observation
  
  # First try matching with bp_rec
  match <- bp_rec_dt[full_name == temp$full_name &
                    state == temp$state &
                    municipio == temp$municipio]
  
  if (nrow(match) > 0) {
    # Closest year
    match <- match[which.min(abs(year - temp$year))]
  } else {
    # Fallback to propez
    match <- propez_dt[full_name == temp$full_name &
                      state == temp$state]
    
    if (nrow(match) > 0) {
      match <- match[which.min(abs(year - temp$year))]
    }
    
    
  }
  
  if (nrow(match) > 0) {
    temp[, locality := match$locality]
    temp[, local_id := match$local_id]
    results[[i]] <- temp
  }
}


bp_local_dt <- rbindlist(results, use.names = TRUE, fill = TRUE)

df <- bp[bp$obs_id %not_in% bp_local_dt$obs_id,]  # get the observations that did not have a match

df$locality <- NA # give them NA for locality
df$local_id <- NA

df <- rbind(df,bp_local_dt) # combine with bp_local_df
bp_local_dt <- df

length(bp_local_dt[is.na(bp_local_dt$locality),1]) # 59,580 obs without a locality
length(unique(bp_local_dt[is.na(bp_local_dt$locality),]$full_name)) # 21359 people without locality listed

# see if these names are in the data 
na_list <- bp_local_dt[is.na(bp_local_dt$locality),] # get a df of the NA's

name_match <- left_join(na_list,bp_local_dt[!is.na(bp_local_dt$locality),], by = c('full_name','state','municipio'), suffix = c("", "_dt")) # join them to the rest of the data by full_name, state, and municipio

name_match$year_diff <- abs(name_match$year - name_match$year_dt) # make a way to distinguish the closest year

# remove the na's
name_match <- name_match[!is.na(name_match$locality_dt),]

# choose the smallest year diff per person

name_match_done <- name_match %>%
  group_by(full_name) %>%
  slice_min(order_by = year_diff, n = 1, with_ties = FALSE) %>%
  ungroup()
name_match_done$locality <- name_match_done$locality_dt
name_match_done$local_id <- name_match_done$local_id_dt
name_match_done <- name_match_done[-c(19:length(name_match_done))]

df <- bp_local_dt[bp_local_dt$obs_id %not_in% name_match_done$obs_id,]

df <- rbind(df,name_match_done) # combine with bp_local_df
bp_local_dt <- df  

# 59,567 obs without a locality
59567/nrow(bp_local_dt) # 0.06243822 of bp observations without locality data

# save this
write.csv(bp_local_dt,
          paste0(dir$cleandata,"bp_with_locality.csv"),
          row.names = F)

## for those people who do not have a locality, Identify them we will work on mapping those later
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## combine with propez data
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(propez)
colnames(bp_local_dt) # match propez colnames to bp_local_dt
pp_df <- propez %>%
  select(-end_date,-folio_id,-fishery,-amount_requested,-federal_supplied,-state_supplied,-RNP,-vessel_RNP,-organization_coop,) %>%
  rename("pay_date" = "start_date" )


bp_df <- bp_local_dt %>%
  select(-age, -obs_id, -paternal_lastname, -maternal_lastname,-first_name) %>%
  rename("total_supplied" = "federal_amount")


# now they should be ready for the rbind
bp_pp_joined <- as.data.frame(rbind(pp_df,bp_df))



# fix gender
bp_pp_joined$gender <- ifelse(bp_pp_joined$gender == "H", "MASCULINO", # instead of H and M, we want masculino and femenino respectively
                       ifelse(bp_pp_joined$gender == "M", "FEMENINO", bp_pp_joined$gender ))

# add an obs id
bp_pp_joined$obs_id <- 1:nrow(bp_pp_joined)


## LOCATION FIX ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check who needs locality data
nrow(bp_pp_joined[is.na(bp_pp_joined$locality),])/nrow(bp_pp_joined) # 0.05175632 of data missing locality
nrow(bp_pp_joined[is.na(bp_pp_joined$local_id),])/nrow(bp_pp_joined) # 0.1079178 of data missing local_id


# fix some issues with locality, for example, localidad sin nombre, san felipe should be in the san felipe municipality, etc.
ids <- unique(loc_geo[c("state_id","state","mun_id","municipio","local_id","locality")])
sin_nombre <- bp_pp_joined[bp_pp_joined$locality == "LOCALIDAD SIN NOMBRE",] # SELEC THE SIN NOMBRES
sn_match <- left_join(sin_nombre, ids, by = c("state_id","state","local_id"), suffix = c("","_loc")) # try to match by state and local id
sn_match$locality <- sn_match$locality_loc # choose the names in locality_loc
sn_match <- sn_match %>% select(-mun_id_loc,-municipio_loc,-locality_loc)
# join them to bp_pp_joined
bp_pp_joined <- bp_pp_joined[bp_pp_joined$obs_id %not_in% sn_match$obs_id,]
bp_pp_joined <- rbind(bp_pp_joined,sn_match)

# now municipios that are new, typically the ones that have the same name as the locality

ids_mun_local_same <- ids[ids$municipio == ids$locality,]

ids_df <- left_join(bp_pp_joined,ids_mun_local_same, suffix = c("","_loc"), by = c("state_id","state","locality"))
ids_df <- ids_df[!is.na(ids_df$municipio_loc),] # remove non-matches
ids_df <- ids_df[ids_df$municipio != ids_df$municipio_loc,] # select the ones that are different
# replace the values
ids_df$mun_id <- ids_df$mun_id_loc 
ids_df$municipio <- ids_df$municipio_loc
ids_df$local_id <- ids_df$local_id_loc

ids_df <- ids_df %>% select(-mun_id_loc,-municipio_loc,-local_id_loc) # remove the loc columns

# join them to bp_pp_joined
bp_pp_joined <- bp_pp_joined[bp_pp_joined$obs_id %not_in% ids_df$obs_id,]
bp_pp_joined <- rbind(bp_pp_joined,ids_df)

# names in localities are not consistent, use the names in loc_geo
mun_df <- unique(bp_pp_joined[c("state_id","state","mun_id","municipio","local_id","locality")])
local_names <- left_join(bp_pp_joined,ids, suffix = c("","_loc"), by = c("state_id","state","local_id")) # use the names in loc_geo
local_names <- local_names[!is.na(local_names$locality_loc),] # remove na's
local_names <- local_names[local_names$locality != local_names$locality_loc,] # select the ones with inconsistent locality name
local_names$locality <- local_names$locality_loc # adopt the loc_geo name
local_names <- local_names %>% select(-mun_id_loc,-municipio_loc,-locality_loc) # remove the loc columns

# join them to bp_pp_joined
bp_pp_joined <- bp_pp_joined[bp_pp_joined$obs_id %not_in% local_names$obs_id,]
bp_pp_joined <- rbind(bp_pp_joined,local_names)

# replace CD. and CD with CIUDAD
df <- bp_pp_joined
df$locality <- gsub("\\bCD\\.\\s+(?=\\w)", "CIUDAD ", df$locality, perl = TRUE)
df$locality <- gsub("\\bCD\\s+(?=\\w)", "CIUDAD ", df$locality, perl = TRUE)
mun_df <- unique(df[c("state_id","state","mun_id","municipio","local_id","locality")])


mun_df <- unique(bp_pp_joined[c("state_id","state","mun_id","municipio","local_id","locality")])

# there are localities that contain the same name as a locality with an id but are missing one
id_na <- df[is.na(df$local_id) & !is.na(df$locality),]
id_na$keyword <- word(id_na$locality, 1, 2) # use a key word for the locality
mun_df$keyword <- word(mun_df$locality, 1, 2)

id_na_match <- left_join(id_na, mun_df[!is.na(mun_df$local_id),], by = c("state","state_id","municipio","mun_id","keyword"),
                         suffix = c("","_df"))
id_na_match <- id_na_match[!is.na(id_na_match$local_id_df),]
id_na_match <- id_na_match[!is.na(id_na_match$keyword),]

# check for duplicate matches
dup_df <- id_na_match %>% 
  group_by(obs_id) %>% 
  mutate(num_sim_obs = n(), 
         dup_id = row_number(),
         original_obs_id = first(obs_id))%>% 
  ungroup() %>% 
  mutate(is_duplicate = dup_id > 1)
dup_df <- dup_df[dup_df$is_duplicate == FALSE,]

dup_df$local_id <- dup_df$local_id_df
dup_df$locality <- dup_df$locality_df

# REMOVE unncessary columns
id_na_match <- dup_df[-c(15:21)]

# join them to bp_pp_joined
bp_pp_joined <- df[df$obs_id %not_in% id_na_match$obs_id,]
bp_pp_joined <- rbind(bp_pp_joined,id_na_match)

# LOCALITY NAME = MUNICIPIO NAME
df <- bp_pp_joined %>%
  filter(municipio == locality)

df$local_id <- as.numeric(paste0(df$mun_id,"0001")) # the first locality is always the one with same name as municipality, 
  
bp_pp_joined <- bp_pp_joined[bp_pp_joined$obs_id %not_in% df$obs_id,]
bp_pp_joined <- rbind(bp_pp_joined,df)


# MANUAL FIXES
mun_df <- unique(bp_pp_joined[c("state_id","state","mun_id","municipio","local_id","locality")])
mun_df <- arrange(mun_df, state,municipio,locality)
rownames(mun_df) <- 1:nrow(mun_df)
mun_df <- mun_df[!is.na(mun_df$locality),]

na_rows <- which(apply(is.na(mun_df), 1, any))  # Find rows with any NA
rows_to_keep <- unique(sort(c(na_rows - 1, na_rows, na_rows + 1))) # identify the rows before and after

# Keep only valid indices (within bounds)
rows_to_keep <- rows_to_keep[rows_to_keep >= 1 & rows_to_keep <= nrow(mun_df)]

df_subset <- mun_df[rows_to_keep, ]

df <- bp_pp_joined %>%
  mutate(local_id = case_when(
    locality == "COLONIA PROGRESO" & municipio == "PABELLON DE ARTEAGA" ~ 10060001,
    locality == "SAN FELIPE (EJIDO PLAN NACIONAL AGRARIO) [COMPANIA]" ~ 20070001,
    municipio == "COMONDU" & locality %in% c("NINGUNO","COMONDU","EJIDO MATANCITAS GRUPO 10 F") ~ 30010001,
    locality %in% c("EL ZACATAL","LOS CABOS") & municipio == "LOS CABOS" ~ 30080001,
    locality %in% c("LA BOCANA","MULEGE") & municipio == "MULEGE" ~ 30021750,
    locality %in% c("NINGUNO [BAHIA TORTUGAS]","NINGUNO (BAHIA TORTUGAS)") & municipio == "MULEGE" ~ 30020015,
    locality %in% c("CAMPECHE","CORRALON DE GRUAS CAMPECHE") & municipio == "CAMPECHE" ~ 40020001,
    locality %in% c("CARMEN","CORRALON DE GRUAS CAMPECHE") & municipio == "CARMEN" ~ 40030001,
    TRUE ~ local_id  # youre gonna have to do this later
  ))

bp_pp_joined <- df

# check who needs locality data
nrow(bp_pp_joined[is.na(bp_pp_joined$locality),])/nrow(bp_pp_joined) # 0.05196571 of data missing locality

nrow(bp_pp_joined[is.na(bp_pp_joined$local_id),])/nrow(bp_pp_joined) # 0.05680353 of data missing local_id


# arrange/sort how we want
bp_pp_joined <- arrange(bp_pp_joined, year,month,state,municipio,locality,full_name)

bp_pp_joined <- bp_pp_joined %>%
  select(program, pay_date, year, month, state_id, state, mun_id, municipio, local_id, locality,full_name,gender, total_supplied,obs_id )


# save this dataset
write.csv(bp_pp_joined,
          paste0(dir$cleandata,"bp_propez_joined_all_obs.csv"),
          row.names = F)


# 4. Aggregate to fit the production data format -----------------------------------------------------------

bp_agg <- bp_pp_joined %>%
  group_by(program,year,month,state_id,state,mun_id,municipio,local_id,locality) %>%
  summarise(
    n_male = sum(gender == "MASCULINO", na.rm = TRUE),
    n_female = sum(gender == "FEMENINO", na.rm = TRUE), 
    n_participants = n(),
    total_supplied = sum(total_supplied, na.rm = TRUE)
  )

bp_agg <- arrange(bp_agg, year,month,state,municipio,locality)

# save and export
write.csv(bp_agg,
          paste0(dir$cleandata,"bp_propez_aggregated.csv"),
          row.names = F) # last update June 10 2025



#























