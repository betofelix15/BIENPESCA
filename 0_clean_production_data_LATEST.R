# Title: Fish production data (LATEST VERSION)
# Purpose: Clean and organize production data
# Author: Jesus Felix
# Start date: 21 MAY 2025;
################################################################################


#### Step 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot","tidyverse", 'tmap', 'av',"gifski","magick","stringi","KableExtra",
          "mgsub","zoo","plotly")

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


#### Step 1. load the data and combine it --------------------------------------------------------------------------------

# the following is commented off to show that we did inspect for NA's before doing everything in a loop
# for(i in 2006:2024){ # p <- 2017
#   
#  p <- read.csv(paste0(dir$rawdata, "new_production/Produccion_Pesquera_",i,".csv"),
#                stringsAsFactors = FALSE,
#                check.names = FALSE, 
#                blank.lines.skip = TRUE,
#                fileEncoding = "windows-1252") # use the correct encoding for "ñ"
#  # Remove rows where all values are NA (e.g., ",,,,,,,,,,,")
#  p <- p[rowSums(is.na(p)) != ncol(p), ]
#  
#  # save the columns as p_year
#  assign(paste0("p_", i), p)
# 
# }
# 
# # check if any has missing data/NA's
# na_years <- NULL
# for(i in 2006:2024){
#   
#   df <- get(paste0("p_", i))  # Retrieve the data frame
#   na_rows <- length(which(apply(is.na(df), 1, any)))  # Get the number of rows with any NA for that year
#   df <- as.data.frame(cbind(i,na_rows))
#   na_years <- rbind(na_years,df)
#   
# }
#   
# tail(readLines(paste0(dir$rawdata,"new_production/Produccion_Pesquera_2017.csv")), 10)


# it looks like empty rows are added to 2017,2018,2019,2021,2022. These are added as ,,,,,,,,,,,.
# I think what happened was that CONAPESCA's delimiter program allowed for more observations, so they can match earlier years
# the raw file was inspected and the NA rows come after the real data ends.

# this new loop will load everything.
# load first year as p_raw (2006)
p_raw <- read.csv(paste0(dir$rawdata, "new_production/Produccion_Pesquera_2006.csv"),
                  stringsAsFactors = FALSE,
                  check.names = FALSE,
                  blank.lines.skip = TRUE,
                  fileEncoding = "windows-1252")

# any rows with all NA's in p_raw?
p_raw[is.na(p_raw$`EJERCICIO FISCAL`),] # nope

for(i in 2006:2024){ # i <- 2017

  p <- read.csv(paste0(dir$rawdata, "new_production/Produccion_Pesquera_", i, ".csv"),
                header = TRUE, check.names = FALSE, 
                fileEncoding = "windows-1252",
                na.strings = c("NA"))  # Only interpret real NA strings; "" handled manually
  
  # Convert blank strings to NA explicitly
  p[p == ""] <- NA
     
  # Remove rows where all values are NA (e.g., ",,,,,,,,,,,")
  p <- p[rowSums(is.na(p)) != ncol(p), ]
  
  # match the column names to p_raw
  colnames(p) <- colnames(p_raw)

  # bind to p_raw
  p_raw <- rbind(p_raw,p)

}

# any NA's
p_raw[is.na(p_raw$`EJERCICIO FISCAL`),] # nope


# load muebles imobilarios
inmuebles <- read.csv(paste0(dir$rawdata,"Bienes_Inmuebles_CONAPESCA.csv"),
                  stringsAsFactors = FALSE,
                  check.names = FALSE,
                  blank.lines.skip = TRUE,
                  fileEncoding = "windows-1252")




#### Step 2: Translate to English, name normilization,  and correct grouping------------------------------------------------------------------------------

# translate column names
p_eng <- rename(p_raw,
                "year"    = 1,
                "state"    = 2,
                "office"   = 3,
                "month"    = 4,
                "origin"   = 5,
                "species"   = 6,
                "unloaded_weight_kg"  = 7,
                "live_weight_kg"   = 8,                  
                "value_mx" = 9,
                "category"     = 10,
                "state_id"  = 11,                  
                "office_id"  = 12)



### month to numeric ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fix month (found in feb 2025)
p_eng <- p_eng %>%
  mutate(month = case_when(
    month == "ENERO" ~ 1,
    month == "FEBRERO" ~ 2,
    month == "MARZO" ~ 3,
    month == "ABRIL" ~ 4,
    month == "MAYO" ~ 5,
    month == "JUNIO" ~ 6,
    month == "JULIO" ~ 7,
    month == "AGOSTO" ~ 8,
    month == "SEPTIEMBRE" ~ 9,
    month == "OCTUBRE" ~ 10,
    month == "NOVIEMBRE" ~ 11,
    month == "DICIEMBRE" ~ 12,
    month == "1" ~ 1,
    month == "2" ~ 2,
    month == "3" ~ 3,
    month == "4" ~ 4,
    month == "5" ~ 5,
    month == "6" ~ 6,
    month == "7" ~ 7,
    month == "8" ~ 8,
    month == "9" ~ 9,
    month == "10" ~ 10,
    month == "11" ~ 11,
    month == "12" ~ 12
  ))

# names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check out the names of the office
sort(unique(p_eng$office))

# there are ?ì for an i with an accent and ?æ for Ñ, 
# replace all Ñ with n to avoid multiples
# remove latin articles
p_eng$office <- gsub("\\?æ","N",p_eng$office)
p_eng$office <- gsub("\\Ñ","N",p_eng$office)
p_eng$office <- gsub("\\?ì","I",p_eng$office)
p_eng$office <- gsub("\\Í","I",p_eng$office)
p_eng$office <- gsub("^D\\.F\\.P\\.\\s*", "", p_eng$office)

sort(unique(p_eng$office))


# also things to look for:
# "PEITA DE JALTEMBA","PUERTO PEASCO" ,"VALDIVIA ( PIJIJIAPAN )","EL RANCHITO (COAHUAYANA)"
# "AUASCALIENTES" ., "PIJIJIAPAN "
p_eng <- p_eng %>%
  mutate(
  office = case_when(
    office == "AUASCALIENTES" ~ "AGUASCALIENTES",
    office == "VALDIVIA ( PIJIJIAPAN )" ~ "PIJIJIAPAN",
    office == "VALDIVIA ( PIPIJIAPAN )" ~ "PIJIJIAPAN",
    office == "VALDIVIA ( PIJIJIAPAN)"  ~ "PIJIJIAPAN",
    office == "PIJIJIAPAN "             ~ "PIJIJIAPAN",
    office == "PEITA DE JALTEMBA" ~ "PENITA DE JALTEMBA",
    office == "PUERTO PEASCO" ~ "PUERTO PENASCO",
    office == "EL RANCHITO (COAHUAYANA)" ~ "COAHUAYANA",
    office == "SAHUAYO (COJUMATLAN)" ~ "COJUMATLAN",
    office == "MARQUELIA  CRUZ GRANDE" ~ "MARQUELIA CRUZ GRANDE",
    TRUE ~ office
  ))

sort(unique(p_eng$office))


# fix ids and names for states according to state_names_and_ids in figures from https://www.bing.com/search?q=numeros+identidad+estados+de+mexico&cvid=53cec3a28f6a4468a1cca1b791ae962b&gs_lcrp=EgRlZGdlKgYIABBFGDkyBggAEEUYOdIBCTExMzgxajBqOagCCLACAQ&FORM=ANAB01&PC=U531
#unique(p_eng[c("state","state_id")]) # estado de Mexico is there twice 

df <- p_eng %>%
  mutate(state = case_when(
    state_id == 9 ~ "CIUDAD DE MEXICO",
    state_id == 5 ~ "COAHUILA DE ZARAGOZA",
    state_id == 16 ~ "MICHOACAN DE OCAMPO",
    state_id == 30 ~ "VERACRUZ DE IGNACIO DE LA LLAVE",
    TRUE ~ state
  ))

# check out the unique values for states and offices
ids <- unique(df[c("state","state_id","office","office_id")])

# change CD. to CIUDAD
df$office <- gsub("\\bCD\\.\\s+(?=\\w)", "CIUDAD ", df$office, perl = TRUE)

# change PTO. to PUEROT
df$office <- gsub("\\bPTO\\.\\s+(?=\\w)", "PUERTO ", df$office, perl = TRUE)

# there is no aguascalientes, Yucatan, this needs to be Aguascalientes aguascalientes, there is only one office in Aguascalientes, so no need for Desconocida
df$state <- ifelse(df$office_id == 101, "AGUASCALIENTES",df$state)
df$state_id <- ifelse(df$office_id == 101, 1,df$state_id)
df$office <- ifelse(df$office_id == 199, "AGUASCALIENTES",df$office)
df$office_id <- ifelse(df$office_id == 199, 101, df$office_id)


# el rosario 202 with sinaloa should be 2502
df$office_id <- ifelse(df$state == "SINALOA" &df$office_id == 202, 2502,df$office_id)


# el yucatan san felipe with baja california should be 3107 should be 
df$office_id <- ifelse(df$state == "YUCATAN" &df$office_id == 207, 3107,df$office_id)

# VERACRUZ TUXPAN SHOULD BE 1805
df$office_id <- ifelse(df$state_id == 30 & df$office_id == 1805, 3010,df$office_id)


#unique(df[c("state","state_id")]) # estado de Mexico is there twice 
p_eng <- df

### category names and species ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# are categories/species listed where they should be?
species_cat <- unique(p_eng[,c("species","category")])

# categories
sort(unique(p_eng$category))

#"CAPTURA SIN REGISTRO OFICIAL" and "OTRAS SIN REGISTRO OFICIAL" need to be combined
p_eng$category <- ifelse(p_eng$category == "OTRAS SIN REGISTRO OFICIAL", "CAPTURA SIN REGISTRO OFICIAL", p_eng$category)

#"CAPTURA SIN REGISTRO OFICIAL" and "OTRAS SIN REGISTRO OFICIAL" need to be combined
p_eng$category <- ifelse(p_eng$category == "FAUNA", "FAUNA DE ACOMPANAMIENTO", p_eng$category)

# mero and mero y similares need to be combined
p_eng$category <- ifelse(p_eng$category == "MERO", "MERO Y SIMILARES",p_eng$category)

# ornato and peces de ornato should be together
p_eng$category <- ifelse(p_eng$category == "ORNATO", "PECES DE ORNATO",p_eng$category)

# pepino and pepino de mar should be together
p_eng$category <- ifelse(p_eng$category == "PEPINO ", "PEPINO DE MAR",p_eng$category)

# categories
sort(unique(p_eng$category))

# SPECIES, species should be changed before category organization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# what are the current unique species
species_cat <- unique(p_eng[,c("species","category")])

sort(unique(p_eng$species))

# replace Ñ with N
p_eng$species <- gsub("\\Ñ","N",p_eng$species)

# trim the whitespace from each string, space from last word and quotation
p_eng$species <- trimws(p_eng$species)
sort(unique(p_eng$species))

# fix fauna de acompaniamento
p_eng$species <- ifelse(p_eng$species %in%  c("FAUNA DE ACOMPAAMIENTO", "FAUNA DE ACOMPANAMIENTO DE ACOMPANAMIENTODE ACOMPANAMIENTO"),
                        "FAUNA DE ACOMPANAMIENTO",p_eng$species)

# species t should be TIBURON SE
p_eng$species <- ifelse(p_eng$species=="t","TIBURON SE",p_eng$species)

# BOCADULCE (seperate words)
p_eng$species <- ifelse(p_eng$species=="BOCADULCE","BOCA DULCE",p_eng$species)

# BARBILLLA has three L's
p_eng$species <- ifelse(p_eng$species=="BARBILLLA","BARBILLA",p_eng$species)

#MEDUSA BOLA DE CAON TO MEDUSA BOLA DE CANON
p_eng$species <- ifelse(p_eng$species=="MEDUSA BOLA DE CAON","MEDUSA BOLA DE CANON",p_eng$species)

# MEJILLÓN to MEJILLON
p_eng$species <- ifelse(p_eng$species=="MEJILLÓN","MEJILLON",p_eng$species)

# MOJARRA TENHUAYACA TEHUAYACA to  MOJARRA TENGUAYACA
p_eng$species <- ifelse(p_eng$species %in% c("MOJARRA TENHUAYACA","MOJARRA TEHUAYACA"),"MOJARRA TENGUAYACA",p_eng$species)


# are categories/species listed where they should be? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_cat <- unique(p_eng[,c("species","category")])

# for the ones that say otras when they should say something else
# create a keyword-category mapping
keywords0 <- unique(sort(p_eng[
  p_eng$category %not_in% c("","CAPTURA SIN REGISTRO OFICIAL","FAUNA DE ACOMPANAMIENTO",
                            "MERO Y SIMILARES","OTRAS","PECES DE ORNATO","PEPINO DE MAR","RAYA Y SIMILARES",
                            "RUBIA Y VILLAJAIBA"),
]$category))

# add new keywords
keywords <- c(keywords0,"CANGREJO","ORNATO","PEPINO","MERO","MARLIN","CHOPA","ACOCIL")
new_category <- c(keywords0,"CANGREJO","PECES DE ORNATO","PEPINO DE MAR","MERO Y SIMILARES","MARLIN","MOJARRA","LANGOSTINO")

key_map <- as.data.frame(cbind(keywords,new_category))

# Create unique species-category mapping
species_cat2 <- p_eng %>%
  distinct(species, category) %>%
  mutate(unique_id = row_number())

# Add column for new category
species_cat2$new_category <- species_cat2$category  # default to original

# Loop through each row and assign new category if keyword is found
for (i in 1:nrow(species_cat2)) {
  match_index <- str_detect(species_cat2$species[i], key_map$keywords)
  if (any(match_index)) {
    species_cat2$new_category[i] <- key_map$new_category[which(match_index)[1]]
  }

}
# what it wont pick up: GATA|CORNUDA = Tiburon, KOi fish should be ornato , CALLO DE HACHA, CALLO DE HACHA MEDIA LUNA (almeja)
species_cat2$new_category <- ifelse(species_cat2$species %in% c("BOBO","LISETA"), "LISA",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species == "OTRAS ESPECIES","OTRAS",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species %in% c("GATA","GATA SE","PEZ CORNETA","CORNUDA"),"TIBURON",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species %in% c("PLECOSTOMUS","CARPA KOI","PEZ PAYASO","PAYASO"),"PECES DE ORNATO",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species == "PEZ PUERCO","CARPA",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species == "PEZ VELA","MARLIN",species_cat2$new_category)

species_cat2$new_category <- ifelse(species_cat2$species %in% c("CARITO","CHARRITO","XLAVITA"),"SARDINA",species_cat2$new_category)

# make it unique
species_cat_new <- unique(species_cat2[c("species","new_category")])

# REMOVE EMPTY CATEGORY
species_cat_new <- species_cat_new[!(species_cat_new$species == ""),]

# verify the differences before  joining
species_cat_pre <- unique(p_eng[c("species","category")])

p_eng1 <- left_join(p_eng[,-10],species_cat_new, by = "species")

species_cat_post <- unique(p_eng1[c("species","new_category")])

# make p_eng1 the new p_eng and change the name of species to fish_name (species does not fit names)

p_eng <- p_eng1 %>%
  rename("fish_name" = "species", "fish_category" = "new_category")


# Clear som unnecessary df
rm(list = c("p_eng1","species_cat","species_cat2","species_cat_new","species_cat_pre","species_cat_post",
            "key_map","p","keywords","keywords0","new_category","match_index"))



#### Step 3:  NA's and commas in values/weight--------------------------------------------------------------------------------------

p_edit <- p_eng
# any empty?
which(apply(is.na(p_edit), 1, any)) # zero missing data!

# add a row id to keep track
p_edit$obs_id <- 1:length(p_edit$office_id)

# what kind of characters for weight and value?
str(p_edit)

# pd$unload_wt_kg has commas
df <- p_edit
df$unloaded_weight_kg <- sub(",","",df$unloaded_weight_kg)
df$unloaded_weight_kg <- sub(",","",df$unloaded_weight_kg) # multiple commas
df$unloaded_weight_kg <- as.numeric(df$unloaded_weight_kg)

# live weight
df$live_weight_kg <- sub(",","",df$live_weight_kg)
df$live_weight_kg <- sub(",","",df$live_weight_kg) # multiple commas
df$live_weight_kg <- as.numeric(df$live_weight_kg)


# value
df$value_mx <- sub(",","",df$value_mx)
df$value_mx <- sub(",","",df$value_mx) # multiple commas 

# In value (or revenue) there are some values that are not numerical
na_values <- df %>%
  mutate(value_mx = as.numeric(value_mx)) %>%
  filter(is.na(value_mx))

df[df$obs_id %in% na_values$obs_id,]

# Obs id: 946892 has 1500000,000 for the value for 10000 kg of camaron blanco. This is more than likely supposed to be a period instead of a comma, as the average price for camaron is about 150 peso per kilo.
# we will change that comma in the imputations/adjustments section

# now remove the extra comma for the other ones
df$value_mx <- sub(",","",df$value_mx)

# now check for NA's again
na_values <- df %>%
  mutate(value_mx = as.numeric(value_mx)) %>%
  filter(is.na(value_mx))

df[df$obs_id %in% na_values$obs_id,]

# there are 3 observations that say "CAMARON" for a value, 
# Imputations will come later but this is important to keep track of

# MAKE VALUe numeric
df$value_mx <- as.numeric(df$value_mx)

### zero value in all ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  shouldnt be recording anything with a zero dollar or catch value
zero_all <- df[df$value_mx == 0 & 
                df$unloaded_weight_kg == 0 &
                df$live_weight_kg == 0,]

df <- df[df$obs_id %not_in% zero_all$obs_id,]

# save the zero_all for reference
write.csv(zero_all,
          paste0(dir$cleandata,"production_zero_wt_and_value.csv"),
          row.names = F)

# save as p_edit
p_edit <- df

# remove any scientific notation
options(scipen =999)

# save
# write.csv(p_edit,
#           paste0(dir$cleandata,"edited_production.csv"),
#           row.names = F) # last edited 28 MAY 2025

rm(list = c("df","zero_all"))

#### duplicates -------------------------------------------------------------------
#p_edit <- read.csv(paste0(dir$cleandata,"edited_production.csv"))

### duplicate data? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# make a df for duplication analysis
dup_df <- p_edit %>% 
  group_by(year,month,state,office,origin,unloaded_weight_kg,live_weight_kg,value_mx,fish_name,fish_category) %>% 
  mutate(num_sim_obs = n(), 
         dup_id = row_number(),
         original_obs_id = first(obs_id))%>% 
  ungroup() %>% 
  mutate(is_duplicate = dup_id > 1)

# arrange
dup_df <- arrange(dup_df,original_obs_id)

# number of TRUE
dup_true <- dup_df[dup_df$is_duplicate == TRUE,]

# export this duplicated data in case we need it but it is extremely unlikely that there were two catches
# in the same month in the same place with the same amount in kg and value
# these need to be removed
# however, these may be completions of fish licenses, which list limits to catch. A cooperative can report multiple catches in one month
# for all of their licenses, and for simplicity, divide them up evenly for each fisher. For this reason, we could aggregate them and save 
# as a df with duplicates identified
write.csv(dup_df, 
           paste0(dir$cleandata,"production_data_duplicates_identified_not_removed.csv"),
          row.names = F) # last saved may_28_2025

# we can move forward with the data for the fish that is not duplicated but before removing duplicates
# inform which row had duplicates
# identify the rows that were in dup_true by the original_obs_id
dup_list <- unique(dup_true$original_obs_id)

p_nodup <- p_edit %>%
  filter(obs_id %not_in% dup_true$obs_id)

p_nodup$has_duplicates <- ifelse(p_nodup$obs_id %in% dup_list,1,0)

# save
write.csv(p_nodup,
          paste0(dir$cleandata,"production_data_duplicates_removed.csv"),
          row.names = F) # last saved may_28_2025

# remove unncessary df
rm(list = c("dup_true","dup_list"))

#### Step 5: imputations/adjustments --------------------------------------------------------------------------------

# we must do this for p_nodup and dup_df
# imputed and adjusted columns will be named column_imp_adj
p_nodup <- read.csv(paste0(dir$cleandata,"production_data_duplicates_removed.csv"))
dup_df <- read.csv(paste0(dir$cleandata,"production_data_duplicates_identified_not_removed.csv"))

### NA's in Value and observation 946892
# Obs id: 946892 has 1500000,000 for the value for 10000 kg of camaron blanco. This is more than likely supposed to be a period instead of a comma, as the average price for camaron is about 150 peso per kilo.
# we will change what was a comma to a period
p_nodup$value_mx_imp_adj <- ifelse(p_nodup$obs_id == 946892, "1500000.00",p_nodup$value_mx)
dup_df$value_mx_imp_adj <- ifelse(dup_df$obs_id == 946892, "1500000.00",dup_df$value_mx)


## any zero value_mx p_nodup~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zero_value_mx <- p_nodup[p_nodup$value_mx == 0| is.na(p_nodup$value_mx),] 

# any endangered species?
unique(zero_value_mx$fish_name)
unique(zero_value_mx$fish_category)

# acuacultura se puede vender entonces debe tener un valor, so totoaba that is aquaculture is good
# anything with a less than 1 kg of weight is probably dunage
# any FAUNA DE ACOMPANAMIENTO is removed
# mako shark are endangered, along with other sharks. Its easier just to remove sharks from imputations
# it could be that one shark was captured with other fish and released.
# otras especies is also removed

df <- p_nodup[p_nodup$obs_id %not_in% zero_value_mx$obs_id,]

value_imp_df <- NULL
z_list <- as.list(zero_value_mx$obs_id)
for (f in z_list) {
  
  temp <- zero_value_mx[zero_value_mx$obs_id == f, ]
  
  # go to next under these conditions
  if (
    temp$fish_category %in% c("FAUNA DE ACOMPANAMIENTO", "OTRAS", "RAYA Y SIMILARES", "TIBURON", "PECES DE ORNATO") |
    temp$unloaded_weight_kg < 1 |
    temp$live_weight_kg < 1
  ) {
    next
  }
  
  # Compute mean price
  m_price <- mean(
    df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$value_mx /
      df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$unloaded_weight_kg,
    na.rm = TRUE
  )
  
  temp$value_mx_imp_adj <- temp$unloaded_weight_kg * m_price
  
  value_imp_df <- rbind(value_imp_df, temp)
}

# change the NA in imputed to 0
value_imp_df$value_mx_imp_adj <- ifelse(is.na(value_imp_df$value_mx_imp_adj), 0, value_imp_df$value_mx_imp_adj)

# add to the main data
p_nodup <- p_nodup[!(p_nodup$obs_id %in% value_imp_df$obs_id),]
p_nodup <- rbind(p_nodup,value_imp_df)          

# make it numeric
p_nodup$value_mx_imp_adj <- as.numeric(p_nodup$value_mx_imp_adj)

# check for na
p_nodup[is.na(p_nodup$value_mx_imp_adj)]


## any zero value_mx dup_df~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zero_value_mx <- dup_df[dup_df$value_mx == 0| is.na(dup_df$value_mx),] 

# any endangered species?
unique(zero_value_mx$fish_name)
unique(zero_value_mx$fish_category)

# acuacultura se puede vender entonces debe tener un valor, so totoaba that is aquaculture is good
# anything with a less than 1 kg of weight is probably dunage
# any FAUNA DE ACOMPANAMIENTO is removed
# mako shark are endangered, along with other sharks. Its easier just to remove sharks from imputations
# it could be that one shark was captured with other fish and released.
# otras especies is also removed

df <- dup_df[dup_df$obs_id %not_in% zero_value_mx$obs_id,]

value_imp_df <- NULL
z_list <- as.list(zero_value_mx$obs_id)
for (f in z_list) {
  
  temp <- zero_value_mx[zero_value_mx$obs_id == f, ]
  
  # go to next under these conditions
  if (
    temp$fish_category %in% c("FAUNA DE ACOMPANAMIENTO", "OTRAS", "RAYA Y SIMILARES", "TIBURON", "PECES DE ORNATO") |
    temp$unloaded_weight_kg < 1 |
    temp$live_weight_kg < 1
  ) {
    next
  }
  
  # Compute mean price
  m_price <- mean(
    df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$value_mx /
      df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$unloaded_weight_kg,
    na.rm = TRUE
  )
  
  temp$value_mx_imp_adj <- temp$unloaded_weight_kg * m_price
  
  value_imp_df <- rbind(value_imp_df, temp)
}

# change the NA in imputed to 0
value_imp_df$value_mx_imp_adj <- ifelse(is.na(value_imp_df$value_mx_imp_adj), 0, value_imp_df$value_mx_imp_adj)

# add to the main data
dup_df <- dup_df[!(dup_df$obs_id %in% value_imp_df$obs_id),]
dup_df <- rbind(dup_df,value_imp_df)          

# make it numeric
dup_df$value_mx_imp_adj <- as.numeric(dup_df$value_mx_imp_adj)

### weight in kg p_nodup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zero_wt <- p_nodup[p_nodup$unloaded_weight_kg == 0|
                   p_nodup$live_weight_kg == 0 ,]


# order of loop
# if it has a live weight, use the live weight for missing unloaded
# if it has a unloaded weight, use the unloaded weight for missing live
# if the value is substantially high, above 1000 pesos, find the average price and divide the 
# value by that price
# anything else, next

z_list <- as.list(zero_wt$obs_id)

wt_imp_df <- NULL

df <- p_nodup[p_nodup$obs_id %not_in% zero_wt$obs_id,]

for(i in z_list){
  
  # select the observation
  temp <- zero_wt[zero_wt$obs_id == i,]
  
  # if it has missing unloaded but live weight
  if(temp$unloaded_weight_kg == 0 & temp$live_weight_kg > 0){
    
    temp$unl_wt_kg_imp_adj <- temp$live_weight_kg
    temp$liv_wt_kg_imp_adj <- temp$live_weight_kg
    
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
  
  # if it has missing live but unloaed weight
  if(temp$unloaded_weight_kg > 0 & temp$live_weight_kg == 0){
    
    temp$liv_wt_kg_imp_adj <- temp$unloaded_weight_kg
    temp$unl_wt_kg_imp_adj <- temp$unloaded_weight_kg
    
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
  
  # # if the value is substantially high, above 1000 pesos, find the average price and divide the 
  # value by that price
  if(temp$value_mx >= 1000 & temp$unloaded_weight_kg == 0 & temp$live_weight_kg == 0){
    
    m_price <- mean(
      df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$value_mx /
        df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$unloaded_weight_kg,
      na.rm = TRUE
    )
    
    temp$unl_wt_kg_imp_adj <- temp$value_mx/m_price
    temp$liv_wt_kg_imp_adj <- temp$value_mx/m_price
    
    # save and next
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
    # all else
  else
    {# leave it as is
      
      temp$unl_wt_kg_imp_adj <- temp$unloaded_weight_kg
      temp$liv_wt_kg_imp_adj <- temp$live_weight_kg
      
      wt_imp_df <- rbind(wt_imp_df,temp)
      
    }
}


# add to the main data
p_nodup$unl_wt_kg_imp_adj <- p_nodup$unloaded_weight_kg
p_nodup$liv_wt_kg_imp_adj <- p_nodup$live_weight_kg

p_nodup <- p_nodup[p_nodup$obs_id %not_in% wt_imp_df$obs_id,]
p_nodup <- rbind(p_nodup,wt_imp_df)          

# make it numeric
p_nodup$unl_wt_kg_imp_adj <- as.numeric(p_nodup$unl_wt_kg_imp_adj)
p_nodup$liv_wt_kg_imp_adj <- as.numeric(p_nodup$liv_wt_kg_imp_adj)


# any na's
p_nodup[is.na(p_nodup$unl_wt_kg_imp_adj),]

# remove NA's
p_nodup$unl_wt_kg_imp_adj <- ifelse(is.na(p_nodup$unl_wt_kg_imp_adj),p_nodup$unloaded_weight_kg,p_nodup$unl_wt_kg_imp_adj)
p_nodup$liv_wt_kg_imp_adj <- ifelse(is.na(p_nodup$unl_wt_kg_imp_adj),p_nodup$live_weight_kg,p_nodup$unl_wt_kg_imp_adj)

### weight in kg dup_df ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zero_wt <- dup_df[dup_df$unloaded_weight_kg == 0|
                     dup_df$live_weight_kg == 0 ,]


# order of loop
# if it has a live weight, use the live weight for missing unloaded
# if it has a unloaded weight, use the unloaded weight for missing live
# if the value is substantially high, above 1000 pesos, find the average price and divide the 
# value by that price
# anything else, next

z_list <- as.list(zero_wt$obs_id)

wt_imp_df <- NULL

df <- dup_df[dup_df$obs_id %not_in% zero_wt$obs_id,]

for(i in z_list){
  
  # select the observation
  temp <- zero_wt[zero_wt$obs_id == i,]
  
  # if it has missing unloaded but live weight
  if(temp$unloaded_weight_kg == 0 & temp$live_weight_kg > 0){
    
    temp$unl_wt_kg_imp_adj <- temp$live_weight_kg
    temp$liv_wt_kg_imp_adj <- temp$live_weight_kg
    
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
  
  # if it has missing live but unloaed weight
  if(temp$unloaded_weight_kg > 0 & temp$live_weight_kg == 0){
    
    temp$liv_wt_kg_imp_adj <- temp$unloaded_weight_kg
    temp$unl_wt_kg_imp_adj <- temp$unloaded_weight_kg
    
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
  
  # # if the value is substantially high, above 1000 pesos, find the average price and divide the 
  # value by that price
  if(temp$value_mx >= 1000 & temp$unloaded_weight_kg == 0 & temp$live_weight_kg == 0){
    
    m_price <- mean(
      df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$value_mx /
        df[df$year == temp$year & df$state == temp$state & df$fish_name == temp$fish_name, ]$unloaded_weight_kg,
      na.rm = TRUE
    )
    
    temp$unl_wt_kg_imp_adj <- temp$value_mx/m_price
    temp$liv_wt_kg_imp_adj <- temp$value_mx/m_price
    
    # save and next
    # save and next
    wt_imp_df <- rbind(wt_imp_df,temp)
    
    next
  }
  # all else
  else
  {# leave it as is
    
    temp$unl_wt_kg_imp_adj <- temp$unloaded_weight_kg
    temp$liv_wt_kg_imp_adj <- temp$live_weight_kg
    
    wt_imp_df <- rbind(wt_imp_df,temp)
    
  }
}
  
# add to the main data
dup_df$unl_wt_kg_imp_adj <- dup_df$unloaded_weight_kg
dup_df$liv_wt_kg_imp_adj <- dup_df$live_weight_kg

dup_df <- dup_df[dup_df$obs_id %not_in% wt_imp_df$obs_id,]
dup_df <- rbind(dup_df,wt_imp_df)          

# make it numeric
dup_df$unl_wt_kg_imp_adj <- as.numeric(dup_df$unl_wt_kg_imp_adj)
dup_df$liv_wt_kg_imp_adj <- as.numeric(dup_df$liv_wt_kg_imp_adj)


# any na's
dup_df[is.na(dup_df$unl_wt_kg_imp_adj),]

# remove NA's
dup_df$unl_wt_kg_imp_adj <- ifelse(is.na(dup_df$unl_wt_kg_imp_adj),dup_df$unloaded_weight_kg,dup_df$unl_wt_kg_imp_adj)
dup_df$liv_wt_kg_imp_adj <- ifelse(is.na(dup_df$unl_wt_kg_imp_adj),dup_df$live_weight_kg,dup_df$unl_wt_kg_imp_adj)


### ~~~~ identify the rows that had imputations
p_nodup$has_unl_wt_imputation <- ifelse(p_nodup$unloaded_weight_kg != p_nodup$unl_wt_kg_imp_adj,1,0)
p_nodup$has_liv_wt_imputation <- ifelse(p_nodup$live_weight_kg != p_nodup$liv_wt_kg_imp_adj,1,0)
p_nodup$has_value_imputation <- ifelse(p_nodup$value_mx != p_nodup$value_mx_imp_adj,1,0)

# count them
length(p_nodup[p_nodup$has_unl_wt_imputation == 1,]$obs_id)
length(p_nodup[p_nodup$has_liv_wt_imputation == 1,]$obs_id)
length(p_nodup[p_nodup$has_value_imputation == 1,]$obs_id)

dup_df$has_unl_wt_imputation <- ifelse(dup_df$unloaded_weight_kg != dup_df$unl_wt_kg_imp_adj,1,0)
dup_df$has_liv_wt_imputation <- ifelse(dup_df$live_weight_kg != dup_df$liv_wt_kg_imp_adj,1,0)
dup_df$has_value_imputation <- ifelse(dup_df$value_mx != dup_df$value_mx_imp_adj,1,0)

# count them
length(dup_df[dup_df$has_unl_wt_imputation == 1,]$obs_id)
length(dup_df[dup_df$has_liv_wt_imputation == 1,]$obs_id)
length(dup_df[dup_df$has_value_imputation == 1,]$obs_id)


## ~~~~~~ save theses ~~~~~~~~~~~~~~~

write.csv(p_nodup,
          paste0(dir$cleandata,"production_data_nodup_imputations_included.csv"),
          row.names = F) # last saved 6 june_2025

write.csv(dup_df,
          paste0(dir$cleandata,"production_data_duplicates_imputations_included.csv"),
          row.names = F) # last saved 6 june_2025


### step 6. Final edits, removals, and name changes --------------------------------------------------- 

# remove the observations with zero value in the imputed
p_final_nodup <- p_nodup[p_nodup$value_mx_imp_adj != 0,]

p_final_dup <- dup_df[dup_df$value_mx_imp_adj != 0,]

# remove the observations where both imputed weights are zero
p_final_nodup <- p_final_nodup[p_final_nodup$unl_wt_kg_imp_adj != 0 &
                                 p_final_nodup$liv_wt_kg_imp_adj != 0,]

p_final_dup <- p_final_dup[p_final_dup$unl_wt_kg_imp_adj != 0 &
                             p_final_dup$liv_wt_kg_imp_adj != 0,]

# add MXN to value
p_final_nodup <- rename(p_final_nodup,
                         "value_mxn"= "value_mx",
                        "value_mxn_imp_adj" ="value_mx_imp_adj")


p_final_dup <- rename(p_final_dup,
                      "value_mxn"= "value_mx",
                      "value_mxn_imp_adj" ="value_mx_imp_adj")

# create calculated price from nonimputed and imputed
# keep track of this observation id for verification : 649728
# divide value by unloaded weight, if no unloaded weight, then by live weight (no imputation only)
p_final_nodup$price_mxn_per_kg <- p_final_nodup$value_mxn/p_final_nodup$unloaded_weight_kg
p_final_nodup$price_mxn_per_kg <- ifelse(p_final_nodup$unloaded_weight_kg == 0,
                                         p_final_nodup$value_mxn/p_final_nodup$live_weight_kg,
                                         p_final_nodup$price_mxn_per_kg)

# imputed price
p_final_nodup$price_mxn_per_kg_imp <- p_final_nodup$value_mxn_imp_adj/p_final_nodup$unl_wt_kg_imp_adj
p_final_nodup[p_final_nodup$obs_id == 649728,]

# price for the duplicate data
p_final_dup$price_mxn_per_kg <- p_final_dup$value_mxn/p_final_dup$unloaded_weight_kg
p_final_dup$price_mxn_per_kg <- ifelse(p_final_dup$unloaded_weight_kg == 0,
                                         p_final_dup$value_mxn/p_final_dup$live_weight_kg,
                                         p_final_dup$price_mxn_per_kg)

# imputed price
p_final_dup$price_mxn_per_kg_imp <- p_final_dup$value_mxn_imp_adj/p_final_dup$unl_wt_kg_imp_adj
p_final_dup[p_final_dup$obs_id == 649728,]


# reposition the columns
p_final_nodup <- p_final_nodup %>%
  dplyr::select(year, month, state, office, state_id,office_id, origin, fish_category,fish_name,
         unloaded_weight_kg,live_weight_kg,value_mxn,price_mxn_per_kg,unl_wt_kg_imp_adj,liv_wt_kg_imp_adj,
         value_mxn_imp_adj,price_mxn_per_kg_imp,has_unl_wt_imputation,has_liv_wt_imputation,has_value_imputation,has_duplicates,obs_id)

p_final_dup <- p_final_dup %>%
  dplyr::select(year, month, state, office, state_id,office_id, origin, fish_category,fish_name,
         unloaded_weight_kg,live_weight_kg,value_mxn,price_mxn_per_kg,unl_wt_kg_imp_adj,liv_wt_kg_imp_adj,value_mxn_imp_adj,
         price_mxn_per_kg_imp,has_unl_wt_imputation,has_liv_wt_imputation,has_value_imputation,is_duplicate,dup_id,num_sim_obs,
         original_obs_id,obs_id)


# round numeric columns to second decimal
p_final_nodup[] <- lapply(p_final_nodup, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})

p_final_dup[] <- lapply(p_final_dup, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})


# rearrange to order
p_final_nodup <- arrange(p_final_nodup, year,month,state,state_id,office,office_id,fish_category,fish_name)

p_final_dup <- arrange(p_final_dup, year,month,state,state_id,office,office_id,fish_category,fish_name)

# new fixes (june 10 2025)
p_final_nodup <- read.csv(paste0(dir$cleandata,"final_production_no_duplicates.csv"))
p_final_dup <- read.csv(paste0(dir$cleandata,"final_production_with_duplicates.csv"))

office_id <- unique(p_final_nodup[c("state_id","state","office_id","office")]) 


# fixes that needed to be made
# some fixes needed in prod, change office names by latest name
prod <- p_final_nodup # DO THIS AGAIN FOR P_FINAL_DUP
prod$state <- ifelse(prod$office_id == 901,"CIUDAD DE MEXICO",prod$state )
prod$state_id <- ifelse(prod$office_id == 901,9,prod$state_id )
prod$office <- ifelse(prod$office_id == 901,"BENITO JUAREZ",prod$office)
prod$office <- ifelse(prod$office_id == 707,"VILLA FLORES",prod$office)
prod$office <- ifelse(prod$office_id == 1101,"GUANAJUATO",prod$office)
prod$office <- ifelse(prod$office_id == 1203,"ARCELIA",prod$office)
prod$office <- ifelse(prod$state == "SINALOA" & prod$office == 'OFICINAS CENTRALES',
                      "MAZATLAN",prod$office) # the oficinas centrales are in Mazatlan
prod$office_id <- ifelse(prod$office == "MAZATLAN",2508,prod$office_id)
prod$office <- ifelse(prod$office_id == 2704,"CORONEL ANDRES SANCHEZ MAGALLANES",prod$office)
prod$office <- ifelse(prod$office_id == 3101,"CELESTUN",prod$office)
prod$office_id <- ifelse(prod$office_id == 2200,2201,prod$office_id)
prod$office <- ifelse(prod$office_id %in% c(410,1410) & prod$state == "CAMPECHE","SAN ANTONIO CARDENAS",prod$office)
prod$office_id <- ifelse(prod$office == "SAN ANTONIO CARDENAS", 410,prod$office_id)
prod$office <- ifelse(prod$office_id == 204,"ISLA DE CEDROS",prod$office)
prod$office <- ifelse(prod$office_id == 301,"BAHIA TORTUGAS",prod$office)
prod$office <- ifelse(prod$office_id == 2308,"JAVIER ROJO GOMEZ (PUNTA ALLEN)",prod$office)

# change CD. to CIUDAD
prod$office <- gsub("\\bCD\\.\\s+(?=\\w)", "CIUDAD ", prod$office, perl = TRUE)

# change PTO. to PUERTO
prod$office <- gsub("\\bPTO\\.\\s+(?=\\w)", "PUERTO ", prod$office, perl = TRUE)

prod$office <- gsub("\\.", "", prod$office, perl = TRUE)

office_id <- unique(prod[c("state_id","state","office_id","office")])

p_final_nodup <- prod
# save the final datasets

write.csv(p_final_nodup,
          paste0(dir$cleandata,"final_production_no_duplicates.csv"),
          row.names = F) # last updated 10 JUNE 2025

write.csv(p_final_dup,
          paste0(dir$cleandata,"final_production_with_duplicates.csv"),
          row.names = F) # last updated 10 JUNE 2025

# summary of production data

#p_final_nodup <- read.csv(paste0(dir$cleandata,"final_production_no_duplicates.csv"))
#p_final_dup <- read.csv(paste0(dir$cleandata,"final_production_with_duplicates.csv"))



## explanation of variables --------------------------------------------------------------------

variable <- colnames(p_final_nodup)
explanation <- c("year of observation",
                 "Month of observation",
                 "state in Mexico where conapesca office is located",
                 "Conapesca office in Mexico where data is recorded/attributed to",
                 "Identifier for state",
                 "Identifier for the office",
                 "CAPTURA: fish was caught in the sea, lake, river, etc. AQUACULTURA: fish was farmed in aquaculture",
                 "Category of fish listed by Conapesca",
                 "Fish name reported by fisher or fishing cooperative",
                 "Weight in kg of fish caught reported by fisher or fishing cooperative at time it was unloaded from boat",
                 "Weight in kg of fish reported by fisher or fishing cooperative at the moment it was caught or kept alive (ornate fish are typically pets)",
                 "Total value in Mexican Pesos (MXN) of the catch reported by fisher or fishing cooperative",
                 "The calculated unit price in MXN per kg. This is done by dividing revenue by unloaded weight (or live weight where unloaded weight is missing).",
                 "Imputed uloaded weight in kg when it was zero or missing when value exists. If live weight was present, it uses that. Else, the price is calculated from an average in that year of the fish in the respective state and existing value is divided by it.",
                 "Imputed live weight in kg when it was zero or missing exists. If unloaded weight was present, it uses that. Else, the price is calculated from an average in that year of the fish in the respective state and existing value is divided by it.",
                 "Imputed value in MXN for missing or zero value when weight exists. the price is calculated from an average in that year of the fish in the respective state and multiplied to existing weight.",
                 "The calculated unit price in MXN per kg. This is done by dividing imputed value by imputed unloaded weight.",
                 "1 means this row had unloaded weight imputed in unl_wt_kg_imp_adj",
                 "1 means row has live weight imputations in liv_wt_kg_imp_adj",
                 "1 means the value is imputed in value_mxn_imp_adj",
                 "1 means it has duplicates in the raw data",
                 "row specific identifier")
df <- as.data.frame(cbind(variable,explanation))

# save this
write.csv(df,
          paste0(dir$cleandata,"column_explanation_final_dataset.csv"),
          row.names = F)
