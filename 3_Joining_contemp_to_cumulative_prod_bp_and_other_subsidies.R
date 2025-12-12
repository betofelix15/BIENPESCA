# Title: 3: Joining and aggregating BP, Production Data, and other subsidies
# Purpose: Analyze possible outcome variables and combine them with BP data
# Author: Jesus Felix
# Start date: 14 june 2025
################################################################################
# This version does municipio level data first then summarizes for the states
# this is when deflation takes place. Every monetary value after this is considered
# deflated at 2022 as the base year.

# 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata")

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

# 1. LOAD DATA ( updated 13 Nov 2025 to include gas sub)---------------------------------------------------------------------------------------------------------------------

# bp data 
bp <- read.csv(paste0(dir$cleandata,"agg_bp_data_with_office.csv"))

# production data
prod <- read.csv(paste0(dir$cleandata,"final_production_no_duplicates.csv"))

# load the fuel subsidy
gas <- read.csv(paste0(dir$cleandata,"gas_ribereno_summarized_with_office.csv"))

# load the diesel subsidy
diesel <- read.csv(paste0(dir$cleandata,"diesel_marino_summarized_with_office.csv"))

# other subsidies
other_sub <- read.csv(paste0(dir$cleandata,"other_subsidies_summarized_w_office.csv"))


# cpi data for price deflation
# cpi <- read_excel(paste0(dir$rawdata,"cpi_monthly_by_city_mexico.xlsx"))
# 
# cpi <- as.data.frame(cpi[13:292,1:2])
# colnames(cpi) <- c("date","national_cpi")
# cpi <- cpi %>%
#   separate(date, c("month_char","year"), " ")
# 
# month_char <- unique(cpi$month_char)
# month <- 1:12
# m_df <- as.data.frame(cbind(month_char,month))
# cpi <- left_join(cpi,m_df, by = "month_char")
# cpi$national_cpi <- as.numeric(cpi$national_cpi)
# 
# # change base year? (dec 2022)
# cpi$nat_cpi_2022_base <- cpi$national_cpi/(cpi[cpi$year == 2022 &
#                                                cpi$month == 12,]$national_cpi)
# export this 
#write.csv(cpi, paste0(dir$cleandata, "cpi_clean_mexico_monthly.csv"), row.names = F)
cpi <- read.csv(paste0(dir$cleandata, "cpi_clean_mexico_monthly.csv"))


cpi_annual <- cpi %>%
  group_by(year) %>%
  summarize(
    cpi = median(nat_cpi_2022_base)
  )
cpi_annual$year <- as.numeric(cpi_annual$year)


# 2. JOIN BP, PRODUCTION, AND OTHER SUB in aggregates, office frequency, deflate --------------------------------------------------------------------------------------------------------


# aggregate bp to the office and office id and year (using month is too complicated)
bp_agg <- bp %>%
  left_join(cpi_annual, by = "year") %>%
  group_by(year,office_id,office,program) %>% # don't aggregate to state because we know that there are some offices outside of bp location states
  reframe(contemp_male = sum(n_male),
            contemp_female = sum(n_female),
            contemp_participants = sum(n_participants),
            contemp_bp = sum(total_supplied/cpi, na.rm = T)
  )


# aggregate the subsidies and deflate
gas_agg <- gas %>%
  left_join(cpi_annual, by = "year") %>%
  group_by(year,state_id,state,office_id,office) %>%
  summarise(
    gas_mxn_value = sum(subsidy_mxn_value/cpi, na.rm = T)
  )


dis_agg <- diesel %>%
  left_join(cpi_annual, by = "year") %>%
  group_by(year,state_id,state,office_id,office) %>%
  summarise(
    diesel_mxn_value = sum(subsidy_mxn_value/cpi, na.rm = T)
  )

osub_agg <- other_sub %>%
  left_join(cpi_annual, by = "year") %>%
  group_by(year,state_id,state,office_id,office) %>%
  summarise(
    modernacion_mxn_value = sum(total_mxn_modernacion/cpi, na.rm = TRUE),
    inspeccion_mxn_value = sum(total_mxn_inspeccion/cpi, na.rm = TRUE),
    adiestramiento_mxn_value = sum(total_mxn_adiestramiento/cpi, na.rm = TRUE),
    cadenas_mxn_value = sum(total_mxn_cadenas/cpi, na.rm = TRUE),
    acuacultura_mxn_value = sum(total_mxn_acuacultura/cpi, na.rm = TRUE),
    transformacion_mxn_value = sum(total_mxn_transformacion/cpi, na.rm = TRUE),
    estudios_mxn_value = sum(total_mxn_estudios/cpi, na.rm = TRUE)
  )

# really we are just interested in the lumping these into an aggregate, but acuacultura is of interest
osub_agg$other_sub_mxn_value <- osub_agg$modernacion_mxn_value + osub_agg$inspeccion_mxn_value + osub_agg$adiestramiento_mxn_value +
                                osub_agg$transformacion_mxn_value + osub_agg$estudios_mxn_value + osub_agg$cadenas_mxn_value
  


# aggregate production but identify key fisheries and imputations

# IDENTIFY KEY NON-BP FISH
sport_fish <- c("MARLIN SE","PEZ VELA","MARLIN AZUL","MARLIN NEGRO","MARLIN BLANCO","MARLIN RAYADO","PEZ ESPADA","SABALO","PEZ GALLO","PEZ DORADO")
non_bp_fish <- c(unique(prod[prod$fish_category %in% c("FAUNA DE ACOMPANAMIENTO","PECES DE ORNATO"),]$fish_name),
                        sport_fish)

# summarize. (updated in 9 DEC 2025 to remove aquaculture from aggregate and deflate at the same time)
p_agg <- prod %>%
  left_join(cpi_annual, by = "year") %>%
  group_by(year,month,state_id, state,office_id,office) %>% 
  reframe(
          # don't worry about live weight, this pertains to fish needed alive (live weight is silenced)    
          # all of the non-bp fish (this includes sport fish)
          agg_unld_wt_kg = sum(unloaded_weight_kg[fish_name %not_in% non_bp_fish]),
          # agg_live_wt_kg = sum(live_weight_kg[fish_name %not_in% non_bp_fish]),
          agg_rev_mxn = sum(value_mxn[fish_name %not_in% non_bp_fish]/cpi, na.rm = T),
          agg_unld_wt_kg_imp = sum(unl_wt_kg_imp_adj[fish_name %not_in% non_bp_fish], na.rm = T),
          #agg_live_wt_kg_imp = sum(liv_wt_kg_imp_adj[fish_name %not_in% non_bp_fish], na.rm = T),
          agg_rev_mxn_imp = sum(value_mxn_imp_adj[fish_name %not_in% non_bp_fish]/cpi, na.rm = T),
          n_unl_wt_imp = sum(has_unl_wt_imputation[fish_name %not_in% non_bp_fish], na.rm = T),
          #n_liv_wt_imp = sum(has_liv_wt_imputation[fish_name %not_in% non_bp_fish], na.rm = T),
          n_unl_wt_imp = sum(has_unl_wt_imputation[fish_name %not_in% non_bp_fish], na.rm = T),
          n_rev_imp = sum(has_value_imputation[fish_name %not_in% non_bp_fish], na.rm = T),
          n_duplicates = sum(has_duplicates[fish_name %not_in% non_bp_fish], na.rm = T),
          avg_price_mxn_per_kg  = agg_rev_mxn/agg_unld_wt_kg,
          avg_imp_price_mxn_per_kg = agg_rev_mxn_imp/agg_unld_wt_kg_imp,
          
          
          # all of the non-bp fish without aquaculture
          agg_unld_wt_kg_non_ac = sum(unloaded_weight_kg[fish_name %not_in% non_bp_fish & origin == "CAPTURA"]),
          #agg_live_wt_kg_non_ac = sum(live_weight_kg[fish_name %not_in% non_bp_fish& origin == "CAPTURA"]),
          agg_rev_mxn_non_ac = sum(value_mxn[fish_name %not_in% non_bp_fish& origin == "CAPTURA"]/cpi, na.rm = T),
          agg_unld_wt_kg_imp_non_ac = sum(unl_wt_kg_imp_adj[fish_name %not_in% non_bp_fish & origin == "CAPTURA"], na.rm = T),
          #agg_live_wt_kg_imp_non_ac = sum(liv_wt_kg_imp_adj[fish_name %not_in% non_bp_fish & origin == "CAPTURA"], na.rm = T),
          agg_rev_mxn_imp_non_ac = sum(value_mxn_imp_adj[fish_name %not_in% non_bp_fish & origin == "CAPTURA"]/cpi, na.rm = T),
          
          
          
          # Shrimp (without aquaculture)
          
          agg_unld_wt_shrimp = sum(unloaded_weight_kg[fish_category == "CAMARON" & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_shrimp = sum(value_mxn[fish_category == "CAMARON" & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          agg_unld_wt_kg_imp_shrimp = sum(unl_wt_kg_imp_adj[fish_category == "CAMARON" & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_imp_shrimp = sum(value_mxn_imp_adj[fish_category == "CAMARON" & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          avg_price_mxn_per_kg_shrimp = agg_rev_mxn_shrimp/agg_unld_wt_shrimp,
          avg_imp_price_mxn_per_kg_shrimp = agg_rev_mxn_imp_shrimp/agg_unld_wt_kg_imp_shrimp,
          
          # lisa (without aquaculture)
          
          agg_unld_wt_lisa = sum(unloaded_weight_kg[fish_category == "LISA" & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_lisa = sum(value_mxn[fish_category == "LISA" & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          agg_unld_wt_kg_imp_lisa = sum(unl_wt_kg_imp_adj[fish_category == "LISA" & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_imp_lisa = sum(value_mxn_imp_adj[fish_category == "LISA" & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          avg_price_mxn_per_kg_lisa = agg_rev_mxn_lisa/agg_unld_wt_lisa,
          avg_imp_price_mxn_per_kg_lisa = agg_rev_mxn_imp_lisa/agg_unld_wt_kg_imp_lisa,
          
          # sport fish (shouldnt have aquaculture, but just to make sure.....)
          
          agg_unld_wt_sport = sum(unloaded_weight_kg[fish_name %in% sport_fish & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_sport = sum(value_mxn[fish_category %in% sport_fish & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          agg_unld_wt_kg_imp_sport = sum(unl_wt_kg_imp_adj[fish_category %in% sport_fish & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_imp_sport = sum(value_mxn_imp_adj[fish_category %in% sport_fish & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          avg_price_mxn_per_kg_sport = agg_rev_mxn_sport/agg_unld_wt_sport,
          avg_imp_price_mxn_per_kg_sport = agg_rev_mxn_imp_sport/agg_unld_wt_kg_imp_sport,
          
          # full placebo, all non_bp fish (without aquaculture)
          
          agg_unld_wt_placebo = sum(unloaded_weight_kg[fish_name %in% non_bp_fish & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_placebo = sum(value_mxn[fish_category %in% non_bp_fish & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          agg_unld_wt_kg_imp_placebo = sum(unl_wt_kg_imp_adj[fish_category %in% non_bp_fish & origin == "CAPTURA"], na.rm = TRUE),
          agg_rev_mxn_imp_placebo = sum(value_mxn_imp_adj[fish_category %in% non_bp_fish & origin == "CAPTURA"]/cpi, na.rm = TRUE),
          avg_price_mxn_per_kg_placebo = agg_rev_mxn_placebo/agg_unld_wt_placebo,
          avg_imp_price_mxn_per_kg_placebo = agg_rev_mxn_imp_placebo/agg_unld_wt_kg_imp_placebo,
          
          
        
          # aquaculture
          agg_unld_wt_aquaculture = sum(unloaded_weight_kg[origin == "ACUACULTURA"], na.rm = TRUE),
          agg_rev_mxn_aquaculture = sum(value_mxn[origin == "ACUACULTURA"]/cpi, na.rm = TRUE),
          agg_unld_wt_kg_imp_aquaculture = sum(unl_wt_kg_imp_adj[origin == "ACUACULTURA"], na.rm = TRUE),
          agg_rev_mxn_imp_aquaculture = sum(value_mxn_imp_adj[origin == "ACUACULTURA"]/cpi, na.rm = TRUE),
          avg_price_mxn_per_kg_aquaculture = agg_rev_mxn_aquaculture/agg_unld_wt_aquaculture,
          avg_imp_price_mxn_per_kg_aquaculture = agg_rev_mxn_imp_aquaculture/agg_unld_wt_kg_imp_aquaculture
          
          
          )


# Left join  prod with bp
join <- left_join(p_agg,bp_agg, by = c("year","office_id","office"))


# make any propesca/bienpesca numerical related stuff from NA to zero
join$contemp_male <- ifelse(is.na(join$contemp_male),0,join$contemp_male)
join$contemp_female <- ifelse(is.na(join$contemp_female),0,join$contemp_female)
join$contemp_participants <- ifelse(is.na(join$contemp_participants),0,join$contemp_participants)
join$contemp_bp <- ifelse(is.na(join$contemp_bp),0,join$contemp_bp)


# left join join with all the other subsidies
all_join <- left_join(join, gas_agg, by = c("year","state_id","state","office_id","office"))
all_join <- left_join(all_join, dis_agg, by = c("year","state_id","state","office_id","office"))
all_join <- left_join(all_join, osub_agg %>% select(year,state_id,state,office_id,office,other_sub_mxn_value,acuacultura_mxn_value), 
                      by = c("year","state_id","state","office_id","office"))


# make any subsidy numerical related stuff from NA to zero
all_join$gas_mxn_value <- ifelse(is.na(all_join$gas_mxn_value),0,all_join$gas_mxn_value)
all_join$diesel_mxn_value <- ifelse(is.na(all_join$diesel_mxn_value),0,all_join$diesel_mxn_value)
all_join$acuacultura_mxn_value <- ifelse(is.na(all_join$acuacultura_mxn_value),0,all_join$acuacultura_mxn_value)
all_join$other_sub_mxn_value <- ifelse(is.na(all_join$other_sub_mxn_value),0,all_join$other_sub_mxn_value)

all_join$program <- ifelse(is.na(all_join$program),"NO PP OR BP",all_join$program)


# make columns for each year and check what offices have that year
office_joint_freq <- NULL
for(o in as.list(unique(all_join$office_id))){ # o <- 101
  
  temp <- all_join[all_join$office_id == o,c(1:6)]
  
  yr_dum <- temp[1,c(3:6)] # use the first observations to save office info without year/month
  for(i in 2006:2024){ # i <- 2006
    df <- temp[1,c(3:6)] 
    if(i %in% temp$year){
      dum <- 1
    }else{
      dum <- 0
    }
    df$dum <- dum
    colnames(df)[5] <- paste("dum_",i, sep = "_")
    yr_dum <- cbind(yr_dum,df[5])
    
  }
  office_joint_freq <- rbind(office_joint_freq,yr_dum)
}
# sum across the rows
office_joint_freq$total_yrs <- rowSums(office_joint_freq[ , c(5:23)], na.rm=TRUE)

# export these
# write.csv(join, paste0(dir$cleandata,"bp_pd_join_13_nov_2025.csv"), row.names = F)
write.csv(all_join, paste0(dir$cleandata,"bp_pd_and_other_subs_joined.csv"), row.names = F)
write.csv(office_joint_freq, paste0(dir$cleandata,"office_joint_freq.csv"), row.names = F)





######### here we make 3 seperate datasets. Description of each below -------------------------------------------------------
# dataset 1) no combining offices, only the offices that existed throughout the entire period (17 yrs)
# dataset 2) offices are combined if they have less than 10 years of production, 10 and more get filled with zeros 
# dataset 3) keep all offices but no combining

######### dataset 1: keep only offices with strictly 19 years (use this one!) ------------------------------------------------------------------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"bp_pd_and_other_subs_joined.csv"))
office_joint_freq <- read.csv(paste0(dir$cleandata,"office_joint_freq.csv"))

ds_1 <- all_join[all_join$office_id %in%
                     office_joint_freq[office_joint_freq$total_yrs == 19,]$office_id,]
length(unique(ds_1$office_id)) #134 offices

# make a dataframe to work on
d <- unique(ds_1[c("year", "state_id", "state", 'office_id', "office",'contemp_bp','gas_mxn_value','diesel_mxn_value','acuacultura_mxn_value','other_sub_mxn_value')]) 

# make a column for all subsidies
d <- d %>%
  mutate(all_sub_mxn_value = rowSums(across(6:10), na.rm = TRUE))

# add the cumulative exposure

d$cumulative_bp <- ave(d$contemp_bp, d$office_id, FUN=cumsum)

d$cumulative_gas_mxn <- ave(d$gas_mxn_value, d$office_id, FUN = cumsum)

d$cumulative_diesel_mxn <- ave(d$diesel_mxn_value, d$office_id, FUN = cumsum)

d$cumulative_acuacultura_mxn <- ave(d$acuacultura_mxn_value, d$office_id, FUN = cumsum)

d$cumulative_other_sub_mxn <- ave(d$other_sub_mxn_value, d$office_id, FUN = cumsum)

d$cumulative_all_sub_mxn <- ave(d$all_sub_mxn_value, d$office_id, FUN = cumsum)


# create a dummy that begins when treatment starts for all subsidie
d$bp_treated <- ifelse(d$cumulative_bp > 0, 1,0)
d$gas_treated <- ifelse(d$cumulative_gas_mxn > 0, 1,0)
d$diesel_treated <- ifelse(d$cumulative_diesel_mxn > 0, 1,0)
d$acua_treated <- ifelse(d$cumulative_acuacultura_mxn > 0, 1,0)
d$other_treated <- ifelse(d$cumulative_other_sub_mxn > 0, 1,0)
d$all_treated <- ifelse(d$cumulative_all_sub_mxn > 0, 1,0)

# visualize the distribution

# What is the average for treatment over all the years for those treated?
all_sum <- summary(d[d$cumulative_all_sub_mxn > 0,]$cumulative_all_sub_mxn)
bp_sum <- summary(d[d$cumulative_bp > 0,]$cumulative_bp)
gas_sum <- summary(d[d$cumulative_gas_mxn > 0,]$cumulative_gas_mxn)
diesel_sum <- summary(d[d$cumulative_diesel_mxn > 0,]$cumulative_diesel_mxn)
acua_sum <- summary(d[d$cumulative_acuacultura_mxn > 0,]$cumulative_acuacultura_mxn)
other_sum <- summary(d[d$cumulative_other_sub_mxn > 0,]$cumulative_other_sub_mxn)


# create a cumulative treatment dummy for 1st quarter, 2nd quarter (median), 3rd quarter, and the average

d$all_cmltv_1q <- ifelse(d$cumulative_bp  >=  all_sum[2], 1,0) # if cumulative for all subsidies reaches its first quarter quantities
d$all_cmltv_2q <- ifelse(d$cumulative_bp  >=  all_sum[3], 1,0) # if cumulative for all subsidies reaches its first quarter quantities
d$all_cmltv_3q <- ifelse(d$cumulative_bp  >=  all_sum[5], 1,0) # if cumulative for all subsidies reaches its first quarter quantities
d$all_cmltv_avg <- ifelse(d$cumulative_bp  >=  all_sum[4], 1,0) # if cumulative for all subsidies reaches its first quarter quantities

d$bp_cmltv_1q <- ifelse(d$cumulative_bp  >=  bp_sum[2], 1,0) # if cumulative for bp subsidies reaches its first quarter quantities
d$bp_cmltv_2q <- ifelse(d$cumulative_bp  >=  bp_sum[3], 1,0) # if cumulative for bp subsidies reaches its first quarter quantities
d$bp_cmltv_3q <- ifelse(d$cumulative_bp  >=  bp_sum[5], 1,0) # if cumulative for bp subsidies reaches its first quarter quantities
d$bp_cmltv_avg <- ifelse(d$cumulative_bp  >=  bp_sum[4], 1,0) # if cumulative for bp subsidies reaches its first quarter quantities

d$gas_cmltv_1q <- ifelse(d$cumulative_gas_mxn  >=  gas_sum[2], 1,0) # if cumulative for gas subsidies reaches its first quarter quantities
d$gas_cmltv_2q <- ifelse(d$cumulative_gas_mxn  >=  gas_sum[3], 1,0) # if cumulative for gas subsidies reaches its first quarter quantities
d$gas_cmltv_3q <- ifelse(d$cumulative_gas_mxn  >=  gas_sum[5], 1,0) # if cumulative for gas subsidies reaches its first quarter quantities
d$gas_cmltv_avg <- ifelse(d$cumulative_gas_mxn  >=  gas_sum[4], 1,0) # if cumulative for gas subsidies reaches its first quarter quantities

d$diesel_cmltv_1q <- ifelse(d$cumulative_diesel_mxn  >=  diesel_sum[2], 1,0) # if cumulative for diesel subsidies reaches its first quarter quantities
d$diesel_cmltv_2q <- ifelse(d$cumulative_diesel_mxn  >=  diesel_sum[3], 1,0) # if cumulative for diesel subsidies reaches its first quarter quantities
d$diesel_cmltv_3q <- ifelse(d$cumulative_diesel_mxn  >=  diesel_sum[5], 1,0) # if cumulative for diesel subsidies reaches its first quarter quantities
d$diesel_cmltv_avg <- ifelse(d$cumulative_diesel_mxn  >=  diesel_sum[4], 1,0) # if cumulative for diesel subsidies reaches its first quarter quantities

d$acua_cmltv_1q <- ifelse(d$cumulative_acuacultura_mxn  >=  acua_sum[2], 1,0) # if cumulative for acua subsidies reaches its first quarter quantities
d$acua_cmltv_2q <- ifelse(d$cumulative_acuacultura_mxn  >=  acua_sum[3], 1,0) # if cumulative for acua subsidies reaches its first quarter quantities
d$acua_cmltv_3q <- ifelse(d$cumulative_acuacultura_mxn  >=  acua_sum[5], 1,0) # if cumulative for acua subsidies reaches its first quarter quantities
d$acua_cmltv_avg <- ifelse(d$cumulative_acuacultura_mxn  >=  acua_sum[4], 1,0) # if cumulative for acua subsidies reaches its first quarter quantities

d$other_cmltv_1q <- ifelse(d$cumulative_other_sub_mxn  >=  other_sum[2], 1,0) # if cumulative for other subsidies reaches its first quarter quantities
d$other_cmltv_2q <- ifelse(d$cumulative_other_sub_mxn  >=  other_sum[3], 1,0) # if cumulative for other subsidies reaches its first quarter quantities
d$other_cmltv_3q <- ifelse(d$cumulative_other_sub_mxn  >=  other_sum[5], 1,0) # if cumulative for other subsidies reaches its first quarter quantities
d$other_cmltv_avg <- ifelse(d$cumulative_other_sub_mxn  >=  other_sum[4], 1,0) # if cumulative for other subsidies reaches its first quarter quantities

# 
# ### visualize the change in untreated to treated with cumulative exposure ### 
# delta_yrs <-  d %>% 
#   filter(year > 2008) %>%
#   group_by(year) %>% 
#   summarise(
#     treated = sum(all_treated),
#     untreated = (length(unique(d$office_id))-treated),
#     
#     t_500k = sum(c_500k),
#     t_1m = sum(c_1m),
#     t_5m = sum(c_5m),
#     t_med = sum(c_median),  
#     t_avg = sum(c_avg),
#     
#     ut_500k = length(unique(d$office_id))-t_500k,
#     ut_1m = length(unique(d$office_id))-t_1m,
#     ut_5m = length(unique(d$office_id))-t_5m,
#     ut_med = length(unique(d$office_id))-t_med, 
#     ut_avg = length(unique(d$office_id))-t_avg,
#   )
# 
# 
# png(paste0(dir$fig, "change_cumulative_exposure_dataset1.png"))
# #create line plot of treated
# plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,140),
#      xlab = "Years", ylab = "# of offices", main = "Dataset 1: Change in treated per year")
# 
# #overlay line plot of untreated
# lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,140), lty = 2, lwd = 2)
# lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,140), lty = 4, lwd = 1)
# lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,140), lty = 5, lwd = 2)
# lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,140), lty = 6, lwd = 3)
# 
# #add legend
# legend(2020,60, legend=c('treated',
#                          "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
#        col=c('black',
#              '#ffdb00',"#ee7b06",'#a12424','#996600'),
#        lty=c(1,2,4,5,6),
#        
#        cex = .7)
# dev.off()

# add d to all_join
d1_join <- left_join(ds_1 %>% select(-contemp_bp,-gas_mxn_value,-diesel_mxn_value,-acuacultura_mxn_value,-other_sub_mxn_value),
                     d, by = c("year", "state_id", "state", 'office_id', "office"))

# export the delta_years
#write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset1_13_nov_2025.csv"), row.names = F)

# export the main
write.csv(d1_join, paste0(dir$cleandata, "monthly_prod_annual_subsidies_9_dec_2025.csv"), row.names = F)

###  STOP HERE, NO NEED TO KEEP GOING!
######### dataset 2: Combining offices ---------------------------------------------------------------------------------------------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"all_join.csv"))
office_joint_freq <- read.csv(paste0(dir$cleandata,"office_joint_freq.csv"))

# now the offices that existed for at least 10 years (more than half of the time)
semi_office <- all_join[all_join$office_id %in%
                            office_joint_freq[office_joint_freq$total_yrs > 9,]$office_id,]
length(unique(semi_office$office_id))

# all the offices that existed for half the time or less
bad_office <- all_join[all_join$office_id %in%
                           office_joint_freq[office_joint_freq$total_yrs < 10,]$office_id,]
length(unique(bad_office$office_id)) #19 offices had less than 10 years

# we can combine the offices by distance
office_sf <- st_read(paste0(dir$shp, "office_points_latest.shp"))

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")


# fill datasets for loop
semi <- office_sf[office_sf$office_id %in% semi_office$office_id,]
bf_check <- list(NULL)
agg_wt_semi <- NULL
contemp_bp_semi <- NULL
for(i in as.list(unique(bad_office$office_id))){ # i <- 308
  
  bad <- office_sf[office_sf$office_id == i,]
  
  # make a buffer for each of these big enough to cover at most 4 offices
  for(s in as.list(seq(10000,400000, by = 10000))){ # s <- 100000 
    
    bf <- st_buffer(bad,dist = s)
    bf_check <- st_contains(bf,semi)
    
    # once the buffer captures at least 1 office and no more than 4, stop loop
    if(length(bf_check[[1]] > 1 & length(bf_check[[1]]) <= 4)){
      break()
    }
  }
  
  # using bf_check, pass the catch and subsidy data on to the semi offices within buffer
  # identify the values for bad
  b_contemp <- bad_office[bad_office$office_id == i,]
  # distribute by percentage of catch, for each specific year
  n_id <- semi[bf_check[[1]],]$office_id
  n_contemp <- semi_office[semi_office$office_id %in% n_id & 
                             semi_office$year %in% b_contemp$year,]
  # because we don't know if an office came after exposure, we have to summarize the production again
  df <- n_contemp %>%
    group_by(year) %>%
    summarize(contemp_male = sum(contemp_male),
              contemp_female = sum(contemp_female),
              contemp_participants = sum(contemp_participants),
              contemp_bp = sum(contemp_bp))
  n_contemp <- left_join(n_contemp,df, by = "year")
  n_contemp$pcnt_wt <- n_contemp$agg_wt_unloaded/n_contemp$tot_wt
  n_contemp$pcnt_vl <- n_contemp$agg_value_mx/n_contemp$tot_vl
  n_contemp$pcnt_sub <- n_contemp$contemporary/n_contemp$tot_contemp
  n_contemp$pcnt_sub <- ifelse(is.na(n_contemp$pcnt_sub),0,n_contemp$pcnt_sub) # sometimes contemporary is zero
  n_contemp <- left_join(n_contemp,b_contemp[c(1,6:8)], by = "year")
  n_contemp$agg_wt_unloaded <- (n_contemp$pcnt_wt*n_contemp$agg_wt_unloaded.y)+n_contemp$agg_wt_unloaded.x
  n_contemp$agg_value_mx <- (n_contemp$pcnt_vl*n_contemp$agg_value_mx.y)+n_contemp$agg_value_mx.x
  n_contemp$contemporary <- (n_contemp$pcnt_sub*n_contemp$contemporary.y)+n_contemp$contemporary.x
  contemp_bp_semi <- rbind(contemp_bp_semi,n_contemp[-c(6:17)])
  
  # print out completed so you know how the loop is going
  print(paste0("Office ID ",i," is complete"))
  
}

# check for duplications, some offices may be captured more than once
length(contemp_bp_semi[duplicated(contemp_bp_semi[c(1:5)])== T,]$office_id)
# df <- agg_wt_semi %>%
#   group_by(year,state_id,state,office_id,office) %>%
#   summarise(agg_wt_unloaded = sum(agg_wt_unloaded),
#             agg_value_mx = sum(agg_value_mx))
# agg_wt_semi <- df

df <- left_join(semi_office, contemp_bp_semi, by = c("year","state_id","state","office_id","office")) %>%
  mutate(agg_wt_unloaded = ifelse(is.na(agg_wt_unloaded.y), agg_wt_unloaded.x, agg_wt_unloaded.y)) %>% 
  mutate(agg_value_mx = ifelse(is.na(agg_value_mx.y), agg_value_mx.x, agg_value_mx.y)) %>% 
  mutate(contemporary = ifelse(is.na(contemporary.y), contemporary.x, contemporary.y)) %>% 
  select(year,state_id,state,office_id,office, agg_wt_unloaded,agg_value_mx,contemporary)

length(unique(df$office_id))

# fill in the gaps (there should be 2397 obs)
filled_bp <- NULL
for(i in as.list(unique(df$office_id))){ # i <- 410
  temp <- df[df$office_id == i,]
  
  for(y in 2006:2022){ # y <- 2014
    year <- y
    agg_wt_unloaded <- 0
    agg_value_mx <- 0
    contemporary <- 0
    if(y %not_in% temp$year){
      d <- as.data.frame(cbind(year,temp[1,c(2:5)],agg_wt_unloaded,agg_value_mx,contemporary))
      temp <- rbind(temp,d)
    }else{
      
      temp <- temp
    }
    temp <- arrange(temp,year)
  }
  filled_bp <- rbind(filled_bp,temp) 
}

# check the differences
check_original <- paste(df$year,df$office_id,df$office,sep = "_")
check_new <- paste(filled_bp$year,filled_bp$office_id,filled_bp$office,sep = "_")
diff_check <- check_new[check_new %not_in% check_original]

# officially make dataset 2
ds_2 <- filled_bp

# rearrange 
ds_2 <- arrange(ds_2, office_id,year)

# make cumulative exposure
ds_2$cumulative <- ave(ds_2$contemporary, ds_2$office_id, FUN=cumsum)

# export the dataset
write.csv(ds_2,paste0(dir$cleandata,"offices_combined_df_2.csv"), 
          row.names = F)

d <- read.csv(paste0(dir$cleandata,"offices_combined_df_2.csv"))

# visualize the distribution
options(scipen=999)
hist(d[d$cumulative > 0,]$cumulative)
hist(d[d$cumulative > 0 & d$cumulative < 50000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 10000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 500000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 500000 & d$year == 2014,]$cumulative, breaks = 15)

# create a dummy that begins when treatment starts
d$d_treated <- ifelse(d$cumulative > 0, 1,0)

# create a cumulative treatment dummy

d$c_100k <- ifelse(d$cumulative  >=  100000,
                   1,0) # if cumulative 

d$c_500k <- ifelse(d$cumulative >= 500000,
                   1,0) # cumulative reaches has reached 500k 

d$c_2m <- ifelse(d$cumulative >= 2000000,
                 1,0) # cumulative reaches 2 million

d$c_5m <- ifelse(d$cumulative >= 5000000,
                 1,0) # cumulative reaches 5 million

### visualize the change in untreated to treated with cumulative exposure ### 
delta_yrs <-  d %>% 
  filter(year > 2011) %>%
  group_by(year) %>% 
  summarise(
    treated = sum(d_treated),
    untreated = (length(unique(d$office_id))-treated),
    
    t_100k = sum(c_100k),
    t_500k = sum(c_500k),
    t_2m = sum(c_2m),
    t_5m = sum(c_5m),    
    
    ut_100k = length(unique(d$office_id))-t_2m,
    ut_500k = length(unique(d$office_id))-t_500k,
    ut_2m = length(unique(d$office_id))-t_2m,
    ut_5m = length(unique(d$office_id))-t_5m,    
  )


png(paste0(dir$fig, "change_cumulative_exposure_dataset2.png"))
#create line plot of treated
plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,145),
     xlab = "Years", ylab = "# of offices", main = "Dataset 2: Change in treated per year")

#overlay line plot of untreated
lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,145), lty = 2, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,145), lty = 4, lwd = 1)
lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,145), lty = 5, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,145), lty = 6, lwd = 3)

#add legend
legend(2020,60, legend=c('treated',
                         "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
       col=c('black',
             '#ffdb00',"#ee7b06",'#a12424','#996600'),
       lty=c(1,2,4,5,6),
       
       cex = .7)
dev.off()
# export the delta_years
write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset2.csv"), row.names = F)

# export the main
write.csv(d, paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"), row.names = F)


########################## Dataset 3: No combining, no exclusion -------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"all_join.csv"))

ds_3 <- all_join

ds_3$cumulative <- ave(ds_3$contemporary, ds_3$office_id, FUN=cumsum)

# export the dataset
write.csv(ds_3,paste0(dir$cleandata,"all_offices_no_combining_df_3.csv"), 
          row.names = F)

d <- read.csv(paste0(dir$cleandata,"all_offices_no_combining_df_3.csv"))

# visualize the distribution
options(scipen=999)
hist(d[d$cumulative > 0,]$cumulative)
hist(d[d$cumulative > 0 & d$cumulative < 50000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 10000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 500000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 500000 & d$year == 2014,]$cumulative, breaks = 15)

# create a dummy that begins when treatment starts
d$d_treated <- ifelse(d$cumulative > 0, 1,0)

# create a cumulative treatment dummy

d$c_100k <- ifelse(d$cumulative  >=  100000,
                   1,0) # if cumulative 

d$c_500k <- ifelse(d$cumulative >= 500000,
                   1,0) # cumulative reaches has reached 500k 

d$c_2m <- ifelse(d$cumulative >= 2000000,
                 1,0) # cumulative reaches 2 million

d$c_5m <- ifelse(d$cumulative >= 5000000,
                 1,0) # cumulative reaches 5 million

### visualize the change in untreated to treated with cumulative exposure ### 
delta_yrs <-  d %>% 
  filter(year > 2011) %>%
  group_by(year) %>% 
  summarise(
    treated = sum(d_treated),
    untreated = (length(unique(d$office_id))-treated),
    
    t_100k = sum(c_100k),
    t_500k = sum(c_500k),
    t_2m = sum(c_2m),
    t_5m = sum(c_5m),    
    
    ut_100k = length(unique(d$office_id))-t_2m,
    ut_500k = length(unique(d$office_id))-t_500k,
    ut_2m = length(unique(d$office_id))-t_2m,
    ut_5m = length(unique(d$office_id))-t_5m,    
  )


png(paste0(dir$fig, "change_cumulative_exposure_dataset3.png"))
#create line plot of treated
plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,160),
     xlab = "Years", ylab = "# of offices", main = "Dataset 3: Change in treated per year")

#overlay line plot of untreated
lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,160), lty = 2, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,160), lty = 4, lwd = 1)
lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,160), lty = 5, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,160), lty = 6, lwd = 3)

#add legend
legend(2020,60, legend=c('treated',
                         "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
       col=c('black',
             '#ffdb00',"#ee7b06",'#a12424','#996600'),
       lty=c(1,2,4,5,6),
       
       cex = .7)
dev.off()
# export the delta_years
write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset3.csv"), row.names = F)

# export the main
write.csv(d, paste0(dir$cleandata, "bienpesca_binary_exposure_dataset3.csv"), row.names = F)


# END OF CODE

################################################################################
###############################################################################
############### SHRIMP ----------------------------------------------------
##### LOAD DATA ----------------------------------------------------------
# using the driving distance linkage with agg_wt
agg_wt <- read.csv(paste0(dir$cleandata,"production_agg_wt_shrimp.csv"))

contemp_bp <- NULL
for(i in 2014:2022){
  
  contemp_office_yr <- read.csv(paste0(dir$cleandata,"contemp_office_",i,".csv"))
  
  contemp_bp <- rbind(contemp_bp,contemp_office_yr)
}


######### make adjustments to data --------------------------------------------------
# prior to 2006, data was only allocated to Pacific. We will look from 2006 onward
agg_wt_06 <- agg_wt[agg_wt$year > 2005,]
length(unique(agg_wt_06$office_id)) # 126

# put date in front,organize by office_id and year
contemp_bp <- contemp_bp %>% select(year, everything()) %>%
  arrange(office_id,year)

# combine the production and exposure and fill in the gaps at the end!
length(unique(contemp_bp$office_id)) # 155 offices with BP exposure
length(unique(agg_wt_06[agg_wt_06$year > 2013,]$office_id))# 109 offices with shrimp production

# we will exclude any office that did exist during the time of treatement
pd_treatable <- agg_wt_06[agg_wt_06$office_id %in% agg_wt_06[agg_wt_06$year > 2013,]$office_id,]
length(unique(pd_treatable$office_id)) # confirmed 109

# join the production and bp, left_join having production as the main (it has more observations)
all_join <- left_join(pd_treatable, contemp_bp[-c(6,7)], by = c("year","state_id","state","office_id","office")) %>%
  mutate(contemporary = ifelse(is.na(contemporary),0,contemporary))
length(unique(all_join$office_id))  # 109

# make columns for each year and check what offices have that year
office_joint_freq <- NULL
for(o in as.list(unique(all_join$office_id))){ # o <- 101
  
  temp <- all_join[all_join$office_id == o,c(1:5)]
  
  yr_dum <- temp[1,c(2:5)]
  for(i in 2006:2022){ # i <- 2006
    df <- temp[1,c(2:5)]
    if(i %in% temp$year){
      dum <- 1
    }else{
      dum <- 0
    }
    df$dum <- dum
    colnames(df)[5] <- paste("dum_",i, sep = "_")
    yr_dum <- cbind(yr_dum,df[5])
    
  }
  office_joint_freq <- rbind(office_joint_freq,yr_dum)
}
# sum across the rows
office_joint_freq$total_yrs <- rowSums(office_joint_freq[ , c(5:21)], na.rm=TRUE)

# export these
write.csv(all_join, paste0(dir$cleandata,"all_join_shrimp.csv"), row.names = F)
write.csv(office_joint_freq, paste0(dir$cleandata,"office_joint_freq_shrimp.csv"), row.names = F)

######### here we make 3 seperate datasets. Description of each below -------------------------------------------------------
# dataset 1) no combining offices, only the offices that existed throughout the entire period (17 yrs)
# dataset 2) offices are combined if they have less than 10 years of production, 10 and more get filled with zeros 
# dataset 3) keep all offices but no combining

######### dataset 1: keep only offices with strictly 17 years ------------------------------------------------------------------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"all_join_shrimp.csv"))
office_joint_freq <- read.csv(paste0(dir$cleandata,"office_joint_freq_shrimp.csv"))

ds_1 <- all_join[all_join$office_id %in%
                     office_joint_freq[office_joint_freq$total_yrs == 17,]$office_id,]
length(unique(ds_1$office_id)) #101 offices
# should be 2244 obs (132*17)
# add the cumulative exposure
ds_1$cumulative <- ave(ds_1$contemporary, ds_1$office_id, FUN=cumsum)

# export the dataset
write.csv(ds_1,paste0(dir$cleandata,"offices_17_years_only_df_1_shrimp.csv"), 
          row.names = F)

d <- read.csv(paste0(dir$cleandata,"offices_17_years_only_df_1_shrimp.csv"))

# visualize the distribution
options(scipen=999)
hist(d[d$cumulative > 0,]$cumulative, breaks = 30)
hist(d[d$cumulative > 0 & d$cumulative < 50000000,]$cumulative, breaks = 25)
hist(d[d$cumulative > 0 & d$cumulative < 10000000,]$cumulative, breaks = 25)
hist(d[d$cumulative > 0 & d$cumulative < 500000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 500000 & d$year == 2014,]$cumulative, breaks = 15)

# create a dummy that begins when treatment starts
d$d_treated <- ifelse(d$cumulative > 0, 1,0)

# create a cumulative treatment dummy

d$c_100k <- ifelse(d$cumulative  >=  100000,
                   1,0) # if cumulative 

d$c_500k <- ifelse(d$cumulative >= 500000,
                   1,0) # cumulative reaches has reached 500k 

d$c_2m <- ifelse(d$cumulative >= 2000000,
                 1,0) # cumulative reaches 2 million

d$c_5m <- ifelse(d$cumulative >= 5000000,
                 1,0) # cumulative reaches 5 million

### visualize the change in untreated to treated with cumulative exposure ### 
delta_yrs <-  d %>% 
  filter(year > 2011) %>%
  group_by(year) %>% 
  summarise(
    treated = sum(d_treated),
    untreated = (length(unique(d$office_id))-treated),
    
    t_100k = sum(c_100k),
    t_500k = sum(c_500k),
    t_2m = sum(c_2m),
    t_5m = sum(c_5m),    
    
    ut_100k = length(unique(d$office_id))-t_2m,
    ut_500k = length(unique(d$office_id))-t_500k,
    ut_2m = length(unique(d$office_id))-t_2m,
    ut_5m = length(unique(d$office_id))-t_5m,    
  )


png(paste0(dir$fig, "change_cumulative_exposure_dataset1_shrimp.png"))
#create line plot of treated
plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,140),
     xlab = "Years", ylab = "# of offices", main = "Dataset 1: Change in treated per year")

#overlay line plot of untreated
lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,140), lty = 2, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,140), lty = 4, lwd = 1)
lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,140), lty = 5, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,140), lty = 6, lwd = 3)

#add legend
legend(2020,60, legend=c('treated',
                         "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
       col=c('black',
             '#ffdb00',"#ee7b06",'#a12424','#996600'),
       lty=c(1,2,4,5,6),
       
       cex = .7)
dev.off()
# export the delta_years
write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset1_shrimp.csv"), row.names = F)

# export the main
write.csv(d, paste0(dir$cleandata, "shrimp_binary_exposure_dataset1.csv"), row.names = F)

######### dataset 2: Combining offices ---------------------------------------------------------------------------------------------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"all_join_shrimp.csv"))
office_joint_freq <- read.csv(paste0(dir$cleandata,"office_joint_freq_shrimp.csv"))

# now the offices that existed for at least 10 years (more than half of the time)
semi_office <- all_join[all_join$office_id %in%
                            office_joint_freq[office_joint_freq$total_yrs > 9,]$office_id,]
length(unique(semi_office$office_id))

# all the offices that existed for half the time or less
bad_office <- all_join[all_join$office_id %in%
                           office_joint_freq[office_joint_freq$total_yrs < 10,]$office_id,]
length(unique(bad_office$office_id)) #16 offices had less than 10 years

# we can combine the offices by distance
office_sf <- st_read(paste0(dir$shp, "office_points_nov_21_2024.shp"))

# fill datasets for loop
semi <- office_sf[office_sf$office_id %in% semi_office$office_id,]
bf_check <- list(NULL)
agg_wt_semi <- NULL
contemp_bp_semi <- NULL
for(i in as.list(unique(bad_office$office_id))){ # i <- 308
  
  bad <- office_sf[office_sf$office_id == i,]
  
  # make a buffer for each of these big enough to cover at most 4 offices
  for(s in as.list(seq(10000,400000, by = 10000))){ # s <- 100000 
    
    bf <- st_buffer(bad,dist = s)
    bf_check <- st_contains(bf,semi)
    
    # once the buffer captures at least 1 office and no more than 4, stop loop
    if(length(bf_check[[1]] > 1 & length(bf_check[[1]]) <= 4)){
      break()
    }
  }
  
  # using bf_check, pass the catch and subsidy data on to the semi offices within buffer
  # identify the values for bad
  b_contemp <- bad_office[bad_office$office_id == i,]
  # distribute by percentage of catch, for each specific year
  n_id <- semi[bf_check[[1]],]$office_id
  n_contemp <- semi_office[semi_office$office_id %in% n_id & 
                             semi_office$year %in% b_contemp$year,]
  # because we don't know if an office came after exposure, we have to summarize the production again
  df <- n_contemp %>%
    group_by(year) %>%
    summarize(tot_wt = sum(agg_wt_unloaded_shrimp),
              tot_vl = sum(agg_value_mx),
              tot_contemp = sum(contemporary))
  n_contemp <- left_join(n_contemp,df, by = "year")
  n_contemp$pcnt_wt <- n_contemp$agg_wt_unloaded_shrimp/n_contemp$tot_wt
  n_contemp$pcnt_vl <- n_contemp$agg_value_mx/n_contemp$tot_vl
  n_contemp$pcnt_sub <- n_contemp$contemporary/n_contemp$tot_contemp
  n_contemp$pcnt_sub <- ifelse(is.na(n_contemp$pcnt_sub),0,n_contemp$pcnt_sub) # sometimes contemporary is zero
  n_contemp <- left_join(n_contemp,b_contemp[c(1,6:8)], by = "year")
  n_contemp$agg_wt_unloaded_shrimp <- (n_contemp$pcnt_wt*n_contemp$agg_wt_unloaded_shrimp.y)+n_contemp$agg_wt_unloaded_shrimp.x
  n_contemp$agg_value_mx <- (n_contemp$pcnt_vl*n_contemp$agg_value_mx.y)+n_contemp$agg_value_mx.x
  n_contemp$contemporary <- (n_contemp$pcnt_sub*n_contemp$contemporary.y)+n_contemp$contemporary.x
  contemp_bp_semi <- rbind(contemp_bp_semi,n_contemp[-c(6:17)])
  
  # print out completed so you know how the loop is going
  print(paste0("Office ID ",i," is complete"))
  
}

# check for duplications, some offices may be captured more than once
length(contemp_bp_semi[duplicated(contemp_bp_semi[c(1:5)])== T,]$office_id)
# df <- agg_wt_semi %>%
#   group_by(year,state_id,state,office_id,office) %>%
#   summarise(agg_wt_unloaded_shrimp = sum(agg_wt_unloaded_shrimp),
#             agg_value_mx = sum(agg_value_mx))
# agg_wt_semi <- df

df <- left_join(semi_office, contemp_bp_semi, by = c("year","state_id","state","office_id","office")) %>%
  mutate(agg_wt_unloaded_shrimp = ifelse(is.na(agg_wt_unloaded_shrimp.y), agg_wt_unloaded_shrimp.x, agg_wt_unloaded_shrimp.y)) %>% 
  mutate(agg_value_mx = ifelse(is.na(agg_value_mx.y), agg_value_mx.x, agg_value_mx.y)) %>% 
  mutate(contemporary = ifelse(is.na(contemporary.y), contemporary.x, contemporary.y)) %>% 
  select(year,state_id,state,office_id,office, agg_wt_unloaded_shrimp,agg_value_mx,contemporary)

length(unique(df$office_id))

# fill in the gaps (there should be 2397 obs)
filled_bp <- NULL
for(i in as.list(unique(df$office_id))){ # i <- 401
  temp <- df[df$office_id == i,]
  
  for(y in 2006:2022){ # y <- 2014
    year <- y
    agg_wt_unloaded_shrimp <- 0
    agg_value_mx <- 0
    contemporary <- 0
    if(y %not_in% temp$year){
      d <- as.data.frame(cbind(year,temp[1,c(2:5)],agg_wt_unloaded_shrimp,agg_value_mx,contemporary))
      temp <- rbind(temp,d)
    }else{
      
      temp <- temp
    }
    temp <- arrange(temp,year)
  }
  filled_bp <- rbind(filled_bp,temp) 
}

# check the differences
check_original <- paste(df$year,df$office_id,df$office,sep = "_")
check_new <- paste(filled_bp$year,filled_bp$office_id,filled_bp$office,sep = "_")
diff_check <- check_new[check_new %not_in% check_original]

# officially make dataset 2
ds_2 <- filled_bp

# rearrange 
ds_2 <- arrange(ds_2, office_id,year)

# make cumulative exposure
ds_2$cumulative <- ave(ds_2$contemporary, ds_2$office_id, FUN=cumsum)

# export the dataset
write.csv(ds_2,paste0(dir$cleandata,"offices_combined_df_2_shrimp.csv"), 
          row.names = F)

d <- read.csv(paste0(dir$cleandata,"offices_combined_df_2_shrimp.csv"))

# visualize the distribution
options(scipen=999)
hist(d[d$cumulative > 0,]$cumulative)
hist(d[d$cumulative > 0 & d$cumulative < 50000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 10000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 100000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 500000 & d$year == 2014,]$cumulative, breaks = 15)

# create a dummy that begins when treatment starts
d$d_treated <- ifelse(d$cumulative > 0, 1,0)

# create a cumulative treatment dummy

d$c_1m <- ifelse(d$cumulative >= 1000000,
                 1,0) # cumulative reaches has reached 1m 

d$c_100m <- ifelse(d$cumulative >= 50000000,
                   1,0) # cumulative reaches 50 million

d$c_100m <- ifelse(d$cumulative >= 100000000,
                   1,0) # cumulative reaches 100 million

### visualize the change in untreated to treated with cumulative exposure ### 
delta_yrs <-  d %>% 
  filter(year > 2011) %>%
  group_by(year) %>% 
  summarise(
    treated = sum(d_treated),
    untreated = (length(unique(d$office_id))-treated),
    
    t_1m = sum(c_1m),
    t_2m = sum(c_2m),
    t_5m = sum(c_5m),    
    
    ut_1m = length(unique(d$office_id))-t_1m,
    ut_2m = length(unique(d$office_id))-t_2m,
    ut_5m = length(unique(d$office_id))-t_5m,    
  )


png(paste0(dir$fig, "change_cumulative_exposure_dataset2_shrimp.png"))
#create line plot of treated
plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,145),
     xlab = "Years", ylab = "# of offices", main = "Shrimp Dataset 2: Change in treated per year")

#overlay line plot of untreated
lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,145), lty = 2, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,145), lty = 4, lwd = 1)
lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,145), lty = 5, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,145), lty = 6, lwd = 3)

#add legend
legend(2020,60, legend=c('treated',
                         "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
       col=c('black',
             '#ffdb00',"#ee7b06",'#a12424','#996600'),
       lty=c(1,2,4,5,6),
       
       cex = .7)
dev.off()
# export the delta_years
write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset2_shrimp.csv"), row.names = F)

# export the main
write.csv(d, paste0(dir$cleandata, "binary_exposure_dataset2_shrimp.csv"), row.names = F)


########################## Dataset 3: No combining, no exclusion -------------------------------------
all_join <- read.csv(paste0(dir$cleandata,"all_join_shrimp.csv"))

ds_3 <- all_join

ds_3$cumulative <- ave(ds_3$contemporary, ds_3$office_id, FUN=cumsum)

# export the dataset
write.csv(ds_3,paste0(dir$cleandata,"all_offices_no_combining_df_3_shrimp.csv"), 
          row.names = F)

d <- read.csv(paste0(dir$cleandata,"all_offices_no_combining_df_3_shrimp.csv"))

# visualize the distribution
options(scipen=999)
hist(d[d$cumulative > 0,]$cumulative)
hist(d[d$cumulative > 0 & d$cumulative < 50000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 10000000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 0 & d$cumulative < 500000,]$cumulative, breaks = 15)
hist(d[d$cumulative > 500000 & d$year == 2014,]$cumulative, breaks = 15)

# create a dummy that begins when treatment starts
d$d_treated <- ifelse(d$cumulative > 0, 1,0)

# create a cumulative treatment dummy

d$c_100k <- ifelse(d$cumulative  >=  100000,
                   1,0) # if cumulative 

d$c_500k <- ifelse(d$cumulative >= 500000,
                   1,0) # cumulative reaches has reached 500k 

d$c_2m <- ifelse(d$cumulative >= 2000000,
                 1,0) # cumulative reaches 2 million

d$c_5m <- ifelse(d$cumulative >= 5000000,
                 1,0) # cumulative reaches 5 million

### visualize the change in untreated to treated with cumulative exposure ### 
delta_yrs <-  d %>% 
  filter(year > 2011) %>%
  group_by(year) %>% 
  summarise(
    treated = sum(d_treated),
    untreated = (length(unique(d$office_id))-treated),
    
    t_100k = sum(c_100k),
    t_500k = sum(c_500k),
    t_2m = sum(c_2m),
    t_5m = sum(c_5m),    
    
    ut_100k = length(unique(d$office_id))-t_2m,
    ut_500k = length(unique(d$office_id))-t_500k,
    ut_2m = length(unique(d$office_id))-t_2m,
    ut_5m = length(unique(d$office_id))-t_5m,    
  )


png(paste0(dir$fig, "change_cumulative_exposure_dataset3_shrimp.png"))
#create line plot of treated
plot(delta_yrs$year,delta_yrs$treated, type='l', col='black', ylim = c(0,160),
     xlab = "Years", ylab = "# of offices", main = "Dataset 3: Change in treated per year")

#overlay line plot of untreated
lines(delta_yrs$year,delta_yrs$t_100k, col='#ffdb00', ylim = c(0,160), lty = 2, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_500k, col = "#ee7b06", ylim = c(0,160), lty = 4, lwd = 1)
lines(delta_yrs$year,delta_yrs$t_2m, col = '#a12424', ylim = c(0,160), lty = 5, lwd = 2)
lines(delta_yrs$year,delta_yrs$t_5m, col = '#996600', ylim = c(0,160), lty = 6, lwd = 3)

#add legend
legend(2020,60, legend=c('treated',
                         "trtd 100k","trtd 500k",'trtd 2m','trtd 5m'),
       col=c('black',
             '#ffdb00',"#ee7b06",'#a12424','#996600'),
       lty=c(1,2,4,5,6),
       
       cex = .7)
dev.off()
# export the delta_years
write.csv(delta_yrs, paste0(dir$cleandata, "change_in_years_cumulative_dataset3_shrimp.csv"), row.names = F)

# export the main
write.csv(d, paste0(dir$cleandata, "shrimp_binary_exposure_dataset3_shrimp.csv"), row.names = F)


# END OF CODE

