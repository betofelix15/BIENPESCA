# Title: 13: Final estimation
# Purpose: to test estimations and any other codes
# Author: Jesus Felix
# Start date: 25 JUN 2025
################################################################################
#notes:

#### R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal",
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata","did2s","fixest","stargazer","broom","openxlsx","modelsummary")

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


# 1. load/quick clean all data --------------------------------------------------------------------
# load the monthly variation/annual treatment/all offices present (update 13 nov 2025, now with gas)
df <- read.csv(paste0(dir$cleandata,"monthly_prod_annual_subsidies_9_dec_2025.csv"))


# having value doesn't make sense, it should be revenue. Now it will be called rev
names <- colnames(df)
new_names <- gsub("agg_value","agg_rev", names) # replace columns with "agg_value" to "agg_rev"
print(new_names)

colnames(df) <- new_names


##############~~
#covariate data
##############~~~~~~~

# sst celsius (100 km buffer)
sst <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024.csv"))
sst$avg_sst <- sst$avg_sst*0.01 # multiply by band scale
sst$sd_sst <- sst$sd_sst*0.01 # multiply by band scale
# remove unnecessary columns
sst <- sst %>% select(-.geo,-system.index)


# sst celsius (200km buffer)
sst_200k <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024.csv"))
sst_200k$avg_sst_200k <- sst$avg_sst*0.01 # multiply by band scale
sst_200k$sd_sst_200k <- sst$sd_sst*0.01 # multiply by band scale
# remove unnecessary columns
sst_200k <- sst_200k %>% select(-.geo,-system.index,-avg_sst,-sd_sst)

# coastal precipitation land only (meters)
prec <- read.csv(paste0(dir$cleandata,"coastal_precip_2006_2024_all.csv"))

# clorophyll concentration (mg per cubed meter)
clp <- read.csv(paste0(dir$cleandata,"chlorophyll_concentration_mg_m3.csv"))
clp$year <- year(clp$date)
clp$month <- month(clp$date)

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



# 2. Combine/restrucure data -----------------------------------------------------------
# combine with covariates
df_final <- left_join(df,sst, by = c("office_id","year","month"))
df_final <- left_join(df_final,sst_200k, by = c("office_id","year","month"))
df_final <- left_join(df_final,clp, by = c("office_id","year","month"))
df_final <- left_join(df_final,prec, by = c("office_id","year","month"))
df_final <- left_join(df_final,ws, by = c("office_id","year","month"))


# ## add a period fixed effect (monthly)

# Create a sequence of monthly dates from Jan 2006 to Dec 2024
dates <- seq.Date(from = as.Date("2006-01-01"), to = as.Date("2024-12-01"), by = "month")

# Create the time period variable: Jan 2006 = 1, Feb 2006 = 2, etc.
time_period <- seq_along(dates)

# Combine into a data frame for clarity
time_df <- data.frame(
  date = dates,
  time_period = time_period
)
time_df$year <- year(time_df$date)
time_df$month <- month(time_df$date)

# combine
df_final <- left_join(df_final, time_df[c("year","month","time_period")], by = c("year","month"))

# ## # add the one year lags

df_final <- df_final %>%
  arrange(office, year, month) %>%
  group_by(office) %>%
  mutate(
    lag_sst = lag(avg_sst, 12),
    lag_sst_200k = lag(avg_sst_200k,12),
    lag_wind = lag(avg_wind_speed, 12),
    lag_chl = lag(chlorophyll_conc, 12),
    lag_prec = lag(avg_prec, 12),
    lag_catch = lag(agg_unld_wt_kg_imp,12),
    lag_catch_non_ac = lag(agg_unld_wt_kg_imp_non_ac, 12),
    lag_catch_shrimp = lag(agg_unld_wt_kg_imp_shrimp,12),
    lag_catch_lisa = lag(agg_unld_wt_kg_imp_lisa,12),
    lag_catch_placebo = lag(agg_unld_wt_kg_imp_placebo,12)
  )


#export this for future use (updated 10 dec 2025)
write.csv(df_final,
          paste0(dir$cleandata,"final_estimation_ready_dataset.csv"),
          row.names = F)


# 3. DID2S BP estimation (Extensive, shrimp, lisa, agg) ----------------------------------------------------------------

# to skip the cleaning (updated 14 nov 2025, includes 200km buffer sst):
df_final <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset.csv"))



# Identify the year each office was treated
yr_1st_trtd_bp <- df_final %>% 
  filter(contemp_bp == cumulative_bp & contemp_bp != 0) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup()
yr_1st_trtd_bp$yr_1st <- yr_1st_trtd_bp$year

# identify office never treated
df_final %>% filter(year == 2024 & cumulative_bp == 0)

# combine this column with main data
df_extensive_bp <- left_join(df_final,yr_1st_trtd_bp[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_extensive_bp$yr_1st <- ifelse(is.na(df_extensive_bp$yr_1st),0,df_extensive_bp$yr_1st)

# make the relavive year
df_extensive_bp$rel_year <- df_extensive_bp$year - df_extensive_bp$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_extensive_bp$rel_year <- ifelse(df_extensive_bp$yr_1st == 0,Inf,df_extensive_bp$rel_year)

# make treatment boolean
df_extensive_bp$treat_b <- ifelse(df_extensive_bp$bp_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_extensive_bp$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_extensive_bp <- left_join(df_extensive_bp, unit_df, by = "office_id")


colnames(df_extensive_bp)

#export this for future use
# write.csv(df_extensive_bp,
#           paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"),
#           row.names = F)

# to skip the extensive prep
# df_extensive_bp <- read.csv(paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"))

# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#########  SHRIMP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
# all fixed effects for shrimp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
first_stage_bp <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                     lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc+avg_prec +lag_catch |
                      
                     unit + year +time_period,  # + unit[year] (unit specific trends)
                     cluster = "office_id",
                     data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp)

# no monthly fixed effects
first_stage_bp_nm <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                          lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                          
                          unit + year ,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nm)


# no monthly or year
first_stage_bp_nm_ny <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                             lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                             
                             unit ,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nm_ny)


# no FE
first_stage_bp_nfe <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                                lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch 
                                
                                 ,  # + unit[year] (unit specific trends)
                              cluster = "office_id",
                              data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nfe)


# year & month
first_stage_bp_ym <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                              lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                           year + time_period,
                            cluster = "office_id",
                            data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_ym)



# second stage shrimp unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp_shrimp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ avg_wind_speed+avg_sst+chlorophyll_conc +avg_prec+lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))

coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Shrimp Catch from PP/BP",
  xlab = "Relative Year" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)







# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bp_static_shrimp <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp_shrimp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ avg_wind_speed+avg_sst+chlorophyll_conc +avg_prec+lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_shrimp)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp_dynamic <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)




###### ---
###### LISA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######_


first_stage_bp_lisa <- feols(agg_unld_wt_kg_imp_lisa ~ 
                               lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch  |
                          
                          unit ,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_lisa)



# second stage shrimp unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp_lisa",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))


coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Lisa Catch from PP/BP",
  xlab = "Relative Year (2014)" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)

bp_static_lisa <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp_lisa",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_lisa)


##### aggregate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


first_stage_agg <- feols(agg_unld_wt_kg_imp ~ 
                          lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                          
                          unit,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_agg)


# second stage aggregate unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))

coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Aggregate Catch from PP/BP",
  xlab = "Relative Year (2014)" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)

bp_static_agg <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_agg)






# compare
first_stage_compare <- etable(first_stage_bp_nm_ny, first_stage_bp_lisa, first_stage_agg,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 3,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

second_stage_compare <- etable(bp_static_shrimp, bp_static_lisa, bp_static_agg,
                               fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                               digits = 3,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)


# convert tables to dataframes and export
#modelsummary(first_stage_compare,output = paste0(dir$cleandata,"table_1_first_stage.xlsx"))



##### revenue extensive -------------------------------------------------------------------------
# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#########  SHRIMP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
# all fixed effects for shrimp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
first_stage_bp <- feols(avg_price_shrimp_mx_deflated_imp ~ 
                          lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                          
                          unit + year +time_period,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp)

# no monthly fixed effects
first_stage_bp_nm <- feols(avg_price_shrimp_mx_deflated_imp ~ 
                             lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                             
                             unit + year ,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nm)


# no monthly or year
first_stage_bp_nm_ny <- feols(avg_price_shrimp_mx_deflated_imp ~ 
                                lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                                
                                unit ,  # + unit[year] (unit specific trends)
                              cluster = "office_id",
                              data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nm_ny)


# no FE
first_stage_bp_nfe <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                              lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch 
                            
                            ,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_nfe)


# year & month
first_stage_bp_ym <- feols(agg_unld_wt_kg_imp_shrimp ~ 
                             lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                             year + time_period,
                           cluster = "office_id",
                           data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_ym)



# second stage shrimp unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_rev_shrimp_deflated_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))

coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Shrimp Catch from PP/BP",
  xlab = "Relative Year (2014)" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)







# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bp_static_shrimp <- did2s(
  df_extensive_bp,
  yname = "agg_rev_shrimp_deflated_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_shrimp)




###### ---
###### LISA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######_


first_stage_bp_lisa <- feols(agg_rev_lisa_deflated_imp ~ 
                               lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch  |
                               
                               unit ,  # + unit[year] (unit specific trends)
                             cluster = "office_id",
                             data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_bp_lisa)



# second stage shrimp unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_rev_lisa_deflated_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))


coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Lisa Catch from PP/BP",
  xlab = "Relative Year (2014)" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)

bp_static_lisa <- did2s(
  df_extensive_bp,
  yname = "agg_rev_lisa_deflated_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_lisa)


##### aggregate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


first_stage_agg <- feols(agg_unld_wt_kg_imp ~ 
                           lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                           
                           unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive_bp[df_extensive_bp$bp_treated == 0,])
etable(first_stage_agg)


# second stage aggregate unit fe

bp_dynamic <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg_imp",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

names(bp_dynamic$coefficients) <- gsub("rel_year::","",names(bp_dynamic$coefficients))

coefplot(
  bp_dynamic,
  ci.col = NA,
  pt.cex = 0,
  pt.join = T,
  pt.join.par =  list(lwd = 2, col = "darkred"),
  ci.fill = TRUE,
  ci.fill.par = list(col = "lightblue", alpha = 0.5),
  main = "Effect on Aggregate Catch from PP/BP",
  xlab = "Relative Year (2014)" ,
  #group = list(group_name = "rel_year::"),
  group.par = list(text.cex = 0)
)

bp_static_agg <- did2s(
  df_extensive_bp,
  yname = "agg_unld_wt_kg",
  treatment = "bp_treated",
  cluster_var = "office_id",
  first_stage = ~ lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |unit ,
  second_stage = "bp_treated",
  verbose = T
)


etable(bp_static_agg)






# compare
first_stage_compare <- etable(first_stage_bp_nm_ny, first_stage_bp_lisa, first_stage_agg,
                              fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                              digits = 3,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

second_stage_compare <- etable(bp_static_shrimp, bp_static_lisa, bp_static_agg,
                               fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                               digits = 3,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)


# convert tables to dataframes and export
#modelsummary(first_stage_compare,output = paste0(dir$cleandata,"table_1_first_stage.xlsx"))






##### Comparison of fixed effects ------------------------------------------------------------------------

# compare some first stage models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                       avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                       
                       0,  # + unit[year] (unit specific trends)
                     cluster = "office_id",
                     data = df_extensive[df_extensive$d_treated == 0,])

no_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                         
                         0,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

no_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit fE
unit_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year fE (uy)
uy_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                           avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                           
                           unit + year,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                              avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit + year,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                              lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit + year,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year + time_perio fE (uyt)
uyt_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit + year + time_period,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year + time_period,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year + time_period,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])


# compare
no_fe_tex <- etable(no_fe_no_lags, no_fe_catch_lags, no_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

unit_fe_tex <- etable(unit_fe_no_lags, unit_fe_catch_lags, unit_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uy_fe_tex <- etable(uy_fe_no_lags, uy_fe_catch_lags, uy_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uyt_fe_tex <- etable(uyt_fe_no_lags, uyt_fe_catch_lags, uyt_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)
                          


# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bp_static <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = "d_treated",
  verbose = T
)


etable(bp_static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp_dynamic <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)

# check first stage, check covariate significance (steven example)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

first_stage_4 <- feols(agg_unld_wt_kg_imp_shrimp ~ avg_sst+avg_prec+avg_wind_speed+chlorophyll_conc|unit + year, # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$treat_b == F,])
first_stage_4


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# now on revenue
colnames(df_extensive)

rev_att <- did2s(
  df_extensive,
  yname = "agg_value_mx_deflated_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = "d_treated",
  verbose = T
)


etable(rev_att)


# now for the the dynamic

rev_dynamic <- did2s(
  df_extensive,
  yname = "agg_value_mx_deflated_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

# plot it
coefplot(rev_dynamic,
         main = "Effect on the aggregate revenue (CPI deflated)"
         )




### DID2S estimation cumulative -----------------------------------------------------

### dynamic (annual) treatment effect

bp_cumulative <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~(cumulative_deflated),
  verbose = T
)

etable(bp_cumulative)
##
### DID2s estimation intensive (treated at median) ---------------------------------------------------------
# to verify the observations per year match use delta years
delta_yrs <- read.csv(paste0(dir$cleandata,"change_in_years_cumulative_dataset1.csv"))


# Identify the year each office reached the median cumulative exposure
yr_1st_trtd <- df_final %>% 
  filter(c_median == 1) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup() %>%
  rename(yr_1st = year)

# identify office never treated
df_final %>% filter(year == 2024 & c_median == 0)

# combine this column with main data
df_int_med <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_int_med$yr_1st <- ifelse(is.na(df_int_med$yr_1st),0,df_int_med$yr_1st)

# make the relavive year
df_int_med$rel_year <- df_int_med$year - df_int_med$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_int_med$rel_year <- ifelse(df_int_med$yr_1st == 0,Inf,df_int_med$rel_year)

# make treatment boolean
df_int_med$treat_b <- ifelse(df_int_med$c_median == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_int_med$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_int_med <- left_join(df_int_med, unit_df, by = "office_id")



# average treatment effect
bp_static <- did2s(
  df_int_med,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = "c_median",
  verbose = T
)


etable(bp_static)


# dynamic (annual) treatment effect

bp_dynamic <- did2s(
  df_int_med,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)



### DID2s estimation intensive (treated at average) ---------------------------------------------------------
# to verify the observations per year match use delta years
delta_yrs <- read.csv(paste0(dir$cleandata,"change_in_years_cumulative_dataset1.csv"))


# Identify the year each office reached the median cumulative exposure
yr_1st_trtd <- df_final %>% 
  filter(c_avg == 1) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup() %>%
  rename(yr_1st = year)

# identify office never treated
df_final %>% filter(year == 2024 & c_avg == 0)

# combine this column with main data
df_int_avg <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_int_avg$yr_1st <- ifelse(is.na(df_int_avg$yr_1st),0,df_int_avg$yr_1st)

# make the relavive year
df_int_avg$rel_year <- df_int_avg$year - df_int_avg$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_int_avg$rel_year <- ifelse(df_int_avg$yr_1st == 0,Inf,df_int_avg$rel_year)

# make treatment boolean
df_int_avg$treat_b <- ifelse(df_int_avg$c_avg == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_int_avg$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_int_avg <- left_join(df_int_avg, unit_df, by = "office_id")



# average treatment effect
bp_static <- did2s(
  df_int_avg,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = "c_median",
  verbose = T
)


etable(bp_static)


# dynamic (annual) treatment effect

bp_dynamic <- did2s(
  df_int_avg,
  yname = "agg_unld_wt_kg_imp",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_east_wind_speed+avg_north_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)








# 4. DID2S estimation with the gas in value as treatment -------------------------------------------
# Identify the year each office was treated
yr_1st_trtd <- df_final %>% 
  filter(gas_mxn_value == cumulative_gas_mxn & gas_mxn_value != 0) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup()
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
df_final %>% filter(year == 2024 & cumulative_gas_mxn == 0)

# combine this column with main data
df_extensive_gas <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_extensive_gas$yr_1st <- ifelse(is.na(df_extensive_gas$yr_1st),0,df_extensive_gas$yr_1st)

# make the relavive year
df_extensive_gas$rel_year <- df_extensive_gas$year - df_extensive_gas$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_extensive_gas$rel_year <- ifelse(df_extensive_gas$yr_1st == 0,Inf,df_extensive_gas$rel_year)

# make treatment boolean
df_extensive_gas$treat_b <- ifelse(df_extensive_gas$gas_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_extensive_gas$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_extensive_gas <- left_join(df_extensive_gas, unit_df, by = "office_id")


colnames(df_extensive_gas)

#export this for future use
# write.csv(df_extensive,
#           paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"),
#           row.names = F)

# to skip the extensive prep
#df_extensive <- read.csv(paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"))

# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using agg_unld with imputations and no aquaculture
first_stage <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                       avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |
                       
                       unit + year ,  # + unit[year] (unit specific trends)
                     cluster = "office_id",
                     data = df_extensive_gas[df_extensive_gas$gas_treated == 0,])
etable(first_stage)


# compare some first stage models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_fe_no_lags <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                         lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                         
                         0,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$gas_liters_treated == 0,])

no_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

no_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit fE
unit_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                           avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                           
                           unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                              avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                              lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year fE (uy)
uy_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit + year,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year + time_perio fE (uyt)
uyt_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                          avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                          
                          unit + year + time_period,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                             avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                             lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])


# compare
no_fe_tex <- etable(no_fe_no_lags, no_fe_catch_lags, no_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

unit_fe_tex <- etable(unit_fe_no_lags, unit_fe_catch_lags, unit_fe_c_sst_lags,
                      fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                      digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uy_fe_tex <- etable(uy_fe_no_lags, uy_fe_catch_lags, uy_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uyt_fe_tex <- etable(uyt_fe_no_lags, uyt_fe_catch_lags, uyt_fe_c_sst_lags,
                     fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                     digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static <- did2s(
  df_extensive_gas,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |unit + year ,
  second_stage = "gas_treated",
  verbose = T
)


etable(static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynamic <- did2s(
  df_extensive_gas,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |unit + year ,
  second_stage = ~ i(rel_year),
  verbose = T
)

# make the coefficients a table
coef_df <- tidy(dynamic, conf.int = TRUE)
coef_df <- coef_df[1:24,]  # remove the last observation (inf)
coef_df$`Relative Year` <- -10:13 # make the relative year


# make an event study graph
ggplot(coef_df, aes(x = `Relative Year`, y = estimate)) +
  # RIBBON FOR CONFIDENCE INTERVAL
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  
  # LINE FOR COEFFICIENT ESTIMATES
  geom_line(size = 1) +
  
  # POINTS (optional but helpful)
  geom_point(size = 2) +
  
  # ZERO LINE
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = "Event Time (Years)",
    y = "Coefficient Estimate",
    title = "Event Study Coefficients with 95% Confidence Intervals"
  ) +
  
  theme_minimal(base_size = 14)


# 5. DID2S estimation with the other subsidies as treatment -------------------------------------------
# Identify the year each office was treated
yr_1st_trtd <- df_final %>% 
  filter(other_sub_mxn_value == cumulative_other_sub_mxn & other_sub_mxn_value != 0) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup()
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
df_final %>% filter(year == 2024 & cumulative_other_sub_mxn == 0)

# combine this column with main data
df_extensive <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_extensive$yr_1st <- ifelse(is.na(df_extensive$yr_1st),0,df_extensive$yr_1st)

# make the relavive year
df_extensive$rel_year <- df_extensive$year - df_extensive$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_extensive$rel_year <- ifelse(df_extensive$yr_1st == 0,Inf,df_extensive$rel_year)

# make treatment boolean
df_extensive$treat_b <- ifelse(df_extensive$other_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_extensive$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_extensive <- left_join(df_extensive, unit_df, by = "office_id")


colnames(df_extensive)

#export this for future use
# write.csv(df_extensive,
#           paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"),
#           row.names = F)

# to skip the extensive prep
#df_extensive <- read.csv(paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"))

# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using agg_unld with imputations and no aquaculture
first_stage <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                       avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |
                       unit  ,  # + unit[year] (unit specific trends)
                     cluster = "office_id",
                     data = df_extensive[df_extensive$other_treated == 0,])
etable(first_stage)


# compare some first stage models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_fe_no_lags <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                         lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                         
                         0,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$gas_liters_treated == 0,])

no_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

no_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit fE
unit_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                           avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                           
                           unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                              avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                              lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year fE (uy)
uy_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit + year,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year + time_perio fE (uyt)
uyt_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                          avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                          
                          unit + year + time_period,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                             avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                             lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])


# compare
no_fe_tex <- etable(no_fe_no_lags, no_fe_catch_lags, no_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

unit_fe_tex <- etable(unit_fe_no_lags, unit_fe_catch_lags, unit_fe_c_sst_lags,
                      fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                      digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uy_fe_tex <- etable(uy_fe_no_lags, uy_fe_catch_lags, uy_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uyt_fe_tex <- etable(uyt_fe_no_lags, uyt_fe_catch_lags, uyt_fe_c_sst_lags,
                     fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                     digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static <- did2s(
  df_extensive_gas,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |unit + year ,
  second_stage = "other_treated",
  verbose = T
)


etable(static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dynamic <- did2s(
  df_extensive_gas,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch+avg_prec |unit + year ,
  second_stage = ~ i(rel_year),
  verbose = T
)

# make the coefficients a table
coef_df <- tidy(dynamic, conf.int = TRUE)
coef_df <- coef_df[1:24,]  # remove the last observation (inf)
coef_df$`Relative Year` <- -10:13 # make the relative year


# make an event study graph
ggplot(coef_df, aes(x = `Relative Year`, y = estimate)) +
  # RIBBON FOR CONFIDENCE INTERVAL
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  
  # LINE FOR COEFFICIENT ESTIMATES
  geom_line(size = 1) +
  
  # POINTS (optional but helpful)
  geom_point(size = 2) +
  
  # ZERO LINE
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = "Event Time (Years)",
    y = "Coefficient Estimate",
    title = "Event Study Coefficients with 95% Confidence Intervals"
  ) +
  
  theme_minimal(base_size = 14)



# #. DID2S estimation with all subsidies as treatment -------------------------------------------
# Identify the year each office was treated
# read in data (updated 14 nov 2025)
df_final <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset_13_nov_2025.csv"))


yr_1st_trtd <- df_final %>% 
  filter(all_subsidy_mxn_contemp == all_subsidy_mxn_cmltv & all_subsidy_mxn_contemp != 0) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup()
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
df_final %>% filter(year == 2024 & all_subsidy_mxn_cmltv == 0)

# combine this column with main data
df_extensive <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_extensive$yr_1st <- ifelse(is.na(df_extensive$yr_1st),0,df_extensive$yr_1st)

# make the relavive year
df_extensive$rel_year <- df_extensive$year - df_extensive$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_extensive$rel_year <- ifelse(df_extensive$yr_1st == 0,Inf,df_extensive$rel_year)

# make treatment boolean
df_extensive$treat_b <- ifelse(df_extensive$all_subsidy_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_extensive$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_extensive <- left_join(df_extensive, unit_df, by = "office_id")


colnames(df_extensive)

#export this for future use
# write.csv(df_extensive,
#           paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"),
#           row.names = F)

# to skip the extensive prep
#df_extensive <- read.csv(paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"))

# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using agg_unld with imputations and no aquaculture
first_stage_all <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                       avg_wind_speed+avg_sst+chlorophyll_conc 
                       
                       ,  # + unit[year] (unit specific trends)
                     cluster = "office_id",
                     data = df_extensive[df_extensive$all_subsidy_treated == 0,])
etable(first_stage_all)


# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_wind_speed+chlorophyll_conc| unit + year +time_period,
  second_stage = "all_subsidy_treated",
  verbose = T
)


etable(static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp_dynamic <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp_non_ac",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_sst+avg_prec+avg_wind_speed+chlorophyll_conc| unit + year,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)


####### compare the 200k buffer with the 100k buffer

first_stage_all_200k <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                           lag_sst_200k+avg_wind_speed+avg_sst_200k+chlorophyll_conc +lag_catch |
                           
                           unit + year +time_period,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$all_subsidy_treated == 0,])
etable(first_stage_all_200k)




# compare

# compare
all_bp_100_200 <- etable(first_stage_all, first_stage_all_200k, first_stage_bp, first_stage_bp_200k_sst,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)






# compare some first stage models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_fe_no_lags <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                         lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                         
                         0,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$gas_liters_treated == 0,])

no_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

no_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit fE
unit_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                           avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                           
                           unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                              avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                              lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year fE (uy)
uy_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit + year,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year + time_perio fE (uyt)
uyt_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                          avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                          
                          unit + year + time_period,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                             avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                             lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])


# compare
no_fe_tex <- etable(no_fe_no_lags, no_fe_catch_lags, no_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

unit_fe_tex <- etable(unit_fe_no_lags, unit_fe_catch_lags, unit_fe_c_sst_lags,
                      fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                      digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uy_fe_tex <- etable(uy_fe_no_lags, uy_fe_catch_lags, uy_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uyt_fe_tex <- etable(uyt_fe_no_lags, uyt_fe_catch_lags, uyt_fe_c_sst_lags,
                     fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                     digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)


# 6. DID2S estimation with AQUACULTURE as treatment and Aquaculture output -------------------------------------------
# Identify the year each office was treated
# read in data (10 Dec 2025)
df_final <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset.csv"))

# find first year treated by aquaculture
yr_1st_trtd <- df_final %>% 
  filter(acuacultura_mxn_value == cumulative_acuacultura_mxn & acuacultura_mxn_value != 0) %>%
  select(year, office_id) %>%
  group_by(office_id) %>%
  slice(1) %>% # this selects the first observation per group, ensuring first year
  ungroup()
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
df_final %>% filter(year == 2024 & cumulative_acuacultura_mxn == 0)

# combine this column with main data
df_extensive <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_extensive$yr_1st <- ifelse(is.na(df_extensive$yr_1st),0,df_extensive$yr_1st)

# make the relavive year
df_extensive$rel_year <- df_extensive$year - df_extensive$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df_extensive$rel_year <- ifelse(df_extensive$yr_1st == 0,Inf,df_extensive$rel_year)

# make treatment boolean
df_extensive$treat_b <- ifelse(df_extensive$acua_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(df_extensive$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_extensive <- left_join(df_extensive, unit_df, by = "office_id")


colnames(df_extensive)

#export this for future use
# write.csv(df_extensive,
#           paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"),
#           row.names = F)

# to skip the extensive prep
#df_extensive <- read.csv(paste0(dir$cleandata,"extensive_margin_ready_dataset.csv"))

# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using agg_unld with imputations and no aquaculture

first_stage_all <- feols(agg_unld_wt_kg_imp_aquaculture ~
                           chlorophyll_conc+avg_sst+avg_prec+lag_sst+avg_prec
                         |unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$acua_treated == 0,])
etable(first_stage_all)


# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp_aquaculture",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~chlorophyll_conc+avg_sst+avg_prec+lag_sst+avg_prec | unit ,
  second_stage = "acua_treated",
  verbose = T
)


etable(static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp_dynamic <- did2s(
  df_extensive,
  yname = "agg_unld_wt_kg_imp_aquaculture",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~0 | unit ,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(bp_dynamic)
etable(bp_dynamic)


####### compare the 200k buffer with the 100k buffer

first_stage_all_200k <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                                lag_sst_200k+avg_wind_speed+avg_sst_200k+chlorophyll_conc +lag_catch |
                                
                                unit + year +time_period,  # + unit[year] (unit specific trends)
                              cluster = "office_id",
                              data = df_extensive[df_extensive$all_subsidy_treated == 0,])
etable(first_stage_all_200k)




# compare

# compare
all_bp_100_200 <- etable(first_stage_all, first_stage_all_200k, first_stage_bp, first_stage_bp_200k_sst,
                         fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                         digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)






# compare some first stage models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
no_fe_no_lags <- feols(agg_unld_wt_kg_imp_non_ac ~ 
                         lag_sst+avg_wind_speed+avg_sst+chlorophyll_conc +lag_catch |
                         
                         0,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$gas_liters_treated == 0,])

no_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

no_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            0,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit fE
unit_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                           avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                           
                           unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                              avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

unit_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                              lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                              
                              unit,  # + unit[year] (unit specific trends)
                            cluster = "office_id",
                            data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year fE (uy)
uy_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                         avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                         
                         unit + year,  # + unit[year] (unit specific trends)
                       cluster = "office_id",
                       data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                            avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

uy_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                            lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                            
                            unit + year,  # + unit[year] (unit specific trends)
                          cluster = "office_id",
                          data = df_extensive[df_extensive$d_treated == 0,])

#### unit + year + time_perio fE (uyt)
uyt_fe_no_lags <- feols(agg_unld_wt_kg_imp ~ 
                          avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc  |
                          
                          unit + year + time_period,  # + unit[year] (unit specific trends)
                        cluster = "office_id",
                        data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_catch_lags <- feols(agg_unld_wt_kg_imp ~ 
                             avg_prec+avg_wind_speed+avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])

uyt_fe_c_sst_lags <- feols(agg_unld_wt_kg_imp ~ 
                             lag_sst +avg_wind_speed + avg_sst+chlorophyll_conc + lag_catch |
                             
                             unit + year + time_period,  # + unit[year] (unit specific trends)
                           cluster = "office_id",
                           data = df_extensive[df_extensive$d_treated == 0,])


# compare
no_fe_tex <- etable(no_fe_no_lags, no_fe_catch_lags, no_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

unit_fe_tex <- etable(unit_fe_no_lags, unit_fe_catch_lags, unit_fe_c_sst_lags,
                      fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                      digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uy_fe_tex <- etable(uy_fe_no_lags, uy_fe_catch_lags, uy_fe_c_sst_lags,
                    fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                    digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)

uyt_fe_tex <- etable(uyt_fe_no_lags, uyt_fe_catch_lags, uyt_fe_c_sst_lags,
                     fitstat = ~ n + rmse + r2 + aic + bic + ar2,
                     digits = 5,digits.stats = 3,tex = TRUE,placement = "H",powerBelow = -7)


# #. DID2S Intensive BP w/Lisa output -------------------------------------------
# Identify the year each office was treated
# read in data (10 Dec 2025)
df_final <- read.csv(paste0(dir$cleandata,"final_estimation_ready_dataset.csv"))

colnames(df_final)
# Identify the contemporary and cumulative treatment variables
df_final$contemp <- df_final$contemp_bp
df_final$cumulative <- df_final$cumulative_bp

# Identify treatment dummy (in this case, reaching the 1st quarter of bp_cumulative)
df_final$d_treated <- df_final$bp_cmltv_1q

# Identify output of interest
df_final$output <- df_final$agg_unld_wt_kg_imp_lisa

# Find first year treated by treatment
yr_1st_trtd <- df_final %>%
  filter(d_treated == 1) %>%
  group_by(office_id) %>%
  summarize(yr_1st = min(year), .groups = "drop")

# identify office never treated
df_final %>% filter(year == max() & .data[[d_treated]] == 0)

# combine this column with main data
df_estimation <- left_join(df_final,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df_estimation$yr_1st <- ifelse(is.na(df_estimation$yr_1st),0,df_estimation$yr_1st)

# make the relavive year
df_estimation$rel_year <- df_estimation$year - df_estimation$yr_1st

# make inf for those who were not treated, values would be higher than 2000. Turn this to inf
df_estimation$rel_year <- ifelse(df_estimation$rel_year > 2000,Inf,df_estimation$rel_year)

# make treatment boolean (T/F variable for when treatment is on)
df_estimation$treatment_on <- ifelse(df_estimation$d_treated == 1, TRUE, FALSE)

# make a numerical value for each office for the unit identifier (for fixed effects)
unit_list <- unique(df_estimation$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df_estimation <- left_join(df_estimation, unit_df, by = "office_id")


colnames(df_estimation)


# first stage estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# first stage requires that it is done on the untreated (d_treated == 0)
first_stage_all <- feols(output ~
                           chlorophyll_conc+lag_chl+avg_sst+lag_sst+avg_prec+avg_wind_speed
                         |unit,  # + unit[year] (unit specific trends)
                         cluster = "office_id",
                         data = df_estimation[df_estimation$d_treated == 0,])
etable(first_stage_all)


# Second stage: average treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static <- did2s(
  df_estimation,
  yname = "output",
  treatment = "treatment_on",
  cluster_var = "office_id",
  first_stage = ~chlorophyll_conc+lag_chl+avg_sst+lag_sst+avg_prec+avg_wind_speed | unit ,
  second_stage = "d_treated",
  verbose = T
)


etable(static)


# event study: annual treatment effect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

annual <- did2s(
  df_estimation,
  yname = "output",
  treatment = "treatment_on",
  cluster_var = "office_id",
  first_stage = ~chlorophyll_conc+lag_chl+avg_sst+lag_sst+avg_prec+avg_wind_speed | unit,
  second_stage = ~ i(rel_year),
  verbose = T
)

coefplot(annual)
etable(annual)

# make the coefficients a table
coef_df <- tidy(annual, conf.int = TRUE)
coef_df <- coef_df[1:24,]  # remove the last observation (inf)
coef_df$`Relative Year` <- -10:13 # make the relative year


# make an event study graph
ggplot(coef_df, aes(x = `Relative Year`, y = estimate)) +
  # RIBBON FOR CONFIDENCE INTERVAL
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  
  # LINE FOR COEFFICIENT ESTIMATES
  geom_line(size = 1) +
  
  # POINTS (optional but helpful)
  geom_point(size = 2) +
  
  # ZERO LINE
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # event line
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  labs(
    x = "Event Time (Years)",
    y = "Coefficients on kg with 95% CI",
    title = "Intensive impact of PP/BP (1st Quarter) on Lisa Output"
  ) +
  
  theme_bw(base_size = 14)

