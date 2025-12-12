# Title: 11: Testing grounds
# Purpose: to test estimations and any other codes
# Author: Jesus Felix
# Start date: 06 DEC 2024
################################################################################
# This version does municipio level data first then summarizes for the states

#### R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("lattice","terra","foreign","RColorBrewer","rgdal", 
          "matrixStats","moments","maptools","scales","mapview", "sf","readxl","sqldf",
          "dplyr","ggplot2","tidyverse", 'tmap', 'av',"gifski","magick","stringi","caTools",
          "kableExtra","gdata","did2s","fixest")

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

##### DID2S package ------------------------------------------------------------------------------

data("df_hom")

static = did2s(
  df_hom,
  yname = "dep_var", treatment = "treat", cluster_var = "state",
  first_stage = ~ 0 | unit + year,
  second_stage = ~ i(treat, ref=FALSE),
  verbose = TRUE
)

etable(static)

# alternatively we can use the relative year, we estimate event-study type coefficients

dynamic = did2s(df_hom,
                yname = "dep_var", treatment = "treat", cluster_var = "state",
                first_stage = ~ 0 | unit + year,
                second_stage = ~ i(rel_year, ref=c(-1, Inf)),
                verbose = FALSE
)


# plot rel_year coefficients and standard errors
coefplot(dynamic)
y <- dynamic$coefficients
####
#### now with our data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####
# load data
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

## data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # deflate prices
# cpi <- read_excel(paste0(dir$rawdata,"cpi_monthly_by_city_mexico.xlsx"))
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

# add to data
d_2 <- left_join(d_2,cpi_annual, by = "year")
d_2$agg_value_mx_deflated <- d_2$agg_value_mx/d_2$cpi
d_2$cumulative_deflated <- d_2$cumulative/d_2$cpi
d_2$contemporary_deflated <- d_2$contemporary/d_2$cpi

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% filter(contemporary == cumulative & contemporary != 0)
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
d_2 %>% filter(year == 2022 & cumulative == 0)

# combine this column with main data
d_2 <- left_join(d_2,yr_1st_trtd[,c("office_id","yr_1st")], by = "office_id")

# if never treated, then make yr_1st_trtd == 0
d_2$yr_1st <- ifelse(is.na(d_2$yr_1st),0,d_2$yr_1st)

# make the relavive year
d_2$rel_year <- d_2$year - d_2$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
d_2$rel_year <- ifelse(d_2$yr_1st == 0,Inf,d_2$rel_year)

# make treatment boolean
d_2$treat_b <- ifelse(d_2$d_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(d_2$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
d_2 <- left_join(d_2, unit_df, by = "office_id")


## OK, at this point we can regress
## estimate two-stage did on weight of catch~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bp_static <- did2s(
  d_2,
  yname = "agg_wt_unloaded",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = "d_treated",
  verbose = T
)


etable(bp_static)


# now for the the dynamic

bp_dynamic <- did2s(
  d_2,
  yname = "agg_wt_unloaded",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = ~ i(rel_year, ref=c(-1, Inf)),
  verbose = T
)

# plot it
coefplot(bp_dynamic)

# now on revenue

bp_static <- did2s(
  d_2,
  yname = "agg_value_mx_deflated",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = "d_treated",
  verbose = T
)


etable(bp_static)


# now for the the dynamic

bp_dynamic <- did2s(
  d_2,
  yname = "agg_value_mx_deflated",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ 0| unit + year,
  second_stage = ~ i(rel_year, ref=c(-1, Inf)),
  verbose = T
)

# plot it
coefplot(bp_dynamic)

att <- bp_dynamic$coefficients



##### did2s monthly outputs, annual treatment, covariates ------------------------------------

### data load
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))
pd <- read.csv(paste0(dir$cleandata, 'production_nodup_feb2025.csv'))
wind <- read.csv(paste0(dir$cleandata,"avg_wind_speed_2005_2023.csv"))
sst <- read.csv(paste0(dir$cleandata,"sst_average_2005_2023.csv"))
chloropyll <- read.csv(paste0(dir$cleandata,"chlorophyll_concentration_mg_m3.csv"))
chloropyll$year <- year(chloropyll$date)
chloropyll$month <- month(chloropyll$date)
avg_prec <- read.csv(paste0(dir$cleandata,"avg_prec_2005_2023_fluctuations_land_and_sea.csv"))

# combine it
df <- left_join(pd[pd$year > 2005,],
                d_2, by = c("year","state","state_id","office","office_id"))
df <- left_join(df,wind,by = c("year","month","state","state_id","office","office_id"))
df <- left_join(df,sst,by = c("year","month","state","state_id","office","office_id"))
df <- left_join(df,chloropyll,by = c("year","month","office_id"))
df <- left_join(df,avg_prec,c("year","month","state","state_id","office","office_id"))


# if its missing a value for contemp, cumulative, treated, c_100k etct, make it zero
df$contemporary <- ifelse(is.na(df$contemporary),0,df$contemporary) 
df$cumulative <- ifelse(is.na(df$cumulative),0,df$cumulative) 
df$d_treated <- ifelse(is.na(df$d_treated),0,df$d_treated) 
df$c_100k <- ifelse(is.na(df$c_100k),0,df$c_100k) 
df$c_500k <- ifelse(is.na(df$c_500k),0,df$c_500k) 
df$c_2m <- ifelse(is.na(df$c_2m),0,df$c_2m) 
df$c_5m <- ifelse(is.na(df$c_5m),0,df$c_5m) 


# deflate
cpi <- read.csv(paste0(dir$cleandata, "cpi_clean_mexico_monthly.csv"))

# add to data
df <- left_join(df,cpi, by = c("year","month"))
df$value_mx_deflated <- df$value_mx/df$nat_cpi_2022_base
df$cumulative_deflated <- df$cumulative/df$nat_cpi_2022_base
df$contemporary_deflated <- df$contemporary/df$nat_cpi_2022_base


## now prepare for estimation
# Identify the year each office was treated
yr_1st_trtd <- df %>% filter(contemporary == cumulative & contemporary != 0)
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

yr_1st_trtd <- unique(yr_1st_trtd[c("office_id","yr_1st")])

# identify office never treated
df %>% filter(year == 2022 & cumulative == 0)


# combine this column with main data
df2 <- left_join(df,yr_1st_trtd, by = "office_id")

# if never treated, then make yr_1st_trtd == 0
df2$yr_1st <- ifelse(is.na(df2$yr_1st),0,df2$yr_1st)

# make the relavive year
df2$rel_year <- df2$year - df2$yr_1st

# make inf for those who were not treated, values would be higher than 1000, or yr_1st == 0
df2$rel_year <- ifelse(df2$yr_1st == 0,Inf,df2$rel_year)

# make treatment boolean
df2$treat_b <- ifelse(df2$d_treated == 1, TRUE,FALSE)

# make a numerical value for each office for the unit identifier
unit_list <- unique(d_2$office_id)
unit_df <- as.data.frame(cbind(unit_list,c(1:length(unit_list))))    
colnames(unit_df) <- c("office_id","unit")
df2 <- left_join(df2, unit_df, by = "office_id")

# Save this dataset
#write.csv(df2, paste0(dir$cleandata, "monthly_outcome_annual_trt_dataset1.csv"),
#          row.names = F)

# load dataset
d_2 <- read.csv(paste0(dir$cleandata, "monthly_outcome_annual_trt_dataset1.csv"))

# filter by captura
# choose a fish or do the aggregate (if not fish specific, remove" & category == "x"")
d_agg <- d_2 %>%
  filter(origin == "CAPTURA") %>%
  group_by(year,month,state_id,state,office_id,office,unit,d_treated,rel_year,
           treat_b,avg_wind_speed,avg_sst,chlorophyll_conc,avg_prec,yr_1st, cumulative_deflated,contemporary_deflated) %>%
  summarize(
    agg_wt_unloaded = sum(unload_wt),
    agg_revenue = sum(value_mx_deflated)
  )

# annually
# d_agg <- d_2 %>%
#   filter(origin == "CAPTURA") %>%
#   group_by(year,state_id,state,office_id,office,unit,d_treated,rel_year,
#            treat_b,yr_1st, cumulative_deflated,contemporary_deflated) %>%
#   summarize(
#     agg_wt_unloaded = sum(unload_wt, na.rm = T),
#     agg_revenue = sum(value_mx_deflated, na.rm = T),
#     avg_wind_speed = mean(avg_wind_speed, na.rm = T),
#     avg_sst = mean(avg_sst, na.rm = T),
#     avg_chlr = mean(chlorophyll_conc, na.rm = T),
#     tot_prec = sum(avg_prec, na.rm = T)
#   )


# add a numerical identifier for date
date_seq <- seq(as.Date("2006-01-01"),as.Date("2022-12-01"), by = "month")
date_num <- 1:length(date_seq)
date_df <- as.data.frame(cbind(date_seq,date_num))
date_df$date_seq <- as.Date(date_df$date_seq)
date_df$month <- month(date_df$date_seq)
date_df$year <- year(date_df$date_seq)
d_agg <- left_join(d_agg,date_df, by = c("year","month"))

# add a lag
#install.packages("spduration")
# library(spduration)
# d_agg <- panel_lag(x = "agg_wt_unloaded",
#                    id = "office_id", 
#                    t = "date_num",
#                    lag = 1, 
#                    data = d_agg) 

d_lagged <- d_agg %>%
  arrange(office_id,date_num) %>%
  group_by(office_id) %>%
  mutate(agg_wt_lagged = lag(agg_wt_unloaded, n = 1))


# adding covariates
bp_static <- did2s(
  d_lagged,
  yname = "agg_wt_unloaded",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~  avg_wind_speed + avg_sst + chlorophyll_conc + avg_prec + agg_wt_lagged| unit + date_num,
  #second_stage = ~(cumulative_deflated), # for cumulative
  second_stage = "d_treated",
  verbose = T
)


etable(bp_static)


# try manually ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# stage 1 ~~~~~~~~
# remove unit fixed effects
data <- d_agg %>%
  group_by(office_id) %>%
  mutate(Y_demeaned_unit = agg_wt_unloaded - mean(agg_wt_unloaded)) %>%
  ungroup()


# remove time fixed effects
data$post <- ifelse(data$rel_year >= 0,1,0)
data$treat_post <- data$d_treated * data$post

data <- data %>%
  group_by(post) %>%
  mutate(Y_demeaned = Y_demeaned_unit - mean(Y_demeaned_unit)) %>%
  ungroup()

# stage 2 ~~~~~~~

stg_2 <- lm(Y_demeaned ~ treat_post,
   data = data)

summary(stg_2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# now for the the dynamic (in the annual)

bp_dynamic <- did2s(
  d_agg,
  yname = "agg_wt_unloaded",
  treatment = "treat_b",
  cluster_var = "office_id",
  first_stage = ~ avg_wind_speed + avg_sst + avg_chlr + tot_prec | unit + year,
  second_stage =  ~ i(rel_year, ref=c(-1, Inf)),
  verbose = T
)
 
# plot it
coefplot(bp_dynamic)

##### GARDNER-STYLE DYNAMIC DID ---------------------------------------------------------
library(eventstudyinteract)

es <- event_study(
  data = df_extensive,
  outcome_var = "agg_unld_wt_kg_imp",
  unit_var = "office_id",
  time_var = "year",
  treatment_status_var = "treat_b",  # 0/1 dummy for treated units
  cohort_var = "treatment_year",     # First treatment year for each group
  control_vars = c("avg_sst","avg_prec","avg_wind_speed","chlorophyll_conc",
                   "lag_sst","lag_wind","lag_chl","lag_prec","lag_catch"),
  untreated_when = "never treated"
)






##### SST with 100 km buffer or a 200 km buffer -------------------------------------

sst_100k <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024.csv"))

sst_200k <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024_200km_buffer.csv"))

t.test(sst_100k$avg_sst,sst_200k$avg_sst, paired = T)

sst_join <- left_join(sst_100k,sst_200k, by = c("year","month","office_id"), suffix = c("_100km","_200km"))

sst_join$sst_diff <- sst_join$avg_sst_100km - sst_join$avg_sst_200km





