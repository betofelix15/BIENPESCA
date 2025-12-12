# Title: 10: covariate relationships
# Purpose: see relationship between covarites and if there exist better/more signifcant variables
# Author: Jesus Felix
# Start date: 30 JUN 2025
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
          "kableExtra","gdata","did2s","fixest","GGally")

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


# 1. load/quick clean all data --------------------------------------------------------------------
# load the monthly variation/annual treatment/all offices present
df <- read.csv(paste0(dir$cleandata,"monthly_prod_annual_bp_dataset1.csv"))

##############~~
#covariate data
##############~~~~~~~

# sst celsius 
sst <- read.csv(paste0(dir$cleandata,"sst_all_offices_2006_2024.csv"))
sst$avg_sst <- sst$avg_sst*0.01 # multiply by band scale
sst$sd_sst <- sst$sd_sst*0.01 # multiply by band scale
# remove unnecessary columns
sst <- sst %>% select(-.geo,-system.index)

# what is the average sst in the summer?
summer <- c(6,7,8)
summary(sst[sst$month %in% summer,]$avg_sst)
# what if we wanted to see how many days in a month the sst was above 28 C?

# sst anomaly
anom <- read.csv(paste0(dir$cleandata,
                        "sst_anomaly_all_offices_2006_2024.csv"))
anom <- anom %>% select(-.geo,-system.index)

anom$avg_sst_anomaly <- anom$avg_sst_anomaly*0.01 # scale to celcius

# coastal precipitation land only (meters)
prec <- read.csv(paste0(dir$cleandata,"coastal_precip_2006_2024_all.csv"))

summary(prec$avg_prec)

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



df_test <- left_join(df,sst, by = c("office_id","year","month"))
df_test <- left_join(df_test,anom, by = c("office_id","year","month"))
df_test <- left_join(df_test,clp, by = c("office_id","year","month"))
df_test <- left_join(df_test,prec, by = c("office_id","year","month"))
df_test <- left_join(df_test,ws, by = c("office_id","year","month"))
df_test <- left_join(df_test,cpi_annual, by = "year")


# adjust values
df_test$agg_value_mx_deflated <- df_test$agg_value_mxn/df_test$cpi
df_test$agg_value_mx_deflated_imp <- df_test$agg_value_mxn_imp/df_test$cpi

df_test$agg_value_shrimp_deflated <- df_test$agg_value_mxn_shrimp/df_test$cpi
df_test$agg_value_shrimp_deflated_imp <- df_test$agg_value_mxn_imp_shrimp/df_test$cpi

df_test$agg_value_lisa_deflated <- df_test$agg_value_mxn_imp_lisa/df_test$cpi
df_test$agg_value_shrimp_deflated_imp <- df_test$agg_value_mxn_imp_lisa/df_test$cpi

df_test$cumulative_deflated <- df_test$cumulative_bp/df_test$cpi
df_test$contemporary_deflated <- df_test$contemp_bp/df_test$cpi

colnames(df_test)

### pairs plot

# Custom smoother function with colored line
my_smoother <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_smooth(color = "blue", ...) +
    geom_point(alpha = 0.25)  # optional: keep points
}

ggpairs(df_test,
        columns = c(45,47,48,50,54),
        lower = list(continuous = my_smoother))

# check for summer difference
ggpairs(df_test[df_test$month %in% c(6,7,8),],
        columns = c(45,47,48,50,54,17), # 17 is shrimp weight
        lower = list(continuous = my_smoother))

