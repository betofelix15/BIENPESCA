# Google Earth Engine Covariates
# created April 1, 2025
# Author: Jesus Felix
# Description: Code for obtaining covariates from google earth engine. These are 
# typically the ecological covariates that impact fishing capabilities. Examples:
# algal blooms, wind, sea surface temperature, etc.


#### 0. Pre-requesites ----------------------------------------------------------------
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

# create not in
`%not_in%` <- negate(`%in%`)

# remove scientific notation
options(scipen = 999)

#### 1. Set up python and rgee ------------------------------------------------------------

# check whether python is installed (using reticulate package)
py_available()

# if false, install
py_discover_config()
# save this: C:/Users/Jesus/OneDrive/Documents/.virtualenvs/rgee/Scripts/python.exe 


# install python environment (using rgee package)
ee_install_set_pyenv(
  py_path = "C:/Users/Jesus/OneDrive/Documents/.virtualenvs/rgee/Scripts/python.exe",
  py_env = "rgee"
)

reticulate::py_install("earthengine-api", envname = "rgee")

ee_install_upgrade() # this one has worked if you have issues later on (after installing environment, packages, etc.)

# since this has restarted r session, reload the pre-requisites and check that
# dependencies are installed.
ee_check()
ee_Authenticate(user = "j.felix@usu.edu")
# set up connection between r and google earth
ee_Initialize(user = "j.felix@usu.edu", project = "ee-sst-j-felix")

# if asked to generate toke, save it and apply

# I guess numpy is needed
reticulate::py_install("numpy", envname = "rgee")

#### 2. Load/Transform CONAPESCA office boundaries ------------------------------

# load data for office points
office_sf <- read_sf(paste0(dir$shp, "office_points_latest.shp"))
colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                          "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                          "opccupied_households",  "obs_id",   "municipio",  "geometry")

# plot and visualize points
plot(office_sf$geometry)

# for this, it would help to have the longitude and lattitude in seperate columns
coords <- as.data.frame(st_coordinates(office_sf$geometry))

# drop the geometry
office_points <- as.data.frame(st_drop_geometry(office_sf))

# add columns for lat long
office_points$long <- coords$X
office_points$lat <- coords$Y

#### 3. Sea Surface Temperature (SST) anomaly  -----------------------------------------------------------------
# GE Title: NOAA CDR OISST v02r01: Optimum Interpolation Sea Surface Temperature

# https://developers.google.com/earth-engine/datasets/catalog/NOAA_CDR_OISST_V2_1

# Description: The NOAA 1/4 degree daily Optimum Interpolation Sea Surface Temperature (OISST) provides complete ocean temperature fields constructed by combining bias-adjusted observations from different platforms (satellite, ships, buoys) on a regular global grid, with gaps filled in by interpolation. Satellite data from the Advanced Very High Resolution Radiometer (AVHRR) provides the main input which permits the high temporal-spatial coverage beginning in late 1981 to the present.

# The OISST dataset has a single day's data processed twice. First a near real-time preliminary version is released with a lag of 1 day, and a final version with a lag of 14 days. The final version uses extra days for smoothing, and zonal bias correction in addition to replacing the preliminary version.

# citation: Richard W. Reynolds, Viva F. Banzon, and NOAA CDR Program (2008): NOAA Optimum Interpolation 1/4 Degree Daily Sea Surface Temperature (OISST) Analysis, Version 2. [indicate subset used]. NOAA National Centers for Environmental Information. doi:10.7289/V5SQ8XB5 [access date].

# DOI: https://doi.org/10.7289/V5SQ8XB5

# Terms of Use: The NOAA CDR Program's official distribution point for CDRs is NOAA's National Climatic Data Center which provides sustained, open access and active data management of the CDR packages and related information in keeping with the United States' open data policies and practices as described in the President's Memorandum on "Open Data Policy" and pursuant to the Executive Order of May 9, 2013, "Making Open and Machine Readable the New Default for Government Information". In line with these policies, the CDR data sets are nonproprietary, publicly available, and no restrictions are placed upon their use. For more information, see the Fair Use of NOAA's CDR Data Sets, Algorithms and Documentation pdf.

# availability: 1981 to  2025

# provider: NOAA

# Cadence: 1 Day

# Pixel size: 27830 meters

# ee snippet: ee.ImageCollection("NOAA/CDR/OISST/V2_1")

# band of use: anom

# band information - 	Units: Celsius, min:-1887*, max:1902* (* = estimated), scale:0.01
# Temperature anomaly; the daily OISST minus a 30-year climatological mean.

# helpful band: sst, daily sea surface temp

# lets visualize this image set with guaymas office, office number 2604

# generally, its suggested to take a point and turn it into an ee object then make the buffer

# Define a point geometry (longitude, latitude), make the office point an ee object
guaymas <- ee$Geometry$Point(c(office_points[office_points$office_id == 2604,]$long,
                                        office_points[office_points$office_id == 2604,]$lat))

# create the ee buffer
guaymas_buff <- guaymas$buffer(100000)

# select the time period
d1 <- "2006-01-01"
d2 <- "2006-01-31"
# bring the data from ee
anom_collection<- ee$ImageCollection("NOAA/CDR/OISST/V2_1")$ # load the image
  filter(ee$Filter$date(d1,d2))$ # filter the date
  select('anom') # select the band

# Reduce to a single mean image clipped to a boundary
mean_anom <- anom_collection$mean()$clip(guaymas_buff)

# define visual parameters
vis_params <- list(
  min = 0,        # Minimum value for scaling
  max = 100,      # Maximum value for scaling
  palette = c('blue', 'green', 'yellow', 'red') # Color scale
)

# display the map
Map$centerObject(mean_anom, zoom = 3)
Map$addLayer(mean_anom, vis_params, "Mean SST Anomaly")

# now extract the daily data:

# Function to extract daily mean SST
extract_anom <- function(image) {
  # Get image date
  date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
  
  # Reduce the region (mean temperature)
  mean_anom <- image$reduceRegion(
    reducer = ee$Reducer$mean(),
    geometry = guaymas_buff,
    scale = 5000,   # Adjust scale for resolution
    maxPixels = 1e13
  )$get('anom')
  
  # Return as a feature (date, SST)
  ee$Feature(NULL, list(date = date, anomaly = mean_anom))
}

# Apply function to each image
anom_features <- anom_collection$map(extract_anom)

# Convert to FeatureCollection and extract as table
anom_table <- ee$FeatureCollection(anom_features)

# Convert to R DataFrame
anom_df <- st_drop_geometry(ee_as_sf(anom_table))

# multiply by the scale (0.01)
anom_df$anomaly <- anom_df$anomaly*0.01

# Print first few rows
head(anom_df)

# calculate number of days above a 2.5 Celsius anomaly and save
year <- year(d1)
month <- month(d1)
office <- "guaymas"
n_days_2.5_anom <- nrow(anom_df[anom_df$anomaly > 2.5,])
average_anom <- mean(anom_df$anomaly)
guaymas_df <- as.data.frame(cbind(year,month,office,n_days_2.5_anom, average_anom))

### Now lets do it for all offices from 2006 to 2023 ################### #



# List of the offices by id
office_list <- as.list(office_points$office_id) # 171 offices

# create the df to save the data in
sst_anom_df <- NULL

# Generate monthly date list
months <- as.list(as.character(seq(as.Date('2006-01-01'), as.Date('2023-12-31'), by = 'month')))

# start the loop
for(i in office_list[160:171]){ # accidently stopped after 2803, needs to start from 1205 (number 62) onward
  
  # identify the lat and long
  long <- office_points[office_points$office_id == i,]$long
  lat <- office_points[office_points$office_id == i,]$lat
  
  # Define a point geometry (longitude, latitude), make the office point an ee object
  p <- ee$Geometry$Point(c(long,lat))
  
  # create the ee buffer (always 100 km radius)
  p_buff<- p$buffer(100000)
  
  # create an empty dataframe to save monthly values
  monthly_anom <- NULL
  
  # loop to get mean cloud coverage for a month
  for(m in months){ # m <- "2006-01-01"
    
    d1 <- as.Date(m)
    d2 <- as.Date(d1 %m+% months(1)) # new month
    
    # change these into a character (it just works that way)
    d1 <- as.character(d1)
    d2 <- as.character(d2)
    
    # filter the date and clip to buffer in the anom_collection
    filtered_anom<- ee$ImageCollection("NOAA/CDR/OISST/V2_1")$
      filter(ee$Filter$date(d1,d2))$
      select('anom')# filter date

    # Function to extract daily mean SST
    extract_anom <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
      
      # Reduce the region (mean temperature)
      mean_anom <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = p_buff,
        scale = 5000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('anom')
      
      # Return as a feature (date, SST)
      ee$Feature(NULL, list(date = date, anomaly = mean_anom))
    }
    
    # Apply function to each image
    anom_features <- filtered_anom$map(extract_anom)
    
    # Convert to FeatureCollection and extract as table
    anom_table <- ee$FeatureCollection(anom_features)
    
    # Convert to R DataFrame
    anom_df <- st_drop_geometry(ee_as_sf(anom_table))
    
    # for those inland offices
    if(length(anom_df) == 1){
      
      anom_df$anomaly <- 0
    }
    
    # multiply by the scale (0.01)
    anom_df$anomaly <- anom_df$anomaly*0.01
    
    # calculate number of days above a 2.5 Celsius anomaly and save
    year <- year(d1)
    month <- month(d1)
    office <- office_points[office_points$office_id == i, 1:4]
    days_above_2.5_anom <- nrow(anom_df[anom_df$anomaly > 2.5,])
    days_below_neg2.5_anom <- nrow(anom_df[anom_df$anomaly < -2.5,])
    average_anom <- mean(anom_df$anomaly, na.rm = T)
    office_m_df <- as.data.frame(cbind(year,month,office,average_anom,
                                       days_above_2.5_anom,days_below_neg2.5_anom))
                                          
    # save in the monthly
    monthly_anom <- rbind(monthly_anom,office_m_df)
    
    # print month
    print(paste0(m," for ",i," is done!"))
  }
  
  sst_anom_df <- rbind(sst_anom_df,monthly_anom)
  
  print(paste0(i," is finished!"))

}

# accidently hit esc, m was "2017-01-01", on 1205

write.csv(sst_anom_df,paste0(dir$cleandata,"sst_anomally_2006_2023.csv"),
          row.names = F)

## quick analysis

hist(sst_anom_df$average_anom)
hist(sst_anom_df$days_above_2.5_anom)
hist(sst_anom_df$days_below_neg2.5_anom)

summary(sst_anom_df)


#### 3.2 SST Average 2006 to 2023 ------------------------------------------------------------
# https://developers.google.com/earth-engine/datasets/catalog/NOAA_CDR_SST_PATHFINDER_V53
# NOAA AVHRR Pathfinder Version 5.3 Collated Global 4km Sea Surface Temperature 
# citation: Baker-Yeboah, S., K. Saha, D. Zhang, K. S. Casey, R. Evans, and K. A. Kilpatrick (2016). 'Pathfinder Version 5.3 AVHRR Sea Surface Temperature Climate Data Record', Fall AGU 2016 Poster (manuscript in progress)
# Terms of Use: NOAA data, information, and products, regardless of the method of delivery, are not subject to copyright and carry no restrictions on their subsequent use by the public. Once obtained, they may be put to any lawful use. The forgoing data is in the public domain and is being provided without restriction on use and distribution. For more information see the 'constraints' section in https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.nodc:AVHRR_Pathfinder-NCEI-L3C-v5.3.
# DOI: https://doi.org/10.7289/V52J68XX
# snippet: ee.ImageCollection("NOAA/CDR/SST_PATHFINDER/V53")
# cadence 12 hrs
# pixel size: 4000 meters
# band: sea_surface_temperature
# band unit: K
# band scale: 0.01
# band offset: 273.15, this means that just mulitplying by 0.01 to the orginal K value gives you celsius
# band description: skin temperature of the ocean
# sst_avg_df <- read.csv(paste0(dir$cleandata,"sst_average_2005_2023.csv"))

# List of the offices by id
office_list <- as.list(office_points$office_id) # 165 offices




# Create a FeatureCollection of all office buffers
office_fc <- ee$FeatureCollection(
  lapply(1:nrow(office_points), function(i) {
    ee$Feature(
      ee$Geometry$Point(c(office_points$long[i], office_points$lat[i]))$buffer(100000),
      list(
        office_id = office_points$office_id[i],
        state = office_points$state[i],
        state_id = office_points$state_id[i],
        office = office_points$office[i]
      )
    )
  })
)





# create the df to save the data in
sst_avg_df <- NULL

# Generate annual date list
years <- as.list(as.character(seq(as.Date('2005-01-01'), as.Date('2023-12-31'), by = 'year')))


# start the loop
for(i in office_list[119:length(office_list)]){ # it stopped in 2511, needs rerunning, i <- 2511
  
  # identify the lat and long
  long <- office_points[office_points$office_id == i,]$long
  lat <- office_points[office_points$office_id == i,]$lat
  
  # Define a point geometry (longitude, latitude), make the office point an ee object
  p <- ee$Geometry$Point(c(long,lat))
  
  # create the ee buffer (always 100 km radius)
  p_buff<- p$buffer(100000)
  
  # create an empty dataframe to save monthly values
  annual_sst <- NULL
  
  # loop to get mean cloud coverage for a month
  for(y in years){ # y <- "2006-01-01"
    
    d1 <- as.Date(y)
    d2 <- as.Date(d1 %m+% years(1)-1) # the end of the year
    
    # change these into a character (it just works that way)
    d1 <- as.character(d1)
    d2 <- as.character(d2)
    
    # filter the date and clip to buffer in the sst_collection
    filtered_sst<- ee$ImageCollection("NOAA/CDR/SST_PATHFINDER/V53")$
      filter(ee$Filter$date(d1,d2))$
      select('sea_surface_temperature')# filter date
    
    # Function to extract daily mean SST
    extract_sst <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
      
      # Reduce the region (mean temperature)
      mean_sst <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = p_buff,
        scale = 8000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('sea_surface_temperature')
      
      # Return as a feature (date, SST)
      ee$Feature(NULL, list(date = date, sst = mean_sst))
    }
    
    # Apply function to each image
    sst_features <- filtered_sst$map(extract_sst)
    
    # Convert to FeatureCollection and extract as table
    sst_table <- ee$FeatureCollection(sst_features)
    
    # Convert to R DataFrame
    sst_df <- st_drop_geometry(ee_as_sf(sst_table))
    
    # for those inland offices
    if(length(sst_df) == 1){
      
      sst_df$sst <- 0
    }
    
    # organize the new dataframe
    # multiply by the scale (0.01)
    sst_df$sst <- sst_df$sst*0.01
    sst_df$year <- year(sst_df$date)
    sst_df$month <- month(sst_df$date)
    
    sst_df <- sst_df %>%
      group_by(year,month) %>%
      summarize(avg_sst = mean(sst, na.rm = T))
    
    sst_df$state_id <- office_points[office_points$office_id == i,]$state_id
    sst_df$state <- office_points[office_points$office_id == i,]$state
    sst_df$office_id <- office_points[office_points$office_id == i,]$office_id
    sst_df$office <- office_points[office_points$office_id == i,]$office
    
    # save in the monthly
    annual_sst <- rbind(annual_sst,sst_df)
    
    # print month
    print(paste0(y," for ",i," is done!"))
  }
  
  sst_avg_df <- rbind(sst_avg_df,annual_sst)
  
  print(paste0(i," is finished!"))
  
}


# accidently hit esc, m was "2017-01-01", on 1205

write.csv(sst_avg_df,paste0(dir$cleandata,"sst_average_2005_2023.csv"),
          row.names = F)




#### 4. Wind --------------------------------------------------------------------------------

# This is the information for the dataset for wind speed

# Name: ECMWF_ERA5_LAND_DAILY_AGGR ERA5-Land Daily Aggregated - ECMWF Climate Reanalysis
# Link: https://developers.google.com/earth-engine/datasets/catalog/
# Citation: Muñoz Sabater, J., (2019): ERA5-Land monthly averaged data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (18 June 2025), doi:10.24381/cds.68d2bb30
# Terms of Use: Please acknowledge the use of ERA5-Land as stated in the Copernicus C3S/CAMS License agreement:
# 5.1.1 Where the Licensee communicates or distributes Copernicus Products to the public, the Licensee shall inform the recipients of the source by using the following or any similar notice: 'Generated using Copernicus Climate Change Service Information [Year]'.

# 5.1.2 Where the Licensee makes or contributes to a publication or distribution containing adapted or modified Copernicus Products, the Licensee shall provide the following or any similar notice: 'Contains modified Copernicus Climate Change Service Information [Year]';

# Any such publication or distribution covered by clauses 5.1.1 and 5.1.2 shall state that neither the European Commission nor ECMWF is responsible for any use that may be made of the Copernicus Information or Data it contains.

# cadence: 1 Month
# Pixel size: 11132 meters
# Band 1: u_component_of_wind_10m
# Band 1 Unit: m/s
# Band 1 Description: Eastward component of the 10m wind. It is the horizontal speed of air moving towards the east, at a height of ten meters above the surface of the Earth, in meters per second. Care should be taken when comparing this variable with observations, because wind observations vary on small space and time scales and are affected by the local terrain, vegetation and buildings that are represented only on average in the ECMWF Integrated Forecasting System. This variable can be combined with the V component of 10m wind to give the speed and direction of the horizontal 10m wind.
# Band 2: v_component_of_wind_10m
# Band 2 Unit: m/s
# Band 2 Description: Northward component of the 10m wind. It is the horizontal speed of air moving towards the north, at a height of ten meters above the surface of the Earth, in meters per second. Care should be taken when comparing this variable with observations, because wind observations vary on small space and time scales and are affected by the local terrain, vegetation and buildings that are represented only on average in the ECMWF Integrated Forecasting System. This variable can be combined with the U component of 10m wind to give the speed and direction of the horizontal 10m wind.

# make the offices have a buffer
offices_ee <- sf_as_ee(office_sf %>% st_buffer(100000))  # 100km buffer

# Load image collection
wind_ic <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$
  filterDate("2006-01-01", "2024-12-31")

# Loop through years/months
years <- 2006:2024
months <- 1:12
results <- list()

for (y in years) {
  for (m in months) {
    cat("Processing:", y, "-", m, "\n")
    
    start <- ee$Date(sprintf("%04d-%02d-01", y, m))
    end <- start$advance(1, "month")
    
    mean_img <- wind_ic$
      filterDate(start, end)$
      mean()$
      select(c("u_component_of_wind_10m", "v_component_of_wind_10m"))
    
    # Extract values
    df <- ee_extract(
      x = mean_img,
      y = offices_ee,
      fun = ee$Reducer$mean(),
      scale = 1000,
      via = "getInfo"
    )
    
    df$year <- y
    df$month <- m
    results[[length(results) + 1]] <- df
  }
}

# Combine and save
all_wind <- bind_rows(results)
write_csv(all_wind, "data/clean data/wind_speed_all_offices_2006_2024.csv")


























# List of the offices by id
office_list <- as.list(office_points$office_id) # 171 offices

# create the df to save the data in
avg_wind_speed_df <- NULL

# Generate annual date list
years <- as.list(as.character(seq(as.Date('2005-01-01'), as.Date('2023-12-31'), by = 'year')))


# start the loop
for(i in office_list){ # i <- 2511
  
  # identify the lat and long
  long <- office_points[office_points$office_id == i,]$long
  lat <- office_points[office_points$office_id == i,]$lat
  
  # Define a point geometry (longitude, latitude), make the office point an ee object
  p <- ee$Geometry$Point(c(long,lat))
  
  # create the ee buffer (always 100 km radius)
  p_buff<- p$buffer(100000)
  
  # create an empty dataframe to save monthly values
  annual_wind_speed <- NULL
  
  # loop to get mean cloud coverage for a month
  for(y in years){ # y <- "2006-01-01"
    
    d1 <- as.Date(y)
    d2 <- as.Date(d1 %m+% years(1)-1) # the end of the year
    
    # change these into a character (it just works that way)
    d1 <- as.character(d1)
    d2 <- as.character(d2)
    
    # filter the date and clip to buffer in the wind_speed__collection
    filtered_wind_speed<- ee$ImageCollection("NOAA/CDR/SST_PATHFINDER/V53")$
      filter(ee$Filter$date(d1,d2))$
      select('wind_speed')# filter date
    
    # Function to extract daily mean wind_speed_
    extract_wind_speed <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
      
      # Reduce the region (mean temperature)
      mean_wind_speed <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = p_buff,
        scale = 8000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('wind_speed')
      
      # Return as a feature (date, wind_speed_)
      ee$Feature(NULL, list(date = date, wind_speed = mean_wind_speed))
    }
    
    # Apply function to each image
    wind_speed_features <- filtered_wind_speed$map(extract_wind_speed)
    
    # Convert to FeatureCollection and extract as table
    wind_speed_table <- ee$FeatureCollection(wind_speed_features)
    
    # Convert to R DataFrame
    wind_speed_df <- st_drop_geometry(ee_as_sf(wind_speed_table))
    
    # for those inland offices
    if(length(wind_speed_df) == 1){
      
      wind_speed_df$wind_speed <- 0
    }
    
    # organize the new dataframe
    wind_speed_df$year <- year(wind_speed_df$date)
    wind_speed_df$month <- month(wind_speed_df$date)
    
    wind_speed_df <- wind_speed_df %>%
      group_by(year,month) %>%
      summarize(avg_wind_speed = mean(wind_speed, na.rm = T))
    
    wind_speed_df$state_id <- office_points[office_points$office_id == i,]$state_id
    wind_speed_df$state <- office_points[office_points$office_id == i,]$state
    wind_speed_df$office_id <- office_points[office_points$office_id == i,]$office_id
    wind_speed_df$office <- office_points[office_points$office_id == i,]$office
    
    # save in the monthly
    annual_wind_speed <- rbind(annual_wind_speed,wind_speed_df)
    
    # print month
    print(paste0(y," for ",i," is done!"))
  }
  
  avg_wind_speed_df <- rbind(avg_wind_speed_df,annual_wind_speed)
  
  print(paste0(i," is finished!"))
  
}


# save

write.csv(avg_wind_speed_df,paste0(dir$cleandata,"avg_wind_speed_2005_2024.csv"),
          row.names = F)








#### 5. Chlorophyll concentration----------------------------------------------------------
# for non-GE: https://neo.gsfc.nasa.gov/view.php?datasetId=MY1DMM_CHLORA&year=2006
# Hu, C., Z. Lee, and B.A. Franz (2012). Chlorophyll-a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference, J. Geophys. Res., 117, C01011, doi:10.1029/2011JC007395.
# This map shows where tiny, floating plants live in the ocean. These plants, called phytoplankton, are an important part of the ocean's food chain because many animals (such as small fish and whales) feed on them. Scientists can learn a lot about the ocean by observing where and when phytoplankton grow in large numbers. Scientists use satellites to measure how much phytoplankton are growing in the ocean by observing the color of the light reflected from the shallow depths of the water. Phytoplankton contain a photosynthetic pigment called chlorophyll that lends them a greenish color. When phytoplankton grow in large numbers they make the ocean appear greenish. These maps made from satellite observations show where and how much phytoplankton were growing on a given day, or over a span of days. The black areas show where the satellite could not measure phytoplankton.
# Imagery produced by the NASA Earth Observations (NEO) in coordination with Gene Feldman and Norman Kuring, NASA Goddard Ocean Color Group.
# unit of measurement: mg/m^3
# The MODIS Chlorophyll-a product provides an estimate of the near-surface concentration of chlorophyll calculated using an empirical relationship derived from in situ measurements of chlorophyll and remote sensing reflectances (Rrs) in the blue-to-green region of the visible spectrum. The implementation is contingent on the availability of three or more sensor bands spanning the 440 - 670 nm spectral regime. The current implementation for the default chlorophyll algorithm (chlor_a) employs the standard band ratio algorithm (OCx) merged with the color index (CI) of Hu et al. (2012). As described in that paper, this refinement is restricted to relatively clear water, and the general impact is to reduce artifacts and biases in clear-water chlorophyll retrievals due to residual glint, stray light, atmospheric correction errors, and white or spectrally-linear bias errors in Rrs. As implemented, the algorithm diverges slightly from what was published in Hu et al. (2012) in that the transition between the CI and OCx now occurs at 0.15 < CI < 0.2 mg/m-3 to ensure a smooth transition.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stack the csv raster
# 1. Define list of CSVs (e.g., one per month)
csv_files <- list.files(path = paste0(dir$shp, "clorophyll/"),
                        pattern = "\\.CSV$", 
                        full.names = TRUE)
print(csv_files)
# Optional: sort files chronologically
csv_files <- sort(csv_files)

# 2. Read one CSV to get dimensions and lat/lon
sample_data <- read.csv(csv_files[1], check.names = FALSE)
lats <- sample_data[[1]]
lons <- as.numeric(colnames(sample_data)[-1])

# 3. Define raster grid
nrow_ras <- length(lats)
ncol_ras <- length(lons)

ext <- ext(min(lons), max(lons), min(lats), max(lats))

# 4. Initialize raster stack list
raster_list <- vector("list", length(csv_files))

# 5. Loop through CSVs and convert to rasters
for (i in seq_along(csv_files)) {
  data <- read.csv(csv_files[i], check.names = FALSE)
  values_matrix <- apply(data[,-1], 2, as.numeric)
  values_matrix[values_matrix == 99999.0] <- NA
  
  # terra expects values to be in column-major order (by column)
  raster_obj <- rast(nrows = nrow_ras,
                     ncols = ncol_ras,
                     ext = ext,
                     vals = as.vector(t(values_matrix))) # row-major to column-major
  names(raster_obj) <- tools::file_path_sans_ext(basename(csv_files[i])) # Name by file
  
  raster_list[[i]] <- raster_obj
}

# 6. Stack rasters into a SpatRaster
raster_stack <- rast(raster_list)

# change layer name (added later)
months <- seq.Date(as.Date("2006-01-01"),as.Date("2024-12-01"), by = "month")
names(raster_stack) <- months
names(raster_stack)

# 7 Save the raster stack
writeRaster(raster_stack,paste0(dir$shp,"chlorophyll_stack.tif"), overwrite = T)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract the data from the raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load the raster
raster_stack <- rast(paste0(dir$shp,"chlorophyll_stack.tif"))

# use the office sf and make a buffer of 100 km
office_buffers <- st_buffer(office_sf, dist = 100000)

# convert this sf object to a terra-compatible vector
office_vect <- vect(office_buffers)

# extract the mean values for clorophyll for each layer (month)
extracted_df <- terra::extract(raster_stack, office_vect, fun = mean, na.rm = TRUE)

# add the office id to the extracted df
extracted_df$office_id <- office_sf$office_id

# pivot data to get the long format
chlorophyll_long <- extracted_df %>%
  pivot_longer(cols = -office_id,
               names_to = "layer",
               values_to = "chlorophyll_conc")

# remove the row that says ID
chlorophyll_long <- chlorophyll_long[!(chlorophyll_long$layer == "ID"),]

# convert the layer names to date
chlorophyll_long$date <- as.Date(gsub("MY1DMM_CHLORA_|_rgb.*", "", chlorophyll_long$layer))

# remove the layer column
chlorophyll_long <- chlorophyll_long[,-2]

# save this in the clean data
write.csv(chlorophyll_long, paste0(dir$cleandata,"chlorophyll_concentration_mg_m3.csv"),
          row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## long loop, good for reference ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lets make a loop about it! (you could have done it by month for all offices at once)
# 
# # df to save in
# clorophyll_all_office <- NULL
# months <- seq.Date(as.Date("2006-01-01"),as.Date("2023-12-01"), by = "month")
# 
# for(i in as.list(office_points$office_id)){ # i <- 1201, ACAPULCO, THE VALUE SHOULD BE 0.3457895 IN JAN 2006
# 
#   # 1. Select the office points from the office_sf
#   point <- office_sf[office_sf$office_id == i,]$geometry
# 
#   # 2. Make the buffer for the point, 100 km radius
#   p_buff <- st_buffer(point, dist = 100000)
# 
#   # 3. Make a file to save the values the individual office
#   ind_cl <- NULL
# 
#   # 4. Make a loop for every month from 2006 to 2023, with a raster for each month
#   for(m in as.list(months)){#  m <- "2006-01-01"
# 
#     # 4.1 Read in the csv for the month
#     data <- read.csv(paste0(dir$shp,"clorophyll/MY1DMM_CHLORA_",m,"_rgb_1440x720.SS.csv"),
#                      check.names = FALSE)
# 
#     # 4.2 Extract latitudes and longitudes
#     lats <- data[[1]]
#     lons <- as.numeric(colnames(data)[-1])
# 
#     # 4.3 Extract the matrix of values and convert to numeric matrix
#     #values_matrix <- as.matrix(data[,-1])
#     values_matrix <- apply(data[,-1], 2, as.numeric)
#     values_matrix <- as.matrix(values_matrix)
#     values_matrix[values_matrix == 99999.0] <- NA  # Replace missing values
# 
#     # 4.4 Create raster
#     x_min <- min(lons, na.rm = TRUE)
#     x_max <- max(lons, na.rm = TRUE)
#     y_min <- min(lats, na.rm = TRUE)
#     y_max <- max(lats, na.rm = TRUE)
# 
#     raster1 <- raster(values_matrix,
#                       xmn = x_min, xmx = x_max,
#                       ymn = y_min, ymx = y_max,
#                       crs = crs(p_buff))
#     # 4.4.5 plot raster and buffer (for verification purposes, silence during loop)
#      plot(raster1, ylim = c(10,20), xlim = c(-110,-90))
#      plot(p_buff, add = T, border = "blue", lwd = 1)
# 
#     # 4.5 Convert sfc to terra-compatible object (including the buffer made previously)
#     buffer_vect <- vect(p_buff)
#     raster1_terra <- rast(raster1)
# 
#     # 4.6 summarize area in buffer to a single value
#     values_mean <- extract(raster1_terra, buffer_vect, fun = mean, na.rm = TRUE)
# 
#     # 4.7 Save the values
#     temp_cl <- office_points[office_points$office_id == i,]
#     temp_cl$clorophyll_conc <- values_mean$layer
#     temp_cl$date <- as.Date(m)
#     ind_cl <- rbind(ind_cl,temp_cl)
# 
#     # 4.8 print out name of month to inform how far we are
#     print(paste0(m," for ",i," is done!"))
# 
#   }
# 
#   # 5. Save all the individual values
#   clorophyll_all_office <- rbind(clorophyll_all_office,ind_cl)
# 
#   # 6. print out name of office to tell us we are done
#   print(paste0(i," is complete!"))
# 
# 
# }

# 
# # save the data!
# write.csv(clorophyll_all_office,
#           paste0(dir$cleandata,"clorophyll_concentration_all_office_mg_m3.csc"),
#           row.names = F)

#### 6. Precipitation with rate (fluctuation) --------------------------------------------------------------------------------
# why this one? It emcompasses ocean and land
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GSFC_MERRA_flx_2#terms-of-use
# MERRA-2 M2T1NXFLX: Surface Flux Diagnostics V5.12.4
# description:M2T1NXFLX (or tavg1_2d_flx_Nx) is an hourly time-averaged data collection in Modern-Era Retrospective analysis for Research and Applications version 2 (MERRA-2). This collection consists of assimilated surface flux diagnostics, such as total precipitation, bias corrected total precipitation, surface air temperature, surface specific humidity, surface wind speed, and evaporation from turbulence. The "surface" in this data collection is the model surface layer. The heights of the model surface layer (HLML) vary with time and location, with the value of ~60 meters above ground. The data field is time-stamped with the central time of an hour starting from 00:30 UTC, e.g.: 00:30, 01:30, ... , 23:30 UTC.
# Terms of Use: NASA promotes the full and open sharing of all data with research and applications communities, private industry, academia, and the general public.
# DOI: https://doi.org/10.7289/V52J68XX
# cadence 1 Hour
# pixel size: 69375 meters
# band: PRECTOT
# Y pixel size: 55000 meters
# band unit: 	kg/m^2/s
# band scale: 
# band offset: 
# band description: Total precipitation

# before we go crazy, lets see how we want our buffer to look like 
# so that we only encompass 20 km from coastline

# read in shapefile of mexico
mex_bound <- st_read(paste0(dir$shp,"MEX_adm0.shp"))
plot(mex_bound$geometry)

# now lets select an office, lets say acapulco, office id : 1201
lat <- office_points[office_points$office_id == 1201,]$lat
long <- office_points[office_points$office_id == 1201,]$long  
point <- st_sf(
  geometry = st_sfc(st_point(c(long, lat)), crs = st_crs(mex_bound))
)

# Make sure to transform to a projected CRS before buffering
point_proj <- st_transform(point) # Replace with appropriate projected CRS
point_buff <- st_buffer(point_proj, dist = 50000) # 50 km buffer
point_buff <- st_transform(point_buff, crs = st_crs(mex_bound)) # Transform back for plotting

# Plot the map with the point and buffer
bbox <- st_bbox(point_buff) # to zoom in
ggplot() +
  geom_sf(data = mex_bound, fill = "white", color = "black") +
  geom_sf(data = point, color = "red", size = 1) +
  geom_sf(data = point_buff, fill = NA, color = "blue", linetype = "dashed") +
  coord_sf(xlim = c(bbox["xmin"]-.5, bbox["xmax"]+.5),
           ylim = c(bbox["ymin"]-.5, bbox["ymax"]+.5)) +
  theme_minimal()

# now cut off 20 km from the shorline, for some reason WGS 84 does not do well
# with negative buffers, so lets use 6372 as a temporary buffer

# 1. Ensure all data uses a projected CRS (for distance accuracy)
crs_proj <- 6372 # Example CRS; replace with appropriate UTM for Mexico
buffer_proj <- st_transform(point_buff, crs_proj)
land_proj <- st_transform(mex_bound$geometry, crs_proj)
# 2. Create coastal zone: land within 20 km of the coastline
# Step 2a: Buffer the land outward by 20 km
land_plus_20km <- st_buffer(land_proj, dist = 20000)

# Step 2b: Shrink land inward by 20 km (i.e., erode)
land_minus_20km <- st_buffer(land_proj, dist = -20000)

# Step 2c: The coastal strip is the union
coastal_strip <- st_difference(land_plus_20km, land_minus_20km)
#coastal_strip <- st_difference(land_proj,land_minus_20km) # land only

# 3. Get the intersection: land within buffer AND coastal zone
coastal_buffer_area <- st_intersection(buffer_proj, coastal_strip)

# 4. Reproject back to match original CRS 
coastal_buffer_area <- st_transform(coastal_buffer_area, st_crs(mex_bound$geometry))

# 5. Plot the map with the point new buffer
bbox <- st_bbox(point_buff) # to zoom in
ggplot() +
  geom_sf(data = mex_bound, fill = "white", color = "black") +
  geom_sf(data = point, color = "red", size = 1) +
  geom_sf(data = coastal_buffer_area, fill = NA, color = "blue", linetype = "dashed") +
  coord_sf(xlim = c(bbox["xmin"]-.5, bbox["xmax"]+.5),
           ylim = c(bbox["ymin"]-.5, bbox["ymax"]+.5)) +
  theme_minimal()


# lets make a loop to do this for all the points in the 
# List of the offices by id
office_list <- as.list(office_points$office_id)
crs_proj <- 6372 # Example CRS; replace with appropriate UTM for Mexico
land_proj <- st_transform(mex_bound$geometry, crs_proj)
coastal_strip <- st_transform(coastal_strip, crs_proj)
# Create an empty geometry column (e.g., for POLYGONs)
geometry <- st_sfc(crs = crs(mex_bound))  # No geometry added yet

# Create empty attribute columns
df <- data.frame(state_id = integer(0), state = character(0),
                 office_id = integer(0), office = character(0),
                 long = integer(0), lat = integer(0))

# Combine into an empty sf object
coastal_sf_20km <- st_sf(df, geometry = geometry)

for(i in office_list){ # i <- 1201
  # extract lat and long for i
  lat <- office_points[office_points$office_id == i,]$lat
  long <- office_points[office_points$office_id == i,]$long  
  # turn it into a point
  point <- st_sf(
    geometry = st_sfc(st_point(c(long, lat)), crs = st_crs(mex_bound))
  )
  point_proj <- st_transform(point, crs = crs_proj)
  
  # make the buffer
  point_buff <- st_buffer(point_proj, dist = 50000) # 50 km buffer
  
  #Get the intersection: land within buffer AND coastal zone
  coastal_buffer_area <- st_intersection(point_buff, coastal_strip)
  
  if(length(coastal_buffer_area$geometry) == 0 ){
  
    next
  }
  
  
  #Reproject back to match original CRS 
  coastal_buffer_area <- st_transform(coastal_buffer_area, st_crs(mex_bound$geometry))
  
  # save 
  df_temp <- office_points[office_points$office_id == i,]
  df_temp$geometry <- coastal_buffer_area$geometry
  sf_df <- st_sf(df_temp, sf_column_name = "geometry")
  coastal_sf_20km <- rbind(coastal_sf_20km, sf_df)
}

# save this
st_write(coastal_sf_20km,
         paste0(dir$shp,"coastal_sf_20km.shp"),
         append = F)
st_write(coastal_sf_20km,
         paste0(dir$shp,"coastal_sf_20km_land_only.shp"),
         append = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load coastal sf
coast_sf <- st_read(paste0(dir$shp,"coastal_sf_20km.shp"))

# List of the offices by id
office_list <- as.list(office_points$office_id) # 171 offices

# create the df to save the data in
avg_prec_df <- NULL

# Generate annual date list
years <- as.list(as.character(seq(as.Date('2005-01-01'), as.Date('2023-12-31'), by = 'year')))


# start the loop
for(i in office_list){ # i <- 1201
  
  # select the sf for the coastline
  sf_obj <- coast_sf[coast_sf$office_id == i,]$geometry
  
  # make the sf object an ee object
  ee_obj <- sf_as_ee(sf_obj)

  
  # create an empty dataframe to save monthly values
  annual_wind_speed <- NULL
  
  # loop to get mean cloud coverage for a month
  for(y in years){ # y <- "2006-01-01"
    
    d1 <- as.Date(y)
    d2 <- as.Date(d1 %m+% years(1)-1) # the end of the year
    
    # change these into a character (it just works that way)
    d1 <- as.character(d1)
    d2 <- as.character(d2)
    
    # filter the date and clip to buffer in the wind_speed__collection
    filtered_prec<- ee$ImageCollection("NASA/GSFC/MERRA/flx/2")$
      filter(ee$Filter$date(d1,d2))$
      select('PRECTOT')# filter date
    
    # Function to extract daily mean wind_speed_
    extract_prec <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
      
      # Reduce the region (mean temperature)
      mean_prec <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = ee_obj,
        scale = 15000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('PRECTOT')
      
      # Return as a feature (date, wind_speed_)
      ee$Feature(NULL, list(date = date, prec = mean_prec))
    }
    
    # Apply function to each image
    prec_features <- filtered_prec$map(extract_prec)
    
    # Convert to FeatureCollection and extract as table
    prec_table <- ee$FeatureCollection(prec_features)
    
    # Convert to R DataFrame
    prec_df <- st_drop_geometry(ee_as_sf(prec_table))
    
    # for those inland offices
    if(length(prec_df) == 1){
      
      prec_df$prec <- NA
    }
    
    # organize the new dataframe
    prec_df$year <- year(prec_df$date)
    prec_df$month <- month(prec_df$date)
    
    prec_df <- prec_df %>%
      group_by(year,month) %>%
      summarize(avg_prec = mean(prec, na.rm = T))
    
    prec_df$state_id <- office_points[office_points$office_id == i,]$state_id
    prec_df$state <- office_points[office_points$office_id == i,]$state
    prec_df$office_id <- office_points[office_points$office_id == i,]$office_id
    prec_df$office <- office_points[office_points$office_id == i,]$office
    
    # save in the monthly
    annual_prec <- rbind(annual_prec,prec_df)
    
    # print month
    print(paste0(y," for ",i," is done!"))
  }
  
  avg_prec_df <- rbind(avg_prec_df,annual_prec)
  
  print(paste0(i," is finished!"))
  
}


# save

write.csv(avg_prec_df,paste0(dir$cleandata,"avg_prec_2005_2023_fluctuations_land_and_sea.csv"),
          row.names = F)





#### 6. Precipitation in meters ------------------------------------------
# look into
# https://www.sciencedirect.com/topics/engineering/wet-bulb-temperature#:~:text=Wet%20bulb%20temperature%20is%20the,to%20the%20wet%20bulb%20temperature.
# CHIRPS:  https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_PENTAD
# ERA5-land: https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY
# 
# The dataset chosen for land only!:
# ERA5-Land Monthly Averaged by Hour of Day - ECMWF Climate Reanalysis
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_BY_HOUR
# Pixel Size: 11132 meters
# Band: total_precipitation
# band unit of measurement: m
# band description: Accumulated liquid and frozen water, including rain and snow, that falls to the Earth's surface. It is the sum of large-scale precipitation (that precipitation which is generated by large-scale weather patterns, such as troughs and cold fronts) and convective precipitation (generated by convection which occurs when air at lower levels in the atmosphere is warmer and less dense than the air above, so it rises). Precipitation variables do not include fog, dew or the precipitation that evaporates in the atmosphere before it lands at the surface of the Earth. This variable is accumulated from the beginning of the forecast time to the end of the forecast step. The units of precipitation are depth in meters. It is the depth the water would have if it were spread evenly over the grid box. Care should be taken when comparing model variables with observations, because observations are often local to a particular point in space and time, rather than representing averages over a model grid box and model time step.
# citation: Muñoz Sabater, J., (2019): ERA5-Land monthly averaged data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (<date of access>), doi:10.24381/cds.68d2bb30
# collection: "ECMWF/ERA5_LAND/MONTHLY_BY_HOUR"

coast_sf <- st_read(paste0(dir$shp,"coastal_sf_20km_land_only.shp"))

# List of the offices by id
office_list <- as.list(office_points[office_points$office_id %in% coast_sf$office_id,]$office_id) # 171 offices

# create the df to save the data in
avg_prec_df <- NULL

# Generate annual date list
years <- as.list(as.character(seq(as.Date('2005-01-01'), as.Date('2023-12-31'), by = 'year')))


# start the loop
for(i in office_list[123:128]){ # i <- 2601 (this is where it ended at last)
  
  # select the sf for the coastline
  sf_obj <- coast_sf[coast_sf$office_id == i,]$geometry
  
  # make the sf object an ee object
  ee_obj <- sf_as_ee(sf_obj)
  
  # create an empty dataframe to save monthly values
  annual_prec <- NULL
  
  # loop to get mean cloud coverage for a month
  for(y in years){ # y <- "2006-01-01"
    
    d1 <- as.Date(y)
    d2 <- as.Date(d1 %m+% years(1)-1) # the end of the year
    
    # change these into a character (it just works that way)
    d1 <- as.character(d1)
    d2 <- as.character(d2)
    
    # filter the date and clip to buffer in the wind_speed__collection
    filtered_prec<- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_BY_HOUR")$
      filter(ee$Filter$date(d1,d2))$
      select('total_precipitation')# filter date
    
    # Function to extract daily mean wind_speed_
    extract_prec <- function(image) {
      # Get image date
      date <- ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd')
      
      # Reduce the region (mean temperature)
      mean_prec <- image$reduceRegion(
        reducer = ee$Reducer$mean(),
        geometry = ee_obj,
        scale = 8000,   # Adjust scale for resolution
        maxPixels = 1e13
      )$get('total_precipitation')
      
      # Return as a feature (date, wind_speed_)
      ee$Feature(NULL, list(date = date, prec = mean_prec))
    }
    
    # Apply function to each image
    prec_features <- filtered_prec$map(extract_prec)
    
    # Convert to FeatureCollection and extract as table
    prec_table <- ee$FeatureCollection(prec_features)
    
    # Convert to R DataFrame
    prec_df <- st_drop_geometry(ee_as_sf(prec_table))
    
    # for those inland offices
    if(length(prec_df) == 1){
      
      prec_df$prec <- NA
    }
    
    
    # organize the new dataframe
    prec_df$year <- year(prec_df$date)
    prec_df$month <- month(prec_df$date)
    
    prec_df <- prec_df %>%
      group_by(year,month) %>%
      summarize(avg_prec = mean(prec, na.rm = T))
    
    prec_df$state_id <- office_points[office_points$office_id == i,]$state_id
    prec_df$state <- office_points[office_points$office_id == i,]$state
    prec_df$office_id <- office_points[office_points$office_id == i,]$office_id
    prec_df$office <- office_points[office_points$office_id == i,]$office
    
    # save in the annual
    annual_prec <- rbind(annual_prec,prec_df)
    
    # print year
    print(paste0(y," for ",i," is done!"))
  }
  
  avg_prec_df <- rbind(avg_prec_df,annual_prec)
  
  print(paste0(i," is finished!"))
  
}


# save

write.csv(avg_prec_df,paste0(dir$cleandata,"avg_prec_2005_2023_fluctuations_land_and_sea.csv"),
          row.names = F)
# try this
# https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_DAILY#bands

#### HYCOM: Hybrid Coordinate Ocean Model, Water Temperature and Salinity------------------------
#https://developers.google.com/earth-engine/datasets/catalog/HYCOM_sea_temp_salinity