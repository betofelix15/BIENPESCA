# Step 0. R & Data Preliminaries -- (e.g. working directories, packages, functions) --- ### ----------


# this line cleans the environment, free up memory.
rm(list = ls())


### Packages (load all of them at the beginning)

# This line creates a vector of names of packages that you want to use.
want <- c("purrr","lattice","terra","foreign","RColorBrewer","rgdal", 
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



##### lOAD DATA ---------------------------------------------------------------

y <- 2011

files <- list.files(path = paste0(dir$rawdata, "RLMSEP/RLMSEP_",y),
                    pattern = "*.csv", full.names = T)

combined_data <- files %>%
    lapply(function(f) 
      read_csv(f, locale = locale(encoding = "Latin1"))) %>%
  bind_rows()


# if puerto base is NA in one column, use the other (it made two columns with the same name)
combined_data$`Puerto Base` <- ifelse(is.na(combined_data$`Puerto Base`),
                                      combined_data$PuertoBase, combined_data$`Puerto Base`)

# check for NA's
combined_data[is.na(combined_data$`Puerto Base`),]

# make the date column a date format
d_y <- combined_data

d_y$Fecha <- dmy_hm(d_y$FechaRecepcionUnitrac)

# make a month, day, year, hour column
d_y$month <- month(d_y$Fecha)
d_y$year <- year(d_y$Fecha)
d_y$day  <- day(d_y$Fecha)


# provide the hours change for each row
d_y <- d_y %>%
  group_by(RNP,year,month) %>%
  mutate(
    lag_time = lag(Fecha),
    time_diff_hr = as.numeric(difftime(Fecha, lag_time, units = "hours")),
    time_diff_hr = if_else(is.na(time_diff_hr), 0, time_diff_hr)
  ) %>%
    ungroup()



# create a column for distance traveled: https://www.calculatorsconversion.com/es/calculo-de-rumbo-y-distancia-entre-dos-coordenadas/
# using Fórmula Haversine
# Distancia = 2 × R × asin(√( sin²((lat2 – lat1)/2) + cos(lat1) × cos(lat2) × sin²((long2 – long1)/2) ))
#Radio de la Tierra (R): Valor promedio, habitualmente 6.371 km, aunque puede variar según el modelo esférico o elipsoidal.
# important to convert lat/long to radians: coordinat* (pi/180)
# practice: (31.2304° N, 121.4737° E) to (1.3521° N, 103.8198° E)
R <- 6371.2
lat1 <- 31.2304*(pi/180) 
lat2 <- 1.3521*(pi/180) 
long1 <- 121.4737*(pi/180) 
long2 <- 103.8198*(pi/180) 

distance <- 2*R*asin(sqrt(sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2 ))

# but luckily there is a package for this!
dis_hav <- distHaversine(c(121.4737,31.2304),c(103.8198,1.3521))/1000

# do this for each RNP
d_y2 <- d_y %>%
  group_by(RNP, year, month) %>%
  mutate(
    lag_lat = lag(Latitud),
    lag_long = lag(Longitud),
    distance_m = if_else(
      is.na(lag_lat),
      0,  # first row has no previous point
      distHaversine(
        cbind(lag_long, lag_lat),
        cbind(Longitud, Latitud)
      )
    )
  ) %>%
  ungroup()


# summarize such that monthly, avg_speed, max_speed, min_speed, hours, and first latitude/longitude, and puerto base is collected for each RNP

d_sum2 <- d_y2 %>%
  group_by(RNP,year,month) %>%
  summarize(
    tot_distance_km = sum(distance_m, na.rm = T)/1000,
    tot_hrs = sum(time_diff_hr, na.rm = T),
    avg_speed = mean(Velocidad, na.rm = T),
    max_speed = max(Velocidad, na.rm = T),
    min_speed = min(Velocidad, na.rm = T),
    base_port = first(`Puerto Base`),
    first_lat = first(Latitud),
    first_long = first(Longitud)
    #, .groups =  "drop"     # optional: ungroup the result
  )


# exort so you don't have to do this again
write.csv(d_sum2,
          paste0(dir$cleandata,"fishing_effort_",y,".csv"), 
          row.names = F)



# what if the area traveled is outside of the 50 meters?
# use the example of one RNP

# load office st
office_sf <- st_read(
  paste0(dir$shp, "office_points_latest.shp"))

colnames(office_sf) <- c( "state_id",  "state",    "office_id",  "office",   "locality",  "cvegeo",   "status",   "stat_abr",  "mun_id",  
                         "local_id",  "climate",  "latitud", "longitud",  "altitud",  "letter_id",  "population",  "male_pop",  "feml_pop", 
                         "opccupied_households",  "obs_id",   "municipio",  "geometry")

# 11189, 21899, 8656


i <- 8656
for(m in 1:12){

month <- m
d_test <- d_y[d_y$RNP == i & d_y$month == month,]
if (nrow(d_test) == 0) next
trajectory_sf <- st_as_sf(d_test, coords = c("Longitud","Latitud"), crs = crs(office_sf) )

point1 <- trajectory_sf[1,]


# Compute distance matrix
dist_matrix <- st_distance(point1, office_sf)

# Find index of nearest point for each point in point1
nearest_indices <- apply(dist_matrix, 1, which.min)

# Retrieve coordinates of nearest points
nearest_offices <- office_sf[nearest_indices,]

# make the buffer for this office
radius <- 1000*(50) # how many kilometers? we chose 50 
buffer <- st_buffer(nearest_offices, radius)



# identify what is outside
outside <- st_difference(trajectory_sf,st_union(buffer))

# visualize
#  Get Mexico boundaries
mexico <- ne_countries(scale = "medium", country = "Mexico", returnclass = "sf")


# for a better zoom
# Get buffer centroid
center <- st_coordinates(st_centroid(buffer))

# Define limits (e.g., ±2 degrees around center)
lon0 <- center[1]
lat0 <- center[2]

lon_range <- c(lon0 - 4, lon0 + 4)
lat_range <- c(lat0 - 4, lat0 + 4)


# Plot
plot_track <- ggplot() +
  geom_sf(data = mexico, fill = "antiquewhite") +
  geom_sf(data = buffer, fill = "lightblue", alpha = 0.3, color = "blue") +
  geom_sf(data = trajectory_sf, color = "darkgreen", size = .02) +
  geom_sf(data = outside, color = "red", size = .05) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
  labs(title = paste0("Trajectory near ", trajectory_sf$`Puerto Base`),
       subtitle = 
       paste0("RNP: ",i, ", Year: ",y,", month: ",month,
              ". Red segments are outside the buffer area"),
       x = "Longitude", y = "Latitude") +
  theme_minimal()



# if you want to use a line instead of dots

track_line <- trajectory_sf %>%
  arrange(RNP, Fecha) %>%                   # ensure correct order
  group_by(RNP) %>%
  summarise(do_union = FALSE) %>%         # prevent merging separate tracks
  st_cast("LINESTRING")

outside_line <- st_difference(track_line, st_union(buffer))

# Plot track line
track_line <- ggplot() +
  geom_sf(data = mexico, fill = "antiquewhite") +
  geom_sf(data = buffer, fill = "lightblue", alpha = 0.3, color = "blue") +
  geom_sf(data = track_line, color = "darkgreen", size = .01) +
  geom_sf(data = outside_line, color = "red", size = .01) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
  labs(title = paste0("Trajectory near ", trajectory_sf$`Puerto Base`),
       subtitle = 
         paste0("RNP: ",i, ", Year: ",y,", month: ",month,
                ". Red segments are outside the buffer area"),
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# view it
track_line

# save it
ggsave(filename = paste0(dir$fig, "satelite_monitor_",i,"_",y,"_",month,".png"),
       plot = track_line,
       width = 6,
       height = 6,
       bg = "white")

}



# can we make a map that shows traffic for the whole year, using identifiers for season?
# can we aggregate 

