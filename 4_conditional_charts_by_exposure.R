# Title: 4: Conditional Charts by exposure
# Purpose: Analyze possible outcome variables and combine them with BP data
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

# remove scientific notation
options(scipen = 999)

###### CONDITIONAL harvest BASED ON Extensive ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% filter(contemporary == cumulative & contemporary != 0)
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
d_2 %>% filter(year == 2022 & cumulative == 0)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd[,c(4,15)], by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"trtd_not_yet_never_trtd.png"), width = 700, height = 500)
plot(nvr_pre$year,
       nvr_pre$avg_agg_k_t,
       type = "l",
       lwd = 2,
       main = "Conditional on Extensive",
       xlab = "year",
       ylab = "Avg. wt (1000 kg)",
       xlim = c(2006,2022),
       ylim = c(0,70000),
       col = "black")
lines(not_yet_pre$year,
        not_yet_pre$avg_agg_k_t,
        lty = 3,
        lwd = 2,
        col = "steelblue")
lines(nvr_ny_post$y,
        nvr_ny_post$avg_agg_k_ut,
        col = "gray",
        lty = 1)
lines(nvr_ny_post$y,
        nvr_ny_post$avg_agg_k_t,
        col = "darkred",
        lwd =2,
        lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
         "topleft",
         border = F,
         cex = 1,
         lwd = c(1,2),
         col = c('black',"steelblue","gray","darkred"),
         lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()


# calculate rate
rate_ny <- not_yet_pre$avg_agg_k_t - not_yet_pre$avg_agg_k_t[]

# measure difference in rate


  
###### CONDITIONAL harvest BASED ON Reaching 100K ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_100k == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_100k == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_treatment_100k.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$avg_agg_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional on 100k Treatment",
     xlab = "year",
     ylab = "Avg. wt (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,70000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$avg_agg_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()
  


###### CONDITIONAL harvest BASED ON Reaching 500K ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_500k == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_500k == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_treatment_500k.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$avg_agg_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional on 500k Treatment",
     xlab = "year",
     ylab = "Avg. wt (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,70000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$avg_agg_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()

###### CONDITIONAL harvest BASED ON Reaching 2 million (2m) ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_2m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_2m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_treatment_2m.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$avg_agg_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional on 2 Mil Treatment",
     xlab = "year",
     ylab = "Avg. wt (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,70000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$avg_agg_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()



###### CONDITIONAL harvest BASED ON Reaching 5 million (5m) ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_5m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_5m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(agg_wt_unloaded)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_treatment_5m.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$avg_agg_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional on 5 Mil Treatment",
     xlab = "year",
     ylab = "Avg. wt (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,120000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$avg_agg_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()




###### CONDITIONAL revenue BASED ON Extensive ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% filter(contemporary == cumulative & contemporary != 0)
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
d_2 %>% filter(year == 2022 & cumulative == 0)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd[,c(4,15)], by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_mx_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_value_mx)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_value_mx)/1000
  
  df <- cbind(y,agg_mx_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"trtd_not_yet_never_trtd_revenue.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_mx_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Revenue based on Extensive",
     xlab = "year",
     ylim = c(0,400000),
     ylab = "Avg. Revenue (1000 Pesos)",
     xlim = c(2006,2022),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_mx_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_mx_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()


# calculate rate
rate_ny <- not_yet_pre$agg_mx_k_t - not_yet_pre$agg_mx_k_t[]

# measure difference in rate



###### CONDITIONAL revenue BASED ON Reaching 100K ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_100k == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_100k == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_mx_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_value_mx)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_value_mx)/1000
  
  df <- cbind(y,agg_mx_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_revenue_treatment_100k.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_mx_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Revenue: 100k treatment",
     xlab = "year",
     ylim = c(0,400000),
     ylab = "Avg. Revenue (1000 Pesos)",
     xlim = c(2006,2022),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_mx_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_mx_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()



###### CONDITIONAL revenue BASED ON Reaching 500K ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_500k == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_500k == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_mx_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_value_mx)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_value_mx)/1000
  
  df <- cbind(y,agg_mx_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_revenue_treatment_500k.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_mx_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Revenue: 500k treatment",
     xlab = "year",
     ylim = c(0,400000),
     ylab = "Avg. Revenue (1000 Pesos)",
     xlim = c(2006,2022),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_mx_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_mx_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()

###### CONDITIONAL revenue BASED ON Reaching 2 million (2m) ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_2m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_2m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_mx_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_value_mx)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_value_mx)/1000
  
  df <- cbind(y,agg_mx_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_revenue_treatment_2m.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_mx_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Revenue: 2M treatment",
     xlab = "year",
     ylim = c(0,500000),
     ylab = "Avg. Revenue (1000 Pesos)",
     xlim = c(2006,2022),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_mx_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_mx_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()



###### CONDITIONAL revenue BASED ON Reaching 5 million (5m) ----------------------------------------------

## USING DATASET 2 (COMBINED OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "bienpesca_binary_exposure_dataset2.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_5m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated
d_2 %>% filter(c_5m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_mx_k_t = mean(agg_value_mx)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_mx_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_value_mx)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_value_mx)/1000
  
  df <- cbind(y,agg_mx_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"conditional_revenue_treatment_5m.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_mx_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Revenue: 5M treatment",
     xlab = "year",
     ylim = c(0,500000),
     ylab = "Avg. Revenue (1000 Pesos)",
     xlim = c(2006,2022),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_mx_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_mx_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()





###### CONDITIONAL Shrimp harvest BASED ON Extensive ----------------------------------------------

## USING DATASET 2 (combined OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "binary_exposure_dataset2_shrimp.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% filter(contemporary == cumulative & contemporary != 0)
yr_1st_trtd$yr_1st <- yr_1st_trtd$year

# identify office never treated
d_2 %>% filter(year == 2022 & cumulative == 0) #none

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd[,c(4,15)], by = "office_id")

# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_shrimp_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded_shrimp)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded_shrimp)/1000
  
  df <- cbind(y,agg_shrimp_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}

# extensive in shrimp will not work
# # plotty time, sample size diminishes for untreated over time
# png(paste0(dir$fig,"extensive_conditional_shrimp_catch.png"), width = 700, height = 500)
# plot(nvr_pre$year,
#      nvr_pre$agg_shrimp_k_t,
#      type = "l",
#      lwd = 2,
#      main = "Conditional Shrimp Catch based on Extensive",
#      xlab = "year",
#      ylab = "Avg. Revenue (1000 Pesos)",
#      xlim = c(2006,2022),
#      col = "black")
# lines(not_yet_pre$year,
#       not_yet_pre$agg_shrimp_k_t,
#       lty = 3,
#       lwd = 2,
#       col = "steelblue")
# lines(nvr_ny_post$y,
#       nvr_ny_post$avg_agg_k_ut,
#       col = "gray",
#       lty = 1)
# lines(nvr_ny_post$y,
#       nvr_ny_post$agg_shrimp_k_t,
#       col = "darkred",
#       lwd =2,
#       lty = 4)
# legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
#        "topleft",
#        border = F,
#        cex = 1,
#        lwd = c(1,2),
#        col = c('black',"steelblue","gray","darkred"),
#        lty = c(1,3,1,4))
# abline(v = 2014,col = "red")
# dev.off()


###### CONDITIONAL Shrimp harvest BASED ON 1m ----------------------------------------------

## USING DATASET 2 (combined OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "binary_exposure_dataset2_shrimp.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_1m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated, 2 offices
d_2 %>% filter(c_1m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")



# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_shrimp_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded_shrimp)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded_shrimp)/1000
  
  df <- cbind(y,agg_shrimp_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}

# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"1m_conditional_shrimp_catch.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_shrimp_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Shrimp Catch based on 1M",
     xlab = "year",
     ylab = "Avg. Harvest (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,5000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_shrimp_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_shrimp_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()



###### CONDITIONAL Shrimp harvest BASED ON 2m ----------------------------------------------

## USING DATASET 2 (combined OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "binary_exposure_dataset2_shrimp.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_2m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated, 3 offices
d_2 %>% filter(c_2m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")



# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_shrimp_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded_shrimp)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded_shrimp)/1000
  
  df <- cbind(y,agg_shrimp_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}

# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"2m_conditional_shrimp_catch.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_shrimp_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Shrimp Catch based on 2M",
     xlab = "year",
     ylab = "Avg. Harvest (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,7000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_shrimp_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_shrimp_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()

###### CONDITIONAL Shrimp harvest BASED ON 50m ----------------------------------------------

## USING DATASET 2 (combined OFFICES)
d_2 <- read.csv(paste0(dir$cleandata, "binary_exposure_dataset2_shrimp.csv"))

# Identify the year each office was treated
yr_1st_trtd <- d_2 %>% group_by(office_id) %>% filter(c_50m == 1) %>%
  summarize(yr_1st = min(year))

# identify office never treated, 39 offices
d_2 %>% filter(c_50m == 0 & year == 2022)

# combine with main data
d_2 <- left_join(d_2,yr_1st_trtd, by = "office_id")



# Create the pre
not_yet_pre <- d_2 %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)

nvr_pre <- d_2 %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(agg_shrimp_k_t = mean(agg_wt_unloaded_shrimp)/1000)


# create a treated vs never treated dummy
d_2$trtd <- ifelse(is.na(d_2$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  agg_shrimp_k_t <- mean(d_2[d_2$year== y & d_2$yr_1st <= y & !is.na(d_2$yr_1st),]$agg_wt_unloaded_shrimp)/1000
  avg_agg_k_ut <- mean(d_2[d_2$year== y & (d_2$yr_1st >= y|is.na(d_2$yr_1st)),]$agg_wt_unloaded_shrimp)/1000
  
  df <- cbind(y,agg_shrimp_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}

# plotty time, sample size diminishes for untreated over time
png(paste0(dir$fig,"50m_conditional_shrimp_catch.png"), width = 700, height = 500)
plot(nvr_pre$year,
     nvr_pre$agg_shrimp_k_t,
     type = "l",
     lwd = 2,
     main = "Conditional Shrimp Catch based on 50M",
     xlab = "year",
     ylab = "Avg. Harvest (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,20000),
     col = "black")
lines(not_yet_pre$year,
      not_yet_pre$agg_shrimp_k_t,
      lty = 3,
      lwd = 2,
      col = "steelblue")
lines(nvr_ny_post$y,
      nvr_ny_post$avg_agg_k_ut,
      col = "gray",
      lty = 1)
lines(nvr_ny_post$y,
      nvr_ny_post$agg_shrimp_k_t,
      col = "darkred",
      lwd =2,
      lty = 4)
legend(legend= c("Never Trtd","Not Yet Trtd","Never & not yet trtd", "Treated"),
       "topleft",
       border = F,
       cex = 1,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")
dev.off()
