# Extensive / Intensive Margin by fish
# /Feb 3, 2025
# on the sections that tell you to choose, follow instructions and run the code
# Author: Jesus Felix

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

##### Choose the fish ------------------------------------------------------------------

# choose the fish category
fish <- "PULPO"

# other choices: 
# [1] "BAGRE"   -> catfish                   
# [2] "CARPA"   -> Carp                    
# [3] "LOBINA"                      
# [4] "MOJARRA"  -> its like a tilapia                   
# [5] "ALMEJA"    -> clam                  
# [6] "BAQUETA"                     
# [7] "BERRUGATA"                   
# [8] "BONITO"                      
# [9] "CABRILLA"                    
# [10] "CALAMAR"    -> squid                 
# [11] "CAPTURA SIN REGISTRO OFICIAL"  -> no official record
# [12] "CARACOL" -> snail                 
# [13] "CAZON"                       
# [14] "CORVINA"                     
# [15] "GUACHINANGO"                 
# [16] "JUREL"                       
# [17] "LENGUADO"                    
# [18] "LISA"                        
# [19] "MERO Y SIMILARES"            
# [20] "OTRAS"                       
# [21] "PAMPANO"                     
# [22] "PARGO"                       
# [23] "PULPO"                       
# [24] "RAYA Y SIMILARES"            
# [25] "SIERRA"                      
# [26] "TIBURON" --> shark!                    
# [27] "ALGAS"                       
# [28] "ERIZO"                       
# [29] "JAIBA" -> blue, freshwater crab                  
# [30] "LANGOSTA"                    
# [31] "PEPINO DE MAR"               
# [32] "SARGAZO"                     
# [33] "ABULON"                      
# [34] "ANCHOVETA"                   
# [35] "ATUN"                        
# [36] "BARRILETE"                   
# [37] "CAMARON"  -> shrimp                   
# [38] "FAUNA"                       
# [39] "MACARELA"                    
# [40] "OSTION"                      
# [41] "SARDINA"                     
# [42] "PIERNA"                      
# [43] "RONCO"                       
# [44] "BANDERA"                     
# [45] "ROBALO"                      
# [46] "BESUGO"                      
# [47] "CINTILLA"                    
# [48] "LEBRANCHA"                   
# [49] "PETO"                        
# [50] "RUBIA Y VILLAJAIBA"          
# [51] "TRUCHA"                      
# [52] "ESMEDREGAL"                  
# [53] "RUBIO"                       
# [54] "LANGOSTINO"                  
# [55] "CHARAL"                      
# [56] "OTRAS SIN REGISTRO OFICIAL"  
# [57] "PECES DE ORNATO"             
# [58] "TILAPIA"                     
# [59] "MERO"                        
# [60] "ORNATO"                      
# [61] "PEPINO "                     
# [62] "SARGAZO "                    
# [63] "FAUNA DE ACOMPANAMIENTO" 

########## data prep ------------------------------------------------------------
# load the data
# annual_pcnt_d_category_office has the annual aggregates for each category (cat)
# for each office in each year
# the percentages have been prepped to recognize the fish's proportion in the 
# office's total catch
pd <- read.csv(paste0(dir$cleandata,"annual_pcnt_pd_category_office.csv"))


# make a df from the annual_pcnt_office with only specific fish
df <- pd[pd$category == fish,]

# bring up the bienpesca data
bp <- read.csv(paste0(dir$cleandata,"contemp_bp_office_all_14_22.csv"))

# combine the two, making sure offices and years match
bp$yo_id <- paste(bp$year,bp$office_id, sep = "_") # make ids for year and office
df$yo_id <- paste(df$year,df$office_id, sep = "_")
bp <- bp[bp$yo_id %in% df$yo_id,] # remove the ids that are not within
# join them
df <- left_join(df,
                bp[,-c(5,6)], # no need for these, redundant
                by = c("year","state","state_id","office","office_id","yo_id") )

# if contemporary is NA, make into zero
df$contemporary <- ifelse(is.na(df$contemporary), 0,df$contemporary)


# we want to know how many offices the fish was found in and in what years
# make columns for each year and check what offices have that year
office_joint_freq <- NULL
for(o in as.list(unique(df$office_id))){ # o <- 101

  temp <- df[df$office_id == o,c(1:5)]

  yr_dum <- temp[1,c(2:5)]
  for(i in 2006:2022){ # i <- 2006
    df1 <- temp[1,c(2:5)]
    if(i %in% temp$year){
      dum <- 1
    }else{
      dum <- 0
    }
    df1$dum <- dum
    colnames(df1)[5] <- paste("dum_",i, sep = "_")
    yr_dum <- cbind(yr_dum,df1[5])

  }
  office_joint_freq <- rbind(office_joint_freq,yr_dum)
}
# sum across the rows
office_joint_freq$total_yrs <- rowSums(office_joint_freq[ , c(5:21)], na.rm=TRUE)


# now the offices that existed for at least 10 years (more than half of the time)
semi_office <- df[df$office_id %in%
              office_joint_freq[office_joint_freq$total_yrs > 9,]$office_id,]
length(unique(semi_office$office_id))

# all the offices that existed for half the time or less
bad_office <- df[df$office_id %in%
                           office_joint_freq[office_joint_freq$total_yrs < 10,]$office_id,]
length(unique(bad_office$office_id)) #21 offices had less than 10 years

# we can combine the offices by distance
office_sf <- st_read(paste0(dir$shp, "office_points_nov_21_2024.shp"))

# make Ciudad de Mexico , id = 1510
office_sf[office_sf$office == "CD DE MEXICO",]$office_id <- 1510
office_sf[office_sf$office == "CD DE MEXICO",]$state_id <- 15

# fill datasets for loop
# this loop will find offices that are "bad" and combine their data to a nearby
# office. The "bad" offices were not around during exposure.
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
  df2 <- n_contemp %>% # summarize has the important data from the "good" offices
    group_by(year) %>%
    summarize(tot_wt = sum(cat_total_wt), # summed weight for good offices
              tot_vl = sum(cat_total_rev), # summed revenue for good offices
              tot_contemp = sum(contemporary)) # summed contemporary for good offices
  # create a n_contemp with contemporary data for that year with the good office totals
  n_contemp <- left_join(n_contemp,df2, by = "year")
  
  # provide percentages for size, to properly distribute the "bad" office totals
  n_contemp$pcnt_wt <- n_contemp$cat_total_wt/n_contemp$tot_wt
  n_contemp$pcnt_vl <- n_contemp$cat_total_rev/n_contemp$tot_vl
  n_contemp$pcnt_sub <- n_contemp$contemporary/n_contemp$tot_contemp # sub is for subsidy
  n_contemp$pcnt_sub <- ifelse(is.na(n_contemp$pcnt_sub),0,n_contemp$pcnt_sub) # sometimes contemporary is zero
  
  # join n_contemp (good offices) and b_contemp (bad offices) by year
  n_contemp <- left_join(n_contemp,b_contemp[c("year","cat_total_wt","cat_total_rev","contemporary")], by = "year")
  
  # create the new totals for the good offices by multiplying their representive percentages by the 
  # bad office total and adding that on to the totals they already have
  n_contemp$cat_total_wt <- (n_contemp$pcnt_wt*n_contemp$cat_total_wt.y)+n_contemp$cat_total_wt.x
  n_contemp$cat_total_rev <- (n_contemp$pcnt_vl*n_contemp$cat_total_rev.y)+n_contemp$cat_total_rev.x
  n_contemp$contemporary <- (n_contemp$pcnt_sub*n_contemp$contemporary.y)+n_contemp$contemporary.x
  
  # save these to contemp_bp_semi, removing the unnecessary variables (those with x and y, repeated..)
  contemp_bp_semi <- rbind(contemp_bp_semi,n_contemp[-c(7:8,14:23)])

  # print out completed so you know how the loop is going
  print(paste0("Office ID ",i," is complete"))

}

# check for duplications, some offices may be captured more than once in the same year
length(contemp_bp_semi[duplicated(contemp_bp_semi[c(1:5)])== T,]$office_id)
# summarize them again to verify that we do not have duplicates
df2 <- contemp_bp_semi %>%
  group_by(year,state_id,state,office_id,office) %>%
  summarise(cat_total_wt = sum(cat_total_wt),
            cat_total_rev = sum(cat_total_rev),
            contemporary = sum(contemporary)
            )
# join these to the semi office that were good
df3 <- left_join(semi_office, df2, by = c("year","state_id","state","office_id","office")) %>%
  mutate(cat_total_wt = ifelse(is.na(cat_total_wt.y), cat_total_wt.x, cat_total_wt.y)) %>% 
  mutate(cat_total_rev = ifelse(is.na(cat_total_rev.y), cat_total_rev.x, cat_total_rev.y)) %>%
  mutate(contemporary = ifelse(is.na(contemporary.y), contemporary.x, contemporary.y)) %>%
  select(year,state_id,state,office_id,office, cat_total_wt,cat_total_rev,contemporary,
         office_total_wt,office_total_rev,cat_pct_wt,cat_pct_rev,yo_id)
# how many unique offices? Does it require filling gaps?
n_offices <- length(unique(df3$office_id))
n_years <- length(2006:2022)
correct_obs <- n_offices*n_years
correct_obs

# fill in the gaps, these are the for the years where there is no information 
# for the offices
filled_bp <- NULL
for(i in as.list(unique(df3$office_id))){ # i <- 410
  temp <- df3[df3$office_id == i,]

  for(y in 2006:2022){ # y <- 2014
    year <- y
    cat_total_wt <- 0
    cat_total_rev <- 0
    contemporary <- 0
    office_total_wt <- 0
    office_total_rev <- 0
    cat_pct_wt <- 0
    cat_pct_rev <- 0
    yo_id <- paste(y,i,sep = "_")
    
    if(y %not_in% temp$year){
      d <- as.data.frame(cbind(year,temp[1,c(2:5)],
                               cat_total_wt ,
                               cat_total_rev ,
                               contemporary ,
                               office_total_wt ,
                               office_total_rev ,
                               cat_pct_wt ,
                               cat_pct_rev ,
                               yo_id))
      temp <- rbind(temp,d)
    }else{

      temp <- temp
    }
    temp <- arrange(temp,year)
  }
  filled_bp <- rbind(filled_bp,temp)
}

# check the differences
check_original <- paste(df3$year,df$office_id,df$office,sep = "_")
check_new <- paste(filled_bp$year,filled_bp$office_id,filled_bp$office,sep = "_")
diff_check <- check_new[check_new %not_in% check_original]

# does it match the right number of observations?
# any values missing?
obs_missing <- correct_obs - length(filled_bp$yo_id)
obs_missing

# officially make dataset 2 (this is the combined offices)
ds_2 <- filled_bp

# rearrange
ds_2 <- arrange(ds_2, office_id,year)

# make cumulative exposure
ds_2$cumulative <- ave(ds_2$contemporary, ds_2$office_id, FUN=cumsum)

##### visualize the distribution, Make dummies for intensive/extensive -------------------------------------------
temp <- ds_2

# create a dummy that begins when treatment starts
temp$d_treated <- ifelse(temp$cumulative > 0, 1,0)

# consider a loop to see the optimal treatment levels
# make a list from 50000 to 10 million in 50,000 increments
breaks <- seq(50000,10000000,50000)


# # # # #  threshold 1: 5 untreated ## # # # # # # # # # # # # ##  # ##

for(i in breaks){ # i <- 150000
  
  # make the dummy for that cumulative level
  temp$threshold <- ifelse(temp$cumulative >= i,1,0)
  
  # length of treated, those that reached the threshold 
  # in the first year
  l_1 <- length(temp[temp$threshold == 1 & temp$year == 2014,]$office)
  
  # count the number of offices that did not reach this threshold
  # in the last year 2022
  l_2 <- length(unique(temp[temp$threshold == 0 & temp$year == 2022,]$office))
  
  # define the limits for the untreated at the beginning and at the end
  # and a top limit for the threshold
  if(l_2 > 5){ 
    break 
  }
  print(i)
}

# rename the threshold
temp$intensive_1 <- temp$threshold

# save the threshold value for the intensive

# rename the threshold by the last i by k if less than a million
if(i < 1000000){
     threshold_1 <- paste0(i/1000,"_k") 
    }else{ # if i is greater than 1 mil, then divide by 1mil and use _m
     threshold_1 <- paste0(i/1000000,"_m") 
    }

# # # # #  threshold 2: 15 untreated ## # # # # # # # # # # # # ##  # ##

for(i in breaks){ # i <- 1000000
  # make the dummy for that cumulative level
  temp$threshold <- ifelse(temp$cumulative >= i,1,0)
  
  # length of treated, those that reached the threshold 
  # in the first year
  l_1 <- length(temp[temp$threshold == 1 & temp$year == 2014,]$office)
  
  # count the number of offices that did not reach this threshold
  # in the last year 2022
  l_2 <- length(unique(temp[temp$threshold == 0 & temp$year == 2022,]$office))
  
  # define the limits for the untreated at the beginning and at the end
  if(l_2 > 15){ 
    break 
  }
  print(i)
}

# rename the threshold
temp$intensive_2 <- temp$threshold

# save the threshold value for the intensive

# rename the threshold by the last i by k if less than a million
if(i < 1000000){
  threshold_2 <- paste0(i/1000,"_k") 
}else{ # if i is greater than 1 mil, then divide by 1mil and use _m
  threshold_2 <- paste0(i/1000000,"_m") 
}

# # # # #  threshold 3: 30 untreated ## # # # # # # # # # # # # ##  # ##

for(i in breaks){ # i <- 1000000
  
  # make the dummy for that cumulative level
  temp$threshold <- ifelse(temp$cumulative >= i,1,0)
  
  # length of treated, those that reached the threshold 
  # in the first year
  l_1 <- length(temp[temp$threshold == 1 & temp$year == 2014,]$office)
  
  # count the number of offices that did not reach this threshold
  # in the last year 2022
  l_2 <- length(unique(temp[temp$threshold == 0 & temp$year == 2022,]$office))
  
  # define the limits for the untreated at the beginning and at the end
  if(l_2 > 30){ 
    break 
  }
  print(i)
}

# rename the threshold
temp$intensive_3 <- temp$threshold

# save the threshold value for the intensive

# rename the threshold by the last i by k if less than a million
if(i < 1000000){
  threshold_3 <- paste0(i/1000,"_k") 
}else{ # if i is greater than 1 mil, then divide by 1mil and use _m
  threshold_3 <- paste0(i/1000000,"_m") 
}

# # # # #  threshold 4: 50 untreated ## # # # # # # # # # # # # ##  # ##


for(i in breaks){ # i <- 1000000
  
  # make the dummy for that cumulative level
  temp$threshold <- ifelse(temp$cumulative >= i,1,0)
  
  # length of treated, those that reached the threshold 
  # in the first year
  l_1 <- length(temp[temp$threshold == 1 & temp$year == 2014,]$office)
  
  # count the number of offices that did not reach this threshold
  # in the last year 2022
  l_2 <- length(unique(temp[temp$threshold == 0 & temp$year == 2022,]$office))
  
  # define the limits for the untreated at the beginning and at the end
  if(l_2 > 50){ 
    break 
  }
  print(i)
}

# rename the threshold
temp$intensive_4 <- temp$threshold

# save the threshold value for the intensive

# rename the threshold by the last i by k if less than a million
if(i < 1000000){
  threshold_4 <- paste0(i/1000,"_k") 
}else{ # if i is greater than 1 mil, then divide by 1mil and use _m
  threshold_4 <- paste0(i/1000000,"_m") 
}


# finally drop temp$threshold
temp <- temp %>% select(-threshold)

# rename d_treated to extensive
temp <- rename(temp, "extensive" = "d_treated")

# ## alternate method: Quantiles (uncomment) # # # # # # # # # # # # # # # # # # # #
# temp <- ds_2
# 
# # create a dummy that begins when treatment starts
# temp$extensive <- ifelse(temp$cumulative > 0, 1,0)
# 
# # find quantiles for cumulative in within 2014 to 2022
# # also, no zero values
# hist(temp$cumulative)
# q <- quantile(temp[temp$year < 2023 &
#                    temp$year > 2013 &
#                    temp$cumulative != 0, ]$cumulative,
#               probs = seq(0,1,0.1),
#               na.rm = F,
#               names = F)
# q1 <- signif(q[2], digits = 3)
# q2 <- signif(q[3], digits = 3)
# q3 <- signif(q[4], digits = 3)
# q4 <- signif(q[5], digits = 3)
# 
# temp$intensive_1 <- ifelse(temp$cumulative >= q1, 1,0)
# temp$intensive_2 <- ifelse(temp$cumulative >= q2, 1,0)
# temp$intensive_3 <- ifelse(temp$cumulative >= q3, 1,0)
# temp$intensive_4 <- ifelse(temp$cumulative >= q4, 1,0)
# 


###### choose the margin  ----------------------------
# to avoid rerunning the dataset, name temp something else
d_final <- temp

# create a list to choose from
margin_list <- c("extensive","intensive_1","intensive_2","intensive_3","intensive_4")

# we can do things 2 ways: make a loop or a selection based on the uncommented


# create a loop that makes the charts for the extensive and intensive margin

# or

# choose the margin : c("extensive","intensive 1...intensive 4")
# make a dataframe that is filtered by the margin
 d_margin <- d_final[d_final$extensive == 1,] # extensive margin
# d_margin <- d_final[d_final$intensive_1 == 1,] # extensive margin
# d_margin <- d_final[d_final$intensive_2 == 1,] # extensive margin
# d_margin <- d_final[d_final$intensive_3 == 1,] # extensive margin
# d_margin <- d_final[d_final$intensive_4 == 1,] # extensive margin


# (for the charts): choose the appropriate threshold
 th <- "Extensive" # for extensive
# th <- threshold_1 # for intensive 1
# th <- threshold_2 # for intensive 2
# th <- threshold_3 # for intensive 3
# th <- threshold_4 # for intensive 4
########## Charts -----------------------------------------------------------
# Identify the year each office was treated
yr_1st_trtd <- d_margin %>% group_by(office_id) %>% 
  summarize(yr_1st = min(year))

# combine with main data
d_join <- left_join(d_final,yr_1st_trtd, by = "office_id")

# Create the pre
not_yet_pre <- d_join %>% filter(!is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(cat_total_wt)/1000)

nvr_pre <- d_join %>% filter(is.na(yr_1st) & year < 2014) %>% group_by(year) %>%
  summarize(avg_agg_k_t = mean(cat_total_wt)/1000)


# create a treated vs never treated dummy
d_join$trtd <- ifelse(is.na(d_join$yr_1st), 0,1)
nvr_ny_post <- NULL
# summarize averages per year by year of treatment
for(y in 2014:2022){# y <- 2014
  
  avg_agg_k_t <- mean(d_join[d_join$year== y & d_join$yr_1st <= y & !is.na(d_join$yr_1st),]$cat_total_wt)/1000
  avg_agg_k_ut <- mean(d_join[d_join$year== y & (d_join$yr_1st >= y|is.na(d_join$yr_1st)),]$cat_total_wt)/1000
  
  df <- cbind(y,avg_agg_k_t,avg_agg_k_ut)
  
  nvr_ny_post <- as.data.frame(rbind(nvr_ny_post,df))
  
}


# plotty time, sample size diminishes for untreated over time
# define top limit
max_y <- max(c(nvr_ny_post$avg_agg_k_t,
               nvr_ny_post$avg_agg_k_ut,
               nvr_pre$avg_agg_k_t,
               not_yet_pre$avg_agg_k_t))


plot(nvr_pre$year,
     nvr_pre$avg_agg_k_t,
     type = "l",
     lwd = 2,
     main = paste0("Conditional on ",th,": ",fish),
     xlab = "year",
     ylab = "Avg. wt (1000 kg)",
     xlim = c(2006,2022),
     ylim = c(0,max_y),
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
       cex = .5,
       lwd = c(1,2),
       col = c('black',"steelblue","gray","darkred"),
       lty = c(1,3,1,4))
abline(v = 2014,col = "red")



