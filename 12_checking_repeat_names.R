library(readxl)
BP_2020_2023 <- read_excel("data/raw data/BIENPESCA_2020_2023.xlsx")
View(BP_2020_2023)

# check repeated names
BP_2020_2023$full_name_mun <- paste(BP_2020_2023$NOMBRES,BP_2020_2023$`APELLIDO PATERNO`,
                                    BP_2020_2023$`APELLIDO MATERNO`,BP_2020_2023$`NOMBRE MUNICIPIO`,sep = ",")

# how many times each name is repeated
library(dplyr)
library(tidyverse)
names <- BP_2020_2023 %>%
      group_by(full_name_mun) %>%
      summarise(repeats = n())
summary(names$repeats)
