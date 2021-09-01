library(tidyverse)
library(lubridate)
setwd('C:/Users/M64H098/repo/zomer')
bilt <- read.csv('data_weer.txt')

bilt <- bilt %>% rename(vec_mean_windsp = FHVEC, mean_windsp = FG, mean_tem = TG, 
                        min_tem = TN, max_tem = TX, 
                        sunshine_duration = SQ, precipitation_hr = DR, 
                        precipitation_mm = RH, cloud_cover = NG)

# Selected because NG is not NA here
bilt_70 <-
  bilt %>% filter(YYYYMMDD > 19510101) %>% select(
    YYYYMMDD,
    vec_mean_windsp,
    mean_windsp,
    mean_tem,
    min_tem,
    max_tem,
    sunshine_duration,
    precipitation_hr,
    precipitation_mm,
    cloud_cover
  )

# Daily mean temperature in (0.1 degrees Celsius)
bilt_70$mean_tem <- bilt_70$mean_tem/10
bilt_70$min_tem <- bilt_70$mean_tem/10
bilt_70$max_tem <- bilt_70$mean_tem/10

# Convert column to date
bilt_70$YYYYMMDD <- lubridate::ymd(bilt_70$YYYYMMDD)
bilt_70 <- na.omit(bilt_70)

# Select summer months only
bilt_70_summer <- bilt_70 %>% filter(month(YYYYMMDD) %in% c(7,8))
bilt_70_summer_num <- bilt_70 %>% select(-c(YYYYMMDD, vec_mean_windsp))

# Filter last 30 years
bilt_30_summer <- bilt_70 %>% filter(year(YYYYMMDD) > 1990)
bilt_30_summer_num <- bilt_30_summer %>% select(-c(YYYYMMDD, vec_mean_windsp))

# Filter last 3 years
bilt_3_summer <- bilt_70 %>% filter(year(YYYYMMDD) %in% c(2019, 2020, 2021))
bilt_3_summer_num <- bilt_3_summer %>% select(-c(YYYYMMDD, vec_mean_windsp))
  
colMeans(bilt_70_summer_num)
colMeans(bilt_30_summer_num)
colMeans(bilt_3_summer_num)


