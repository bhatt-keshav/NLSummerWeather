library(tidyverse)
library(lubridate)
setwd('C:/NLSummerWeather')
bilt <- read.csv('data_weer.txt')

# Rename to more meaningful col names
bilt <- bilt %>% rename(vec_mean_windsp = FHVEC, mean_windsp = FG, mean_temp = TG, 
                        min_temp = TN, max_temp = TX, 
                        sunshine_duration = SQ, precipitation_hr = DR, 
                        precipitation_mm = RH, cloud_cover = NG)

# Select relevant cols and filter after 1951 because cloud cover variable is previously unfilled
bilt_70 <-
  bilt %>% filter(YYYYMMDD > 19510101) %>% select(
    YYYYMMDD,
    mean_windsp,
    mean_temp,
    min_temp,
    max_temp,
    sunshine_duration,
    precipitation_hr,
    precipitation_mm,
    cloud_cover
  )

# Daily mean temperature in (0.1 degrees Celsius)
bilt_70$mean_temp <- bilt_70$mean_temp/10
bilt_70$min_temp <- bilt_70$mean_temp/10
bilt_70$max_temp <- bilt_70$mean_temp/10

# Convert column to date
bilt_70$YYYYMMDD <- lubridate::ymd(bilt_70$YYYYMMDD)

bilt_70 <- na.omit(bilt_70)

# Select summer months (July, Aug) only 
bilt_70_summer <- bilt_70 %>% filter(month(YYYYMMDD) %in% c(7,8))
# Select only numeric cols
bilt_70_summer_num <- bilt_70 %>% select(-YYYYMMDD)

# Filter last 30 years
bilt_30_summer <- bilt_70_summer %>% filter(year(YYYYMMDD) > 1990)
bilt_30_summer_num <- bilt_30_summer %>% select(-YYYYMMDD)

# Filter last 3 years
bilt_3_summer <- bilt_70_summer %>% filter(year(YYYYMMDD) %in% c(2019, 2020, 2021))
bilt_3_summer_num <- bilt_3_summer %>% select(-YYYYMMDD)

# Filter this year
bilt_1_summer <- bilt_70_summer %>% filter(year(YYYYMMDD) %in% 2021)
bilt_1_summer_num <- bilt_3_summer %>% select(-YYYYMMDD)

# Just find means for demonstration
colMeans(bilt_70_summer_num)
colMeans(bilt_30_summer_num)
colMeans(bilt_3_summer_num)
colMeans(bilt_1_summer_num)

### Plotting ###
# Convert means into a df
mean_70 <- data.frame(mean = colMeans(bilt_70_summer_num), variable = names(bilt_70_summer_num), history = '70j')
mean_30 <- data.frame(mean = colMeans(bilt_30_summer_num), variable = names(bilt_30_summer_num), history = '30j')
mean_3 <- data.frame(mean = colMeans(bilt_3_summer_num), variable = names(bilt_3_summer_num), history = '3j')
mean_1 <- data.frame(mean = colMeans(bilt_1_summer_num), variable = names(bilt_1_summer_num), history = '1j')

# Bind these means
bound_means <- rbind(mean_70, mean_30, mean_3, mean_1)

bound_means$history <- as.factor(bound_means$history)
levels(bound_means$history) <- c('1j', '3j', '30j' ,'70j')
bound_means$variable <- as.factor(bound_means$variable)
levels(bound_means$variable) <- c('min_temp', 'mean_temp', 'max_temp', 'mean_windsp', 'cloud_cover',
                                  'sunshine_duration', 'precipitation_hr', 'precipitation_mm')

# Plot dodged boxplots
ggplot(bound_means, aes(fill = history, y = mean, x = variable)) + 
  geom_bar(position="dodge", stat="identity") 
  

