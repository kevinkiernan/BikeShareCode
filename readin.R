getwd()

library(dplyr)

dat <- data.frame()

for (i in 1:length(list.files(path = './Data/Month Data'))){
  load(paste0('Data/Month Data/',list.files(path = './Data/Month Data')[i]))
  dat <- rbind(dat, monthly.data)
  
}

##rds file of every ride combined
saveRDS(dat,"allrides.rds")

#readin all rides
allrides <- readRDS('allrides.rds')


set.seed(245)
#sample 5 million rides 
x <- allrides[sample(nrow(allrides), 5000000),]

#read in coordinate data
locations <- read.csv('Data/Capital_Bike_Share_Locations.csv', header = TRUE)

stationlocations <- select(locations, TERMINAL_NUMBER, LATITUDE, LONGITUDE)
