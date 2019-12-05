##bearings

library(lubridate)
library(fossil)
library(dplyr)

load(paste0('Data/Month Data/',list.files(path = './Data/Month Data')[10]))



locations <- read.csv('Data/Capital_Bike_Share_Locations.csv', header = TRUE)

stationlocations <- select(locations, TERMINAL_NUMBER, LATITUDE, LONGITUDE)



bikebearing <- function(bdata, round = TRUE){
  
  library(fossil)
  
  b <- bdata
  
  ##add coordinate data based on station location data
  b$Start_lat <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$Start_long <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  b$End_lat <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$End_long <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  
  #calculate bearings
  bearings <- mapply(earth.bear,b$Start_long, b$Start_lat, b$End_long, b$End_lat)
  
  if (round == TRUE) {
    return(round(bearings,0))
  }
  else {
    return (bearings)
  }
  
}


library(plyr)



dd <- bikebearing(x)

melt(plotdat, id.vars = dd)

plotdat <- as.data.frame(table(dd))
plotdat2 <- rbind(plotdat, rep(0, 7288), rep(360, 7288))




###MAKE A RADAR PLOT
library(fmsb)

radarchart(plotdat)
