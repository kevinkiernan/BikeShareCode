



#add coordinates to data set
bikecoords <- function(data) {
  
  library(dplyr)
  
  
  b <- data
  
  ##add coordinate data based on station location data
  b$Start_lat <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$Start_long <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  b$End_lat <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$End_long <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  return(b)
  
}