#read in coordinate data
locations <- read.csv('Data/Capital_Bike_Share_Locations.csv', header = TRUE)

stationlocations <- select(locations, TERMINAL_NUMBER, LATITUDE, LONGITUDE)
