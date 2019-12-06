#Adds Lat/Long, Distance in Miles, and Trip Heading to data files
wd="~/Desktop/Data Science/STAT515/BikeShare/Git"
output_directory="~/Desktop/Data Science/STAT515/BikeShare/Processed Data/"

library(lubridate)
library(dplyr)
library(geosphere)
library(sp)

filelist=list.files(wd)
locations_path<-paste0(wd,"/","Capital_Bike_Share_Locations.csv")
locations<-read.csv(file=locations_path)
locations<-locations[3:6]

for (i in seq(1:length(filelist))){
  file_path<-paste0(wd,"/",filelist[i])
  print(paste("Reading file",file_path))
  load(file_path)
  monthly.data<-monthly.data %>% na.omit()
  trip.starts<-select(monthly.data,Start.station.lat,Start.station.long)
  trip.ends<-select(monthly.data,End.station.lat,End.station.long)
  #Generate SpatialPoints objects for each trip
  coordinates(trip.starts)<-~Start.station.lat+Start.station.long
  coordinates(trip.ends)<-~End.station.lat+End.station.long
  
  #compute distances and headings
  monthly.data$Distance<-distGeo(trip.starts, trip.ends, a=6378137, f=1/298.257223563)/1609 #distance in miles
  monthly.data$Heading<-bearing(trip.starts, trip.ends, a=6378137, f=1/298.257223563) #heading in degrees
  monthly.data$Heading<-ifelse(monthly.data$Heading<0,360+monthly.data$Heading,monthly.data$Heading)

  #Save the file
  save(monthly.data,file=file_path)
}
