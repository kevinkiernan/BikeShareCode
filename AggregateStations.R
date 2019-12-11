#Takes data files and sorts by trip length into bins
#This data will be used to calculate a histogram of trip length over time
overall_wd<-getwd()
wd=paste0(overall_wd,"/","Git")

library(lubridate)
library(dplyr)

filelist=list.files(wd)
data_files<-grep("2016",filelist)
filelist<-filelist[data_files]

for (i in seq(1:length(filelist))){
  T0<-Sys.time()
  file_path<-paste0(wd,"/",filelist[i])
  print(paste("Reading file",filelist[i]))
  load(file_path)
#Reshape file wide -> long wrt station ID
  monthly.data_long<-monthly.data %>% gather(key="Trip.Type",value="Station.ID",Start.station.number,End.station.number)
  monthly.data_long$Trip.Type<-as.factor(monthly.data_long$Trip.Type)
  levels(monthly.data_long$Trip.Type)<-c("Arrival","Departure")
  monthly.data_long<-monthly.data_long %>% mutate(DTG=if_else(Trip.Type==1,Start.date,End.date)) %>% 
    mutate(Lat=if_else(Trip.Type==1,Start.station.lat,End.station.lat)) %>%
    mutate(Long=if_else(Trip.Type==1,Start.station.long,End.station.long)) %>%
    select(DTG,Station.ID,Trip.Type,Lat,Long) %>%
    mutate(Year=year(DTG)) %>% mutate(Month=month(DTG)) %>% mutate(Day=day(DTG)) %>%
    mutate(Group=paste0(substr(DTG,12,15), "0")) %>% group_by(Station.ID,Year,Month,Day,Group)
  
#Tally arrivals and departures by Station ID and Time
  monthly.tallies<-monthly.data_long%>% group_by(Station.ID,Group) %>% summarize(Arrivals=sum(Trip.Type=="Arrival"),Departures=sum(Trip.Type=="Departure"))
 
  rm(list=c("arrive.tallies","depart.tallies","monthly.data","monthly.data_long"))
  
  #Build running total df  
  while(i==1){
    tallies<-monthly.tallies
    i=i+1
  }
  if(i>1){
    tallies<-rbind(tallies,monthly.tallies)
    tallies<-tallies%>% group_by(Station.ID,Group) %>% summarize(Arrivals=sum(Arrivals),Departures=sum(Departures))
  }
  
#Output step time
T1<-Sys.time()
d<-T1-T0
print(d)
}

#For each time period, compute Total Daily Arrivals, Total Daily Departure, Bike Flux, Total Usage
IDs<-unique(monthly.tallies$Station.ID)
for (i in seq(1:length(IDs))){
  dat<-filter(tallies,Station.ID==IDs[i])
  print(paste0("Processing Station ID #",dat$Station.ID[1]))
  dat<-dat %>% mutate(Total.Arrivals=cumsum(Arrivals)) %>% mutate(Total.Departures=cumsum(Departures))
  while(i==1){
    final.tallies<-dat
    i=i+1
  } 
  if(i>1){
    final.tallies<-rbind(final.tallies,dat)
  }
}
final.tallies$Flux=final.tallies$Arrivals-final.tallies$Departures
final.tallies$Net.Flux=final.tallies$Total.Arrivals-final.tallies$Total.Departures
final.tallies$Total.Usage=final.tallies$Total.Arrivals+final.tallies$Total.Departures
save(final.tallies,file="tallies.rds")
