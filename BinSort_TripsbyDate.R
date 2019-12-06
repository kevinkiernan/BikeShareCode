#Takes data files and sorts by trip length into bins
#This data will be used to calculate a histogram of trip length over time
#wd="~/Desktop/Data Science/STAT515/BikeShare/Git"
overall_wd<-getwd()
wd=paste0(overall_wd,"/","Git")
#output_directory="~/Desktop/Data Science/STAT515/BikeShare/Processed Data/"

library(lubridate)
library(dplyr)

filelist=list.files(wd)
data_files<-grep(".rds",filelist)
filelist<-filelist[data_files]

for (i in seq(1:length(filelist))){
  file_path<-paste0(wd,"/",filelist[i])
  print(paste("Reading file",filelist[i]))
  load(file_path)
  monthly.data$Date<-as.Date(monthly.data$Start.date)
  
  daily_counts_members<-monthly.data %>% filter(Member.type=="Member") %>% group_by(Date) %>% summarise(frequency = n())
  daily_counts_casual<-monthly.data %>% filter(Member.type=="Casual") %>% group_by(Date) %>% summarise(frequency = n())
  daily_counts<-merge(daily_counts_members,daily_counts_casual,by="Date")
  names(daily_counts)<-c("Date","Member.rides","Casual.rides")
  
  if(i>1){
    ridership<-rbind(ridership,daily_counts)
  }
  while(i==1){
    ridership<-daily_counts
    i=i+1
  } 
}

save(ridership,file="RidershipData.rds")
