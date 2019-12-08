#Takes data files and sorts by trip length into bins
#This data will be used to calculate a histogram of trip length over time
overall_wd<-getwd()
wd=paste0(overall_wd,"/","Git")
output_directory="~/Desktop/Data Science/STAT515/BikeShare/Processed Data/"

library(lubridate)
library(dplyr)

filelist=list.files(wd)
data_files<-grep(".rds",filelist)
filelist<-filelist[data_files]

for (i in seq(1:length(filelist))){
  file_path<-paste0(wd,"/",filelist[i])
  print(paste("Reading file",filelist[i]))
  load(file_path)

  hist_data<-hist(monthly.data$Heading,breaks=seq(0,360,1),xlim=c(0,360))
  bins<-hist_data$breaks[1:360]
  hist_data<-as.data.frame(t(hist_data$counts))[1:360]
  hist_data$Date<-as.Date(monthly.data$Start.date[1])
  while(i==1){
    monthly_counts<-hist_data
    i=i+1
  } 
  if(i>1){
    monthly_counts<-rbind(monthly_counts,hist_data)
  }
}

monthly_counts<-monthly_counts[,c(361,seq(1,360))]
names(monthly_counts)<-c("Date",bins)
total_counts<-apply(monthly_counts[,2:ncol(monthly_counts)],2,sum)
total_counts<-cbind(as.data.frame(bins),as.data.frame(total_counts))
save(monthly_counts,total_counts,file="TripHeadingData.rds")
