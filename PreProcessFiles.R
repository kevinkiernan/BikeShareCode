#Takes data files and saves them in a common monthly format
#This will be params we add to the function, eventually
directory="~/Desktop/Data Science/STAT515/BikeShare/"
output_directory="~/Desktop/Data Science/STAT515/BikeShare/RDS Files/"
subdirectory="Monthly Data"
file.date.range="monthly" #Choices are "quarterly","monthly","yearly"

library(lubridate)
library(dplyr)

wd=paste0(directory,subdirectory)
filelist=list.files(wd)

if (file.date.range == "quarterly"){
  print("File type is Quarterly")
  for (i in seq(1:length(filelist))){
    file_path<-paste0(wd,"/",filelist[i])
    print(paste("Reading file",filelist[i]))
    file_path<-paste0(wd,"/",filelist[i])
    df<-read.csv(file=file_path)
  
    df$Start.date<-as.POSIXct(df$Start.date,format="%Y-%m-%d %H:%M:%OS")
    df$End.date<-as.POSIXct(df$End.date,format="%Y-%m-%d %H:%M:%OS")
    df$Month<-as.factor(month(df$Start.date))
    
    #Convert times to minutes (do bc default is seconds and 1st minute is not tracked)
    df$Duration<-round(df$Duration/60,1)
  
    #Create strings for file names
    first_part<-substr(filelist[i],1,4)
    last_part<-substr(filelist[i],7,nchar(filelist[1])-4)
    
    for (j in seq(1:length(levels(df$Month)))){
      if(nchar(levels(df$Month)[j])==1){
        middle_part<-paste0("0",levels(df$Month)[j])
      } else{
        middle_part<-paste0(levels(df$Month)[j])
      }
      monthly.data<-df %>% filter(Month==levels(df$Month)[j])
      output_file_name<-paste0(first_part,middle_part,last_part,".rds")
      output_file_name_path<-paste0(output_directory,output_file_name)
      print(paste("Saving",output_file_name_path))
      save(monthly.data,file=output_file_name_path)
    }
  }
  rm(df)
} else if(file.date.range == "yearly"){
    print("File type is Yearly")
    for (i in seq(1:length(filelist))){
      file_path<-paste0(wd,"/",filelist[i])
      print(paste("Reading file",filelist[i]))
      file_path<-paste0(wd,"/",filelist[i])
      df<-read.csv(file=file_path)
      
      df$Start.date<-as.POSIXct(df$Start.date,format="%Y-%m-%d %H:%M:%OS")
      df$End.date<-as.POSIXct(df$End.date,format="%Y-%m-%d %H:%M:%OS")
      df$Month<-as.factor(month(df$Start.date))
      
      #Convert times to minutes (do bc default is seconds and 1st minute is not tracked)
      df$Duration<-round(df$Duration/60,1)
      
      #Create strings for file names
      first_part<-substr(filelist[i],1,4)
      last_part<-substr(filelist[i],5,nchar(filelist[1])-4)
      
      for (j in seq(1:length(levels(df$Month)))){
        if(nchar(levels(df$Month)[j])==1){
          middle_part<-paste0("0",levels(df$Month)[j])
        } else{
          middle_part<-paste0(levels(df$Month)[j])
        }
        monthly.data<-df %>% filter(Month==levels(df$Month)[j])
        output_file_name<-paste0(first_part,middle_part,last_part,".rds")
        output_file_name_path<-paste0(output_directory,output_file_name)
        print(paste("Saving",output_file_name_path))
        save(monthly.data,file=output_file_name_path)
      }
    }
    rm(df)
  } else if(file.date.range == "monthly"){
    print("File type is Monthly")
    for (i in seq(1:length(filelist))){
      file_path<-paste0(wd,"/",filelist[i])
      print(paste("Reading file",filelist[i]))
      file_path<-paste0(wd,"/",filelist[i])
      df<-read.csv(file=file_path)
      
      df$Start.date<-as.POSIXct(df$Start.date,format="%Y-%m-%d %H:%M:%OS")
      df$End.date<-as.POSIXct(df$End.date,format="%Y-%m-%d %H:%M:%OS")
      df$Month<-as.factor(month(df$Start.date))
      
      #Convert times to minutes (do bc default is seconds and 1st minute is not tracked)
      df$Duration<-round(df$Duration/60,1)
      
      #Create strings for file names
      first_part<-substr(filelist[i],1,4)
      last_part<-substr(filelist[i],7,nchar(filelist[1])-4)
      
      for (j in seq(1:length(levels(df$Month)))){
        middle_part<-paste0("0",levels(df$Month)[j])
        monthly.data<-df %>% filter(Month==levels(df$Month)[j])
        output_file_name<-paste0(first_part,middle_part,last_part,".rds")
        output_file_name_path<-paste0(output_directory,output_file_name)
        print(paste("Saving",output_file_name_path))
        save(monthly.data,file=output_file_name_path)
      }
    }
    rm(df)
} else{
    print("File type not recognized!")
    }