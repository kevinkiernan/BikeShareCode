


##count rides per route in data set
#can add grouping as argument with column name to group by
routeCount <-function(x, group_col = NULL) {
  library(tidyr)
  

      dat <- x
      
      #select relevant cols and create ID
      dat$routeid <- paste(dat$Start.station.number, dat$End.station.number)
      
      ##if no grouping selected
      if (is.null(group_col)){ 
      #aggregate by bike route 
      #should change Bike.number name
      bikeroute <- aggregate(Bike.number ~ routeid , dat, length)
      
      
      bikeroute <- separate(bikeroute, routeid, into = c("Start.station.number","End.station.number"), remove = FALSE)
      
      names(bikeroute) <- c("routeid", "Start.station.number","End.station.number", "No_of_Rides")
     } 
      
      else{
      #if grouping was selected
       lhs <- "Bike.number ~"
       rhs <- paste0("routeid +", group_col)
       
    bikeroute <- aggregate(as.formula(paste(lhs,rhs)), dat, length)
    bikeroute <- separate(bikeroute, routeid, into = c("Start.station.number","End.station.number"), remove = FALSE)
    
    names(bikeroute) <- c("routeid", "Start.station.number","End.station.number", group_col, "No_of_Rides")
  
    }

     return(bikeroute) 
      
}




#provide bikes data set
routesFormat <-  function(x, sameStation = FALSE){
  library(dplyr)
  source("stationlocation.R")
  
  #isolate stations
  p <- select(x, Start.station.number, End.station.number)
  
  if (sameStation == FALSE){
    p <- filter(p, Start.station.number != End.station.number)
  }
  
  #create route id
  
  p$routeid <- paste(p$Start.station.number,p$End.station.number)
  p <- unique(p)
  
  #split data into two - trip start info
  p.origin <- select(p, routeid, Start.station.number)
  p.destination <- select(p, routeid, End.station.number)
  
  
  #add origin, destination cols
  p.origin$type <- "origin"
  p.destination$type <-'destination'
  
  names(p.origin)[2] <- c( "TERMINAL_NUMBER")
  names(p.destination)[2] <- c( "TERMINAL_NUMBER")
  
  p <- rbind(p.origin,p.destination)
  p <- merge(p, stationlocations, all.x = TRUE)
  
  return(p)
}


##some plotdat

dat2 <- x[1:500000,]

routes <- routesFormat(dat2)

rides <- routeCount(dat2)


plotdat <- merge(routes, rides[,c("routeid","No_of_Rides")])

plotdat2 <- merge(plotdat2, stationlocations, all.x = TRUE)

#write.csv(plotdat2, file = "Tableau/routes.csv")
