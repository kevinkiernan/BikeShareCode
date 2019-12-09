

#function to add 
routeidentifier <- function(x, oneWayid = TRUE){
  library(dplyr)
  
  
  dat <- x
  
  ##oneWayid == TRUE means origin are distinct, FALSE means A to B 
  
  if( oneWayid == FALSE){
    
    
    dat$routeid <- paste(dat$Start.station.number,dat$End.station.number)
    return(dat)
   
  } else if (oneWayid == TRUE){
  
  ##Add one way id column, if station no A <= station B, return start.stop
  ##                        if a > b, return stop.start
  dat <- x %>% mutate(routeid = case_when( Start.station.number <=  End.station.number ~
                                             paste(Start.station.number, End.station.number),
                                           Start.station.number >  End.station.number ~ 
                                             paste(End.station.number, Start.station.number))
  )
  
  return(dat)
  }
  
  else { print('argument error')}
  
  
}


##count rides per route in data set
#can add grouping as argument with column name to group by
routeCount <-function(x, group_col = NULL, oneWayid = FALSE) {
  library(tidyr)
  
  
      dat <- routeidentifier(x, oneWayid = oneWayid)
      
      
      ##if no grouping selected
      if (is.null(group_col) & oneWayid == FALSE){ 
        
        #aggregate by bike route 
        #no grouping and dual way id
        bikeroute <- aggregate(Bike.number ~ routeid , dat, length)
        bikeroute <- separate(bikeroute, routeid, into = c("Start.station.number","End.station.number"), remove = FALSE)
        names(bikeroute) <- c("routeid", "Start.station.number","End.station.number", "No_of_Rides")
        
      } 
      
      
      else if (is.null(group_col) & oneWayid == TRUE){
      #no grouping column and single ID
        bikeroute <- aggregate(Bike.number ~ routeid , dat, length)
        names(bikeroute) <- c("routeid", "No_of_Rides")
      }
    
      
      
        #dual way (origin and dest)
      else if (!is.null(group_col) & oneWayid == FALSE){
      #if grouping was selected
       lhs <- "Bike.number ~"
       rhs <- paste0("routeid +", group_col)
       
        bikeroute <- aggregate(as.formula(paste(lhs,rhs)), dat, length)
        bikeroute <- separate(bikeroute, routeid, into = c("Start.station.number","End.station.number"), remove = FALSE)
        
        names(bikeroute) <- c("routeid", "Start.station.number","End.station.number", group_col, "No_of_Rides")
  
      } 
      
      
      
      
      else if(!is.null(group_col) & oneWayid == TRUE){
        #single way id, dont break back into terminal numbers
         lhs <- "Bike.number ~"
        rhs <- paste0("routeid +", group_col)
        bikeroute <- aggregate(as.formula(paste(lhs,rhs)), dat, length)
        names(bikeroute) <- c("routeid",group_col, "No_of_Rides")
      }

     return(bikeroute) 
      
}




#provide bikes data set
routesFormat <-  function(x, sameStation = FALSE, singleRoute = FALSE, group_col = NULL){
  library(dplyr)
  source("stationlocation.R")
  
  #isolate stations
  p <- select(x, Start.station.number, End.station.number, Duration, Distance, eval(group_col))
  
  #remove looping rides
  if (sameStation == FALSE){
   
       p <- filter(p, Start.station.number != End.station.number)
       
  }
  
  #create route id
  
  p$routeid <- paste(p$Start.station.number,p$End.station.number)
  
  p <- unique(p)
  
  
  #split data into two - trip start info
  p.origin <- select(p, routeid, Start.station.number, Duration, Distance, group_col)
  p.destination <- select(p, routeid, End.station.number, Duration, Distance, group_col)
  
  
  #add origin, destination cols
  p.origin$type <- "origin"
  p.destination$type <-'destination'
  
  names(p.origin)[2] <- c( "TERMINAL_NUMBER")
  names(p.destination)[2] <- c( "TERMINAL_NUMBER")
  
  p <- rbind(p.origin,p.destination)
  p <- merge(p, stationlocations, all.x = TRUE)
  
  p <- separate(p, routeid, into = c("Start.station.number","End.station.number"), remove = FALSE)
  
  return(p)
}




add.rush <- function(x){
  x <- x %>% mutate (rush = case_when(depart.hour >= 0 & depart.hour < 5 ~ "Midnight to 4am",
                                      depart.hour >= 5 & depart.hour < 9 ~ "5am to 8am",
                                      depart.hour >= 9 & depart.hour < 13 ~ "9am to Noon",
                                      depart.hour >= 13 & depart.hour < 17 ~ "1pm to 4pm",
                                      depart.hour >= 17 & depart.hour < 21 ~ "5pm to 8pm",
                                      depart.hour >= 21 & depart.hour <= 23 ~ "9pm to Midnight"))
  return (x)
}



##some plotdat

library(lubridate)

dat2 <- x
dat2$depart.hour <- hour(dat2$Start.date)

rides <- routesFormat(dat2,group_col = "depart.hour")

#add speed miles per hour
rides$speed <- rides$Distance / (rides$Duration/60)


rides <- add.rush(rides)

med.ride.speed = aggregate(speed ~ rush + routeid + type + LATITUDE + LONGITUDE, rides, median)

#calculate busiest routes by departing hour
routes <- routeCount(dat2, "depart.hour", oneWayid = TRUE)

routes <- add.rush(routes)

l <- aggregate(No_of_Rides ~ rush + routeid, routes,sum)

routes.popular <- l %>% group_by(rush) %>% filter(No_of_Rides > quantile(No_of_Rides, .99)) 
  
plotdat2 <- merge(med.ride.speed, l)
plotdat3 <- merge(med.ride.speed,routes.popular)

write.csv(plotdat3, file = "Tableau/routes_speed.csv")







