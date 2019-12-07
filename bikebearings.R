##bearings

bikebearing <- function(bdata, round = TRUE, samestation = TRUE, output = c("vector", "plot"), facet_group = "NULL"){
  
  library(fossil)
  library(dplyr)
  
  b <- bdata
  
  if(samestation == FALSE){
    b <- filter(b, Start.station.number != End.station.number)
  }
  
  ##add coordinate data based on station location data
  b$Start_lat <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$Start_long <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  b$End_lat <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$End_long <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  
  #calculate bearings
  bearings <- mapply(earth.bear,b$Start_long, b$Start_lat, b$End_long, b$End_lat)
  
  #round to nearest int if specified
  if (round == TRUE) {
    bearings <- round(bearings,0)
  }
  
  
  #return either vector or radar plot
  if (output == "vector"){
    return(bearings)
  } 
  
  ##returning plot
  else if (output == "plot"){
    
    library(ggplot2)
    
    ##aggregate for plotting
    
    ##split if faceting chart 
    
    ##no faceting
    if (is.null(facet_group)){
      plotdat <- cbind(b, bearings)
      
      ##aggregate just on bearing count
      plotdat <- aggregate(Bike.number ~ bearings, plotdat, length)
      names(plotdat) <- c("bearings", "ride_count")
      
      g <- ggplot(plotdat, aes(x = bearings, y = ride_count, group = 1)) +
        geom_polygon() + coord_polar() + theme_bw() + scale_x_discrete(breaks = c(0, 90, 180, 270)) +
        scale_y_continuous(labels = scales::comma)+
        labs(x = "", y = "Count of Rides")
      
      return(g) 
      
    } else{
      
      plotdat <- cbind(b, bearings)
      #build both sides of aggregation equation
      lhs = "Bike.number ~"
      rhs = paste0("bearings +", facet_group)
      
      #aggregate on bearings and facet group
      plotdat <- aggregate(as.formula(paste(lhs,rhs)), plotdat, length)
      names(plotdat) <- c("bearings", facet_group, "ride_count")
      
      
      g <- ggplot(plotdat, aes_string(x = 'bearings', y = 'ride_count', group = facet_group)) +
        geom_polygon() + coord_polar() + theme_bw() + scale_x_discrete(breaks = c(0, 90, 180, 270)) +
        scale_y_continuous(labels = scales::comma)+
        labs(x = "", y = "Count of Rides") + facet_wrap(facet_group)
      
      return(g) 
      
    }
   
    
  }
  
  
}
