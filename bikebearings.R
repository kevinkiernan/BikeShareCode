##bearings

bikebearing <- function(bdata, round = TRUE, samestation = TRUE, output = c("vector", "plot")){
  
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
  } else if (output == "plot"){
    
    library(ggplot2)
    
    
    plotdat <- as.data.frame(table(bearings))
    plotdat$bearings <- as.factor(plotdat$bearings)
    
    g <- ggplot(plotdat, aes(x = bearings, y = Freq, group = 1)) +
      geom_polygon() + coord_polar() + theme_bw() + scale_x_discrete(breaks = c(0, 90, 180, 270)) +
      scale_y_continuous(labels = scales::comma)+
      labs(x = "", y = "Count of Rides")
    
    return(g)
    
  }
  
  
}
