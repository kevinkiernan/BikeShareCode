



#add coordinates to data set
bikecoords <- function(data) {
  
  library(dplyr)
  
  
  b <- data
  
  ##add coordinate data based on station location data
  b$Start_lat <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$Start_long <- stationlocations[match(b$Start.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  b$End_lat <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LATITUDE"]
  b$End_long <- stationlocations[match(b$End.station.number, stationlocations$TERMINAL_NUMBER), "LONGITUDE"]
  
  return(b)
  
}


##add rush labels
add.rush <- function(x, addhour = TRUE){
  library(dplyr)
  library(lubridate)
  
  ##add hour if needed (requires Start.date col)
  if(addhour == TRUE){
    x <- x %>% mutate(depart.hour = hour(Start.date))
  }
  
  x <- x %>% mutate (rush = case_when(depart.hour >= 0 & depart.hour < 5 ~ "Midnight to 4am",
                                      depart.hour >= 5 & depart.hour < 9 ~ "5am to 8am",
                                      depart.hour >= 9 & depart.hour < 13 ~ "9am to Noon",
                                      depart.hour >= 13 & depart.hour < 17 ~ "1pm to 4pm",
                                      depart.hour >= 17 & depart.hour < 21 ~ "5pm to 8pm",
                                      depart.hour >= 21 & depart.hour <= 23 ~ "9pm to Midnight"))
  
  x$rush <- factor(x$rush, levels = c("Midnight to 4am", "5am to 8am","9am to Noon",
                                      "1pm to 4pm", "5pm to 8pm", "9pm to Midnight"))
  return (x)
}

##add day of week
add.weekday <- function(x){
  library(dplyr)
  library(lubridate)
  
  x <- x %>% mutate(Weekday = weekdays(Start.date))
  
  x$Weekday  <- factor(x$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  return(x)
}


##add speed
add.speed <- function(x){
  library(dplyr)
  library(lubridate)
  
  #add speed miles per hour
  x <- x %>% mutate(Speed = x$Distance / (x$Duration/60))
  
  return(x)
}


##add rush labels
add.rush.q <- function(x, addhour = TRUE){
  library(dplyr)
  library(lubridate)
  
  ##add hour if needed (requires Start.date col)
  if(addhour == TRUE){
    x <- x %>% mutate(depart.hour = hour(Start.date))
  }
  
  x <- x %>% mutate (rush = case_when(depart.hour == 0 ~ "9pm to Midnight",
                                      depart.hour >= 1 & depart.hour < 5 ~ "1am to 4am",
                                      depart.hour >= 5 & depart.hour < 9 ~ "5am to 8am",
                                      depart.hour >= 9 & depart.hour < 13 ~ "9am to Noon",
                                      depart.hour >= 13 & depart.hour < 17 ~ "1pm to 4pm",
                                      depart.hour >= 17 & depart.hour < 21 ~ "5pm to 8pm",
                                      depart.hour >= 21 & depart.hour <= 23 ~ "9pm to Midnight"))
  
  x$rush <- factor(x$rush, levels = c("1am to 4am", "5am to 8am","9am to Noon",
                                      "1pm to 4pm", "5pm to 8pm", "9pm to Midnight"))
  return (x)
}

##ID to add time intervals fro every ten minutes
add.10min <- function(x){
  dat <- x
  ##
  dat$Int <- paste0(substr(dat$Start.date,12,15), "0")
  return(dat)
}
