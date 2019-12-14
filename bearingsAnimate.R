
source("bikebearings.R")
source("misc.R")
source("readin.R")

library(transformr)

##new dummy data
dat5 <- x

##add 10min id interval, rush periods and remove same stations
dat5 <- dat5%>%add.10min()%>%add.rush.q()%>%filter(Start.station.number != End.station.number)
##add bearings
dat5$bearing <- bikebearing(dat5, output = "vector")

##plot
##aggregate
plotdat <- aggregate( Bike.number ~ bearing + rush + Int, dat5, length)
names(plotdat) <- c("bearings", "rush", "Int", "ride_count")


##plot without facets for animations
g <- ggplot(plotdat, aes(x = bearings, y = ride_count, frame = Int)) + geom_polygon() + coord_polar() + 
  theme_bw() + transition_states(Int) +
  scale_x_continuous(breaks = c(0, 90, 180, 270)) +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "", y = "Count of Rides") 

suppressWarnings(animate(g,width = 1000, height = 1000,fps=5))

ggplotly(g)
