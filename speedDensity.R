
library(dplyr)
library(lubridate)
library(ggplot2)

source("misc.R")


##across population, speed of memeber vs casuals

dat3 <- x
dat3 <- dat3%>% filter(Member.type != 'Unknown')%>%add.speed()


Member.medSpeed <- median(dat3$Speed[dat3$Member.type== "Member"])

Casual.medSpeed <- median(dat3$Speed[dat3$Member.type== "Casual"])


g <- ggplot(dat3, aes(Speed, fill = Member.type)) + geom_density(alpha = 0.2)
full.pop.speeds <- g  + xlim(c(0,20)) + theme_minimal() + labs(x = "Speed (mph)", title = "Member vs Casual Ride Speeds\nFull Population") +
                      geom_vline(xintercept = Member.medSpeed, show.legend = NA, linetype = "dashed") +  
                      geom_vline(xintercept = Casual.medSpeed, show.legend = NA, linetype = "dashed") + 
                      annotate("text", x = Member.medSpeed + 1, y = .35, label = paste("Member\nMedian Speed \n",round(Member.medSpeed,2)), 
                               size = 3) + 
                      annotate("text", x = Casual.medSpeed -1, y = .375, label = paste("Casual\nMedian Speed \n",round(Casual.medSpeed,2)),
                               size = 3)

##plot of members v causlas speeds acorss population
full.pop.speeds


##facet all rides by rush times

dat3 <- dat3%>%add.rush.q()


##Calculate median speeds of members by rush
Member.med.data <- dat3 %>% filter(Member.type == "Member") %>% group_by(rush) %>% 
  summarize(medSpeed = median(Speed))

##Calculate median speeds of casuals by rush 
Casual.med.data <- dat3 %>% filter(Member.type == "Casual") %>% group_by(rush) %>% 
  summarize(medSpeed = median(Speed))

##plot facet by rush
g <- ggplot(dat3[dat3$Member.type != "Unknown",], aes(x = Speed, fill = Member.type)) 
speeds.rushtime <- g + geom_density(alpha = 0.2)  + xlim(c(0,20)) + theme_minimal() + labs(x = "Speed (mph)", title = "Member vs Casual Ride Speeds",
                                                                   y = "Density", fill = "Membership") + 
  geom_vline(data = Member.med.data, aes(xintercept = medSpeed), show.legend = NA, linetype = "dashed") +  
  geom_vline(data = Casual.med.data, aes(xintercept = medSpeed), show.legend = NA, linetype = "dashed")  + facet_wrap(~rush)


speeds.rushtime
