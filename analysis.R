### NBA prices

library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(XML)

setwd("~/Dropbox/Other/NBA/prices")

filenames <- list.files()
filenames <- filenames[-which(filenames=="plots")]
d <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))

table(d$home) #what is going on with Minnesota??

### note to self: raw data includes car parking. should get rid of this. 

loc <- sort(unique(d$location))

parking <- c("PARKING", "Parking", "PARKING PASS", 
             "PREMIUM PARKING", "LEXUS", 
             "LEXUS GARAGE RESERVED PARKING", 
             "LEXUS RESERVED PARKING PASS", "SUITE PARKING", 
             "VIP PARKING PASS ONLY", "VIP RESERVED LOT PASS ONLY", "parking")

d<- d[!(d$location %in% parking),]


## summaries
dsum.home <- ddply(d, "home", summarise, mean.price = mean(price), med.price = median(price), total.tickets = sum(qty))
dsum.away <- ddply(d, "away", summarise, mean.price = mean(price), med.price = median(price), total.tickets = sum(qty))

dsum.home <- dsum.home[order(-dsum.home$med.price),]
ord<- c(as.character(dsum.home$home))
dsum.home$team <- reorder(dsum.home$home, -dsum.home$med.price)

for(i in 1:nrow(dsum.home)){
  dsum.home$away.med.price[i] <- dsum.away$med.price[as.character(dsum.away$away) == as.character(dsum.home$home[i])]
}


standings = "http://espn.go.com/nba/standings/_/group/league"

stand.table = readHTMLTable(standings, header=F, which=1,stringsAsFactors=F)

stand.table

temp <- strsplit(stand.table$V1, " ")
temp2 <- lapply((strsplit(as.character(dsum.home$home), " ")), `[[`,1)

stand.team <- rep(NA, length = 30)

for ( i in 1:length(temp)){
  if(temp[[i]][1]=="Los"){
    if(substr(temp[[i]][3], 1, 1)=="C") {
      stand.team[i] <- "Los Angeles Clippers"
    }
    else{
      stand.team[i] <- "Los Angeles Lakers"
    }
  }
  if(temp[[i]][1]=="New"){
    if(substr(temp[[i]][2], 1, 1)=="O") {
      stand.team[i] <- "New Orleans Pelicans"
    }
    else{
      stand.team[i] <- "New York Knicks"
    }
  }
  else{
    stand.team[i]<- as.character(dsum.home$home[which(temp2==temp[[i]][1])])
  }
  
}

stand.team[6] <- "Los Angeles Clippers"

for(i in 1:nrow(dsum.home)){
  dsum.home$standing[i] <- which(as.character(dsum.home$team[i])==stand.team)
}

for(i in 1:nrow(d)){
  d$home.standing[i] <- which(as.character(d$home[i])==stand.team)
}

d$team <- reorder(d$home, d$home.standing)

dgames <- d %>% group_by(team, away, date) %>% summarise(med.price = median(price))
dgames$away[dgames$away=="Portland Trailblazers"] <- "Portland Trail Blazers"

for(i in 1:nrow(dgames)){
  dgames$home.standing[i] <- which(as.character(dgames$team[i])==stand.team)
  dgames$away.standing[i] <- which(as.character(dgames$away[i])==stand.team)
}

dgames$ave.standing <- (dgames$away.standing+ dgames$home.standing)/2
dgames$home.gt.away <- 0
dgames$home.gt.away[dgames$home.standing>dgames$away.standing] <- 1
dgames$home.away.date <- paste0(as.character(dgames$team),", ",as.character(dgames$away), " (", format(as.Date(dgames$date), '%m/%d/%Y'), ")")
dgames2 <- dgames[-which(dgames$med.price==max(dgames$med.price)),]


###################################################

pdf("plots/med_price.pdf")
ggplot(data=dsum.home, aes(x=team, y=med.price, fill=team)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Team",y="Median Price ($)") 
dev.off()


pdf("plots/med_price_home_away.pdf", width = 12)
ggplot(data=dsum.home, aes(x=med.price, y=away.med.price)) +
  geom_point(colour="black", stat="identity") +
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Median Ticket Prices")+
  labs(x="Median Price playing at home ($)",y="Median price playing away ($)") +
  geom_abline(slope=1, intercept=25, colour = "darkgrey")+
  geom_text(aes(label=team),hjust=0, vjust=0, size=4, color = "darkslateblue")+
  scale_x_continuous(limits = c(25, 350))+
  scale_y_continuous(limits = c(25, 350))
dev.off()

pdf("plots/med_price_standing.pdf", width = 12)
ggplot(data=dsum.home, aes(x=standing, y=med.price)) +
  geom_point(colour="black", stat="identity") +
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Ticket Price v Current NBA Standing")+
  labs(x="NBA standing",y="Median price at home ($)") +
  
  geom_text(aes(label=team), size=4, hjust = 0, vjust = 0,color = "seagreen4")+
  scale_x_continuous(limits = c(1,35))
  #scale_y_continuous(limits = c(25, 350))
dev.off()


pdf("plots/med_price_awaystanding.pdf", width = 12)
ggplot(data=dgames, aes(x=team, y=med.price)) +
  geom_boxplot(colour="black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  guides(fill=FALSE)+
  ggtitle("Ticket Price v Current NBA Standing")+
  labs(x="NBA standing (home team)",y="Median ticket price ($)") 
  #geom_text(aes(label=team), size=4, hjust = 0, vjust = 0,color = "seagreen4")+
  #scale_x_continuous(limits = c(1,30))
#scale_y_continuous(limits = c(25, 350))
dev.off()




pdf("plots/med_price_awaystanding_noLALUJ.pdf", width = 12)
ggplot(data=dgames2, aes(x=team, y=med.price)) +
  geom_boxplot(colour="black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  guides(fill=FALSE)+
  ggtitle("Ticket Price v Current NBA Standing")+
  labs(x="Home team, ordered by standing",y="Median ticket price ($)") 
#geom_text(aes(label=team), size=4, hjust = 0, vjust = 0,color = "seagreen4")+
#scale_x_continuous(limits = c(1,30))
#scale_y_continuous(limits = c(25, 350))
dev.off()


pdf("plots/med_price_avestanding.pdf", width = 12)
ggplot(data=dgames2, aes(x=ave.standing, y=med.price, fill = team)) +
  geom_point() +
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Ticket Price v Average NBA Standing of Teams")+
  labs(x="Average NBA standing",y="Median ticket price ($)") +
  geom_text(aes(label=ifelse(med.price>400,home.away,'')), hjust = 0, vjust = 0, size=4, color = "darkblue")+
  scale_x_continuous(limits = c(1,30))
#scale_y_continuous(limits = c(25, 350))
dev.off()