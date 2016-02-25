################# NBA ticket prices #########################
#### Investigating ticket prices using data from StubHub ####
### See: http://shiny.demog.berkeley.edu/monicah/nba/ #######
########### Monica Alexander, February 2016 #################


### Load packages
library(plyr)
library(dplyr)
library(XML)
library(rCharts)
library(shiny)
setwd("~/Dropbox/Other/NBA/prices")


######### Read in files with ticket prices data for each team.  -------------------
### Note: Data in csv sourced from StubHub.com using tickets.py

filenames <- list.files()
d <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))

#table(d$home)

### Raw data includes car parking prices. should get rid of these, just to look at ticket prices. 

loc <- sort(unique(d$location))
parking <- c("PARKING", "Parking", "PARKING PASS", 
             "PREMIUM PARKING", "LEXUS", 
             "LEXUS GARAGE RESERVED PARKING", 
             "LEXUS RESERVED PARKING PASS", "SUITE PARKING", 
             "VIP PARKING PASS ONLY", "VIP RESERVED LOT PASS ONLY", "parking")
d<- d[!(d$location %in% parking),]


################## Summarize ticket prices by team and whether they are playing hom --------
dsum.home <- ddply(d, "home", summarise, mean.price = mean(price), med.price = median(price), total.tickets = sum(qty))
dsum.away <- ddply(d, "away", summarise, mean.price = mean(price), med.price = median(price), total.tickets = sum(qty))

dsum.home <- dsum.home[order(-dsum.home$med.price),]
ord<- c(as.character(dsum.home$home))
dsum.home$team <- reorder(dsum.home$home, -dsum.home$med.price) #ordered factor by price, used for plotting

for(i in 1:nrow(dsum.home)){
  dsum.home$away.med.price[i] <- dsum.away$med.price[as.character(dsum.away$away) == as.character(dsum.home$home[i])]
}


####################### Get the current team standings from table off ESPN website --------------

standings = "http://espn.go.com/nba/standings/_/group/league"
stand.table = readHTMLTable(standings, header=F, which=1,stringsAsFactors=F)
#stand.table

## Match teams from standings table to existing dataframe
temp <- strsplit(stand.table$V1, " ")
temp2 <- lapply((strsplit(as.character(dsum.home$home), " ")), `[[`,1)

stand.team <- rep(NA, length = 30)

for ( i in 1:length(temp)){
  if(temp[[i]][1]=="Los"){ #Two LA teams!
    if(substr(temp[[i]][3], 1, 1)=="C") {
      stand.team[i] <- "Los Angeles Clippers"
    }
    else{
      stand.team[i] <- "Los Angeles Lakers"
    }
  }
  if(temp[[i]][1]=="New"){ #Two teams starting with 'New'!
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

for(i in 1:nrow(dsum.home)){
  dsum.home$standing[i] <- which(as.character(dsum.home$team[i])==stand.team)
}

for(i in 1:nrow(d)){
  d$home.standing[i] <- which(as.character(d$home[i])==stand.team)
}

d$team <- reorder(d$home, d$home.standing)


############### Summarize ticket prices by game -----------------------------------------


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
dgames2 <- dgames[-which(dgames$med.price==max(dgames$med.price)),] #Kobe's last game is ridiculously expensive. 


################# Plot using d3 in rCharts package ----------------------------------------


# specify cols for each team
cols <- c("#006BB6", "#552582", "#000000", "#007DC3", "#F58426", "#860038", "#006BB6", 
          "#F9A01B", "#CE1141", "#C4CED3", "#00A94F", "#CE1141", "#4FA8FF", "#724C9F", 
          "#CE1142", "#E03A3E", "#F0163A", "#000000", "#23375B", "#008348", "#20385B", 
          "#1D1160", "#E56020", "#FFC633", "#002566", "#00471B", "#002B5C", "#B4975A", "#006BB6", "#001F70" )

##################### Plot home vs away price
pricePlot <- nPlot(
med.price ~ away.med.price, 
  data = dsum.home, 
  group = "team",
  type = "scatterChart", showLegend=F)

# Add axis labels and format the tooltip
pricePlot$yAxis(axisLabel = "Ticket price, home games ($)", width = 62, tickValues = "#! function (x) {    
    tickvalues = [25, 75, 125, 175, 225, 275, 325, 375];
    return tickvalues;
} !#")

pricePlot$xAxis(axisLabel = "Ticket price, away games ($)", 
                tickValues = "#! function (x) {    
    tickvalues = [25, 75, 125, 175, 225, 275, 325, 375];
    return tickvalues;
} !#")

pricePlot$chart(color = cols, tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + 'Home: $'+ x + ' , ' + 'Away: $'+ y + '</p>'
        } !#")


pricePlot$chart(showLegend=F, sizeRange = c(250,250), forceY = c(25, 375), forceX = c(25, 375))

pricePlot$setTemplate(
  afterScript = "<style>
.tick line {
  opacity: 0;
}
</style>")

pricePlot$set(width = 600, height = 600)

######################### Plot home price versus standing

standPlot <- nPlot(
  med.price ~ standing, 
  data = dsum.home, 
  group = "team",
  type = "scatterChart", showLegend=F)

# Add axis labels and format the tooltip
standPlot$yAxis(axisLabel = "Ticket price, home games ($)", width = 62, tickValues = "#! function (x) {    
                tickvalues = [25, 75, 125, 175, 225, 275, 325, 375];
                return tickvalues;
                } !#")

standPlot$xAxis(axisLabel = "Current Team Standing", 
                tickValues = "#! function (x) {    
                tickvalues = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30];
                return tickvalues;
                } !#")

standPlot$chart(color = cols, tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + 'Standing: '+ x + ' , ' + 'Price: $'+ y + '</p>'
        } !#")


standPlot$chart(showLegend=F, sizeRange = c(250,250), forceY = c(25, 375), forceX = c(0, 30))

standPlot$setTemplate(
  afterScript = "<style>
.tick line {
  opacity: 0;
}
</style>")

### Shiny chart of median ticket price of games by average team standing

app <- shinyApp(
  ui = fluidPage(
  title = "",
  fluidRow(
    column(4,wellPanel(selectInput(inputId = "team",
                                   label = "Select to highlight games for:",
                                   choices = levels(dgames2$team), 
                                   selected = "Golden State Warriors"),
                       selectInput(inputId = "team2",
                                   label = "and:",
                                   choices = c(" ", levels(dgames2$team)) ), 
                       helpText("Games between selected teams highlighted in red. To select all games for a particular team, leave second drop-down blank."),
                       helpText("Data sourced from StubHub on 02/01/2016"))
           ),
    column(8,     
           showOutput("myChart", "nvd3")
    )
    )
),

server = function(input, output) {
  output$myChart <- renderChart({
    #tm <- input$team
    tmp <- (sapply(1:nrow(dgames2), function(i) strsplit(dgames2$home.away.date[i], ", ")[[1]][2]))
    second.team <- gsub("\\ \\(.*","",tmp) 
    hasteam1 <- (sapply(1:nrow(dgames2), function(i) strsplit(dgames2$home.away.date[i], ", ")[[1]][1])== input$team )| (second.team== input$team)
    if(input$team2==" "){
      cols <- ifelse(hasteam1==T, "red", "darkgrey")
    }
    else{
      hasteam2 <- (sapply(1:nrow(dgames2), function(i) strsplit(dgames2$home.away.date[i], ", ")[[1]][1])== input$team2) |(second.team == input$team2)
      cols <- ifelse(hasteam1&hasteam2==T, "red", "darkgrey")
    }
    avePlot <- nPlot(
      med.price ~ ave.standing, 
      data = dgames2, 
      group = "home.away.date",
      type = "scatterChart", showLegend=F)
    
    avePlot$yAxis(axisLabel = "Median ticket price ($)", width = 62, tickValues = "#! function (x) {    
                  tickvalues = [0, 100, 200, 300,400,500];
                  return tickvalues; } !#")
    avePlot$xAxis(axisLabel = "Average Team Standing", 
                  tickValues = "#! function (x) {    
                  tickvalues = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30];
                  return tickvalues;} !#")
    avePlot$chart(color = cols, tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + 'Average standing: '+ x + ' , ' + 'Price: $'+ y + '</p>'
        } !#")
    avePlot$chart(showLegend=F, forceY = c(0, 650), forceX = c(0, 30),  sizeRange = c(100,100))
    avePlot$setTemplate(afterScript = "<style>.tick line {opacity: 0;}</style>")
    avePlot$addParams(dom="myChart", title = "Median price for resale tickets ")
    avePlot$set(width = 700, height = 500)
    return(avePlot)
  })
},options=list( width="120%", height=500)

  )

runApp(app)  
  