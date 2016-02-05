## server.r
require(rCharts)
dgames2<- read.csv("Shiny/app1/dgames2.csv", header=T)
dgames2$home.away.date <- as.character(dgames2$home.away.date)

shinyServer(function(input, output) {
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
})