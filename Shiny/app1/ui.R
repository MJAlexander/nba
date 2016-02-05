## ui.R
require(rCharts)
dgames2 <- read.csv("Shiny/app1/dgames2.csv", header=T)
dgames2$home.away <- as.character(dgames2$home.away)
shinyUI(fluidPage(
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
))