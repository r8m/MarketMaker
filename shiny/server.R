library(shiny)

library(dplyr)
library(tidyr)
library(xts)
library(zoo)
library(ggplot2)
library(scales)
library(markovchain)

source('orderbookPlotStrategies.R')



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should re-execute automatically
    #     when inputs change
    #  2) Its output type is a plot
    output$politicsPlot <- renderPlot({
        t <- input$time
        s <- input$spread
        PlotStrategies(t,s)
        
    })
})