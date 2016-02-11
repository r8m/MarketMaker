library(shiny)

#library(dplyr)
#library(tidyr)
#library(xts)
#library(zoo)
#library(ggplot2)
#library(scales)
#library(markovchain)

source('orderbookPlotStrategies.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should re-execute automatically
    #     when inputs change
    #  2) Its output type is a plot
    output$politicsPlotRTS <- renderPlot({
        t <- input$time
        s <- input$spread
        PlotStrategies("politics_RTS-3.16_FT_2016-01-04_gamma_0.1_dzetamax_10_.RData",t,s)
    })
    output$politicsPlotSBRF <- renderPlot({
      t <- input$time
      s <- input$spread
      PlotStrategies("politics_SBRF-3.16_FT_2016-01-04_gamma_0.1_dzetamax_10_.RData",t,s)
    })
    output$politicsPlotSi <- renderPlot({
      t <- input$time
      s <- input$spread
      PlotStrategies("politics_Si-12.15_FT_2015-09-16_gamma_0.1_dzetamax_10_.RData",t,s)
    })
    
    
})