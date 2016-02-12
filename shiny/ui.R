library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Market Maker Politics!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("time",
                  "Time:",
                  min = 1,
                  max = 10,
                  value = 1, 
                  step =1),
      sliderInput("spread",
                  "Spread:",
                  min = 1,
                  max = 9,
                  value = 1,
                  step=1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
       tabPanel("Combined BidAsk marketdata",plotOutput("politicsPlotSBRF")),
       tabPanel("Separate BidAsk marketdata",plotOutput("politicsPlotSBRFN"))
      )
    )
  )
))