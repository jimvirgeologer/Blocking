library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rsconnect)

ui <- fluidPage(  
  titlePanel("Plotly"),
      plotlyOutput("plot2"))
server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    POS_FACE_MAP_PLOT
  })
}

shinyApp(ui, server)