library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rsconnect)

ui <- fluidPage(  
<<<<<<< HEAD
  titlePanel("SDN4"),
      plotlyOutput("plot2"))
server <- function(input, output) {
  
    
    p2 <- ggplotly(POS_FACE_MAP_PLOT,height = 1080, width =1920)
    
    
    output$plot2 <- renderPlotly({p2})
=======
  titlePanel("Plotly"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotlyOutput("plot2"))))

server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    POS_FACE_MAP_PLOT
  })
>>>>>>> 31ab53ec96c9b41bba2cec09e0cc1cd78fdbe049
}

shinyApp(ui, server)