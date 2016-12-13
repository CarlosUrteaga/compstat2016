#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

ui <- 
server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())