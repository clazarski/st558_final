#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
house <- read_csv("train.csv")
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   

output$check <- renderText({
  input$xValueselect[1]
})

    output$edaPlot <- renderPlot({
      
      plotData <- house %>% filter(input$xvar >= input$xValueselect[1])
      plot <- ggplot(data = plotData) 
      
      if(input$GraphSelect == 1){
        plot + geom_histogram(aes_string(x = input$xvar))
      }else if (input$GraphSelect == 3){
        plot + geom_bar(aes_string(x = input$xvar))
      }
      else{
        plot + geom_point(aes_string(x = input$xvar, y = input$ybar))
}
          })
  
    observe({
      value <- input$xvar
      if (value == input$xvar){
        house2 <- house %>% select(input$xvar)
        min = min(house2)
        max = max(house2)
        range = max - min

        updateSliderInput(session, "xValueselect", min=min, max = max, step = step)
      }
      else {
        updateSliderInput(session, "xValueselect", min= 1)
      }
    })

  
  
  
  
})
