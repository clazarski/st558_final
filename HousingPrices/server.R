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
   
# Main plot
  #Observe to see if the variable has changed



  
observe({
      value <- input$xvar
      # If the x variable is updated change the slider values
      if (value == input$xvar){
        #Subset the data to only be the x variable
        house2 <- house %>% select(input$xvar)
        # Set the min and max slider values based on the variable values
        min = min(house2)
        max = max(house2)
        # Update the slider
        updateSliderInput(session, "xValueselect", min=min, max = max, step = step)
      }
      else {
        updateSliderInput(session, "xValueselect", min= 1)
      }
      #Update the data
      #Close update slider function
    })      

  # Create the main plot
  output$check <- renderText({

      minvalue <- input$xValueselect[1]
      maxvalue <- input$xValueselect[2]
      output <- c(input$xvar, minvalue, maxvalue)
  })
  
output$edaPlot <- renderPlot({
    
    #Subset the data based on the slider inputs
      minvalue <- input$xValueselect[1]
      maxvalue <- input$xValueselect[2]
    #  variable <- !!rlang::sym(input$xvar)
      plotData <- house %>% filter( !!rlang::sym(input$xvar) >= minvalue &  !!rlang::sym(input$xvar) <= maxvalue)
    #plotData <- house %>% filter(value >= input$xValueselect[1])
    
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


})
