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



     

  # Update the x variable slider
  observe({
    
    #Subset the data to only be the x variable
    house2 <- house %>% select(input$xvar)
    minxvalue <- min(house2)
    maxxvalue <- max(house2)
    
    updateSliderInput(session, "xValueselect", min=minxvalue, max = maxxvalue, step = step)
    #Close update slider function
  })      
  
  # Update the y variable slider
  observe({

      #Subset the data to only be the y variable
      house3 <- house %>% select(input$yvar)
      minyvalue <- min(house3)
      maxyvalue <- max(house3)
      
      updateSliderInput(session, "yValueselect", min=minyvalue, max = maxyvalue, step = step)
    #Close update slider function
  })      
  
  

  
  
  # Create the main plot
  output$check <- renderText({

      minvalue <- input$yValueselect[1]
      maxvalue <- input$yValueselect[2]
      output <- c(input$yvar, minvalue, maxvalue)
  })

# Function to create main plot  
output$edaPlot <- renderPlot({
    
    #Subset the data based on the slider inputs
      minXvalue <- input$xValueselect[1]
      maxXvalue <- input$xValueselect[2]
      minYvalue <- input$yValueselect[1]
      maxYvalue <- input$yValueselect[2]
      
      plotData <- house %>% filter( !!rlang::sym(input$xvar) >= minXvalue &  !!rlang::sym(input$xvar) <= maxXvalue)
    
      plotDataScatter <- plotData %>% filter( !!rlang::sym(input$yvar) >= minYvalue &  !!rlang::sym(input$yvar) <= maxYvalue)
    #Create the base plot
    plot <- ggplot(data = plotData) 
    
    #Create plot based on button selected
    
    #Create histogram
    if(input$GraphSelect == 1){
        plot + 
        geom_histogram(aes_string(x = input$xvar), color = 'black', fill = 'blue')
    
    #Create Bar plot 
    }else if (input$GraphSelect == 3){
        plot + 
        geom_bar(aes_string(x = input$xvar))
    }
    
    #Create Scatter Plot
    else{
      ggplot(data = plotDataScatter)  +
        geom_point(aes_string(x = input$xvar, y = input$yvar), color = 'Red')
    }
  })


})
