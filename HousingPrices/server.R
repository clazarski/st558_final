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
library(DT)
library(tree)
library(caret)
library(randomForest)
house <- read_csv("train.csv")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

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

  

# Function to create main plot  
output$edaPlot <- renderPlot({
    
    #Subset the data based on the slider inputs
      minXvalue <- input$xValueselect[1]
      maxXvalue <- input$xValueselect[2]
      minYvalue <- input$yValueselect[1]
      maxYvalue <- input$yValueselect[2]
      
      XData <- house %>% filter( !!rlang::sym(input$xvar) >= minXvalue &  !!rlang::sym(input$xvar) <= maxXvalue)
    
      YData <- house %>% filter( !!rlang::sym(input$yvar) >= minYvalue &  !!rlang::sym(input$yvar) <= maxYvalue)
      
      plotDataScatter <- XData %>% filter( !!rlang::sym(input$yvar) >= minYvalue &  !!rlang::sym(input$yvar) <= maxYvalue)
      
      output$xTable <- renderTable({
        summaryX <- XData %>% select(!!rlang::sym(input$xvar)) %>% summary() 
      })
      output$yTable <- renderTable({
        summaryY <- YData %>% select(!!rlang::sym(input$yvar)) %>% summary()
        summaryY
      })

    #Create the base plot
    plot <- ggplot(data = XData) 
    
    #Create plot based on button selected
    
    #Create histogram of x variable
    if(input$GraphSelect == 1){
      ggplot(data = XData) + 
        geom_histogram(aes_string(x = input$xvar), color = 'black', fill = 'blue')

      
    #Create histogram of y variable
      }else if(input$GraphSelect == 2){
        
        ggplot(data = YData) +
        geom_histogram(aes_string(x = input$yvar), color = 'black', fill = 'blue')  
        

    #Create Scatter Plot
    }else{
      if(input$LSRL == "TRUE"){

          output$LSRL <- renderText({
            fit <- lm(as.formula(paste(input$yvar," ~ ",input$xvar)), data=plotDataScatter)
            intercept <- fit$coefficients[1]
            slope <- fit$coefficients[2]
            output <- paste("The intercept is",
                            round(intercept,3),
                            "and the slope is",
                            round(slope,3))
          })

        ggplot(data = plotDataScatter, aes_string(x = input$xvar, y = input$yvar))  +
        geom_point( color = 'Red') + 
        geom_smooth(method ='lm')
        
      }else{
        output$LSRL <- renderText({
        })
        ggplot(data = plotDataScatter, aes_string(x = input$xvar, y = input$yvar)) +
          geom_point( color = 'Red') 
        
        
      }
    }
    #End the plotting panel
  })

#Create the train and test data sets
traindata <- eventReactive(input$splitbutton,{
  set.seed(1234)
  splitsize <- as.numeric(input$train)
  numObs <- nrow(house)
  index <- sample(1:numObs, size = splitsize*numObs, replace =FALSE)
  traindata <- house[index,]
})

testdata <- eventReactive(input$splitbutton,{
  set.seed(1234)
  splitsize <- as.numeric(input$train)
  numObs <- nrow(house)
  index <- sample(1:numObs, size = splitsize*numObs, replace =FALSE)
  testdata <- house[-index,]
})


output$datasets <- renderPrint({
  train <- nrow(traindata())
  test <- nrow(testdata())
  data <- data.frame(train, test)
  print(data)
})


mlrFit <- eventReactive(input$button,{
  formula <- as.formula(paste(input$yvariable," ~ ",paste(input$mlrmodelinputs, collapse = "+")))
  
  fit <- train(formula, data = traindata(),
               method = "lm",
               trControl = trainControl(method = "cv",number = 5))
})



treeFit <- eventReactive(input$button,{
  formula <- as.formula(paste(input$yvariable," ~ ",paste(input$treemodelinputs, collapse = "+")))
  fit <- tree(formula, data=traindata())
  
})



rfFit <- eventReactive(input$button,{
  formula <- as.formula(paste(input$yvariable," ~ ",paste(input$rfmodelinputs, collapse = "+")))
  fit <- train(formula, data = traindata(),
                 method = "rf",
                 trControl = trainControl(method = "cv",
                                          number = 5),
                 tuneGrid = data.frame(mtry = 1:3))
})

mlrpred  <- reactive({
  predtreefit <- predict(mlrFit(), newdata = testdata())
  results <- postResample(predtreefit, testdata()$SalePrice)
})
treepred <- reactive({
  predtreefit <- predict(treeFit(), newdata = testdata())
  results <- postResample(predtreefit, testdata()$SalePrice)
})
rfpred <- reactive({
  predtreefit <- predict(rfFit(), newdata = testdata())
  results <- postResample(predtreefit, testdata()$SalePrice)
})

output$mlrformula <- renderPrint({
  formula <- (paste(input$yvariable," ~ ",paste(input$mlrmodelinputs, collapse = "+")))
  print(formula)
})

output$treeformula <- renderPrint({
  formula <- (paste(input$yvariable," ~ ",paste(input$treemodelinputs, collapse = "+")))
  print(formula)
})

output$rfformula <- renderPrint({
  formula <- (paste(input$yvariable," ~ ",paste(input$rfmodelinputs, collapse = "+")))
  print(formula)
})
output$mlrmodel <- renderPrint({
  results <- summary(mlrFit())
  print(results)
})

output$treemodel <- renderPrint({
  results <- summary(treeFit())
  print(results)
})

output$rfmodel <- renderPrint({
  results <- summary(rfFit()$results)
  print(results)
})

output$predictions <- renderPrint({
  results <- data.frame(treepred(), mlrpred(), rfpred())
  print(results)
})

# Create prediction variables


# Create data table for data tab


data <- reactive({
  data <- house %>% select( Id, input$columns) %>% filter(Id >= input$firstrow &  Id <= input$lastrow)
  })





output$fancyTable <- DT::renderDataTable(datatable(data(), 
                                                   extensions = 'Buttons',
                                                   options = list( 
                                                     dom = "Blfrtip",
                                                     buttons = 
                                                       list("copy", list(
                                                         extend = "collection"
                                                         , buttons = c("csv", "excel", "pdf")
                                                         , text = "Download"
                                                       ) ) 
                                                   ))# end of buttons customizationselection = list(target = 'row+column'))
) #End output table function


# End the Server side
})
