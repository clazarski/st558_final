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

#Read in the main data file
house <- read_csv("train.csv")



shinyServer(function(input, output, session) {
# EDA functions
  # Update the x variable slider
  observe({
    #Subset the data to only be the x variable
    house2 <- house %>% select(input$xvar)
    #Identify the min and max value of selected variable
    minxvalue <- min(house2)
    maxxvalue <- max(house2)
    
    #Update the original slider with min and max values
    updateSliderInput(session, "xValueselect", min=minxvalue, max = maxxvalue, step = step)
    #Close update slider function
  })      
  
  # Update the y variable slider
  observe({
      #Subset the data to only be the y variable
      house3 <- house %>% select(input$yvar)
      #Identify the min and max value of selected variable
      minyvalue <- min(house3)
      maxyvalue <- max(house3)
      #Update the original slider with min and max values
      updateSliderInput(session, "yValueselect", min=minyvalue, max = maxyvalue, step = step)
    #Close update slider function
  })      
  
  
# Create the main plot
  # Function to create main plot  
  output$edaPlot <- renderPlot({
    
    #Capture the select ranges of selected variables
    minXvalue <- input$xValueselect[1]
    maxXvalue <- input$xValueselect[2]
    minYvalue <- input$yValueselect[1]
    maxYvalue <- input$yValueselect[2]
      
    #Subset the x variable data according to values selected.
    XData <- house %>% filter( !!rlang::sym(input$xvar) >= minXvalue &  !!rlang::sym(input$xvar) <= maxXvalue)
    
    #Subset the y variable data according to values selected.
    YData <- house %>% filter( !!rlang::sym(input$yvar) >= minYvalue &  !!rlang::sym(input$yvar) <= maxYvalue)
      
    #Create a data set of the filtered data
      plotDataScatter <- XData %>% filter( !!rlang::sym(input$yvar) >= minYvalue &  !!rlang::sym(input$yvar) <= maxYvalue)
    
    #Create summary statistics for the x variable
      output$xTable <- renderTable({
      summaryX <- XData %>% select(!!rlang::sym(input$xvar)) %>% summary() 
      })
      
    #Create summary statistics for the y variable
      output$yTable <- renderTable({
        summaryY <- YData %>% select(!!rlang::sym(input$yvar)) %>% summary()
        summaryY
      })

    #Create the base plot
    plot <- ggplot(data = XData) 
    
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
          #Output the slope and intercept of best fit line if selected
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
          #Create the scatter plot
          ggplot(data = plotDataScatter, aes_string(x = input$xvar, y = input$yvar))  +
            geom_point( color = 'Red') + 
            geom_smooth(method ='lm')
          #If the LSRL button is not checked only plot the scatterplot
           }else{
              output$LSRL <- renderText({
              })
          ggplot(data = plotDataScatter, aes_string(x = input$xvar, y = input$yvar)) +
            geom_point( color = 'Red') 
      } #This ends the nested if else statment
    } # This ends the primary if else statement
  }) #This ends the main plot Functions

#Modeling page functions
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

  #Show the user how the data was separated
  output$datasets <- renderPrint({
    train <- nrow(traindata())
    test <- nrow(testdata())
    result <- paste("The train data has", train, "rows and the test data has", test, "rows")
    print(result)
  })

  #Multiple regression model results
    mlrFit <- eventReactive(input$button,{
      formula <- as.formula(paste(input$yvariable," ~ ",paste(input$mlrmodelinputs, collapse = "+")))
  
      fit <- train(formula, data = traindata(),
               method = "lm",
               trControl = trainControl(method = "cv",number = 5))
    })



  #Classification tree model results
    treeFit <- eventReactive(input$button,{
      formula <- as.formula(paste(input$yvariable," ~ ",paste(input$treemodelinputs, collapse = "+")))
      fit <- tree(formula, data=traindata())
    })


  #Random forest model results
    rfFit <- eventReactive(input$button,{
      formula <- as.formula(paste(input$yvariable," ~ ",paste(input$rfmodelinputs, collapse = "+")))
      fit <- train(formula, data = traindata(),
                 method = "rf",
                 trControl = trainControl(method = "cv",
                                          number = 5),
                 tuneGrid = data.frame(mtry = 1:3))
    })


  #Show the MLR model being computed
  output$mlrformula <- renderPrint({
    formula <- (paste(input$yvariable," ~ ",paste(input$mlrmodelinputs, collapse = "+")))
    print(formula)
  })

  #Show the Tree model being computed
  output$treeformula <- renderPrint({
    formula <- (paste(input$yvariable," ~ ",paste(input$treemodelinputs, collapse = "+")))
    print(formula)
  })

  #Show the Random Forest model being computed
  output$rfformula <- renderPrint({
    formula <- (paste(input$yvariable," ~ ",paste(input$rfmodelinputs, collapse = "+")))
    print(formula)
  })

  #Show the MLR model summary
  output$mlrmodel <- renderPrint({
    results <- summary(mlrFit())
    print(results)
  })

  #Show the Tree model summary
  output$treemodel <- renderPrint({
    results <- summary(treeFit())
    print(results)
  })

  #Show the random forest model summary
  output$rfmodel <- renderPrint({
    results <- summary(rfFit()$results)
    print(results)
  })

  
  #MLR model RMSE using the test data
  mlrpred  <- reactive({
    predmlrfit <- predict(mlrFit(), newdata = testdata())
    results <- postResample(predmlrfit, testdata()$SalePrice)
  })
  
  #Tree model RMSE using the test data
  treepred <- reactive({
    predtreefit <- predict(treeFit(), newdata = testdata())
    results <- postResample(predtreefit, testdata()$SalePrice)
  })
  
  #Random forest model RMSE using the test data
  rfpred <- reactive({
    predrffit <- predict(rfFit(), newdata = testdata())
    results <- postResample(predrffit, testdata()$SalePrice)
  })
  
  #Show the Results of the predictions using each model
  output$predictions <- renderPrint({
    results <- data.frame(treepred(), mlrpred(), rfpred())
    print(results)
  })



#Prediction variables

  #Function to output predictions for a selected model
  output$predmlrFit <- 
    renderPrint({
      #Predictions for the MLR model
      if (input$model == 1){
        formula <- (paste(input$yvariable," ~ ",paste(input$mlrmodelinputs, collapse = "+")))
        predmlrfit <- predict(mlrFit(), newdata = data.frame(
          LotArea=c(input$LotArea), 
          OverallQual = c(input$OverallQual),
          OverallCond = c(input$OverallCond),
          YearBuilt=c(input$YearBuilt),
          GrLivArea = c(input$GrLivArea),
          FullBath = c(input$FullBath),
          HalfBath = c(input$HalfBath),
          BedroomAbvGr = c(input$Bedrooms),
          GarageCars = c(input$GarageCars)))
        results <- paste("The predicted value is", predmlrfit)
        print(results)
      
      }
      #Predictions for the Tree model
      else if (input$model == 2){
        formula <- (paste(input$yvariable," ~ ",paste(input$treemodelinputs, collapse = "+")))
        predtreefit <- predict(treeFit(), newdata = data.frame(
          LotArea=c(input$LotArea), 
          OverallQual = c(input$OverallQual),
          OverallCond = c(input$OverallCond),
          YearBuilt=c(input$YearBuilt),
          GrLivArea = c(input$GrLivArea),
          FullBath = c(input$FullBath),
          HalfBath = c(input$HalfBath),
          BedroomAbvGr = c(input$Bedrooms),
          GarageCars = c(input$GarageCars)))
        results <- paste("The predicted sale price is", predtreefit)
        print(results)
      }
      #Predictions for the random forest model
      else{
        formula <- (paste(input$yvariable," ~ ",paste(input$rfmodelinputs, collapse = "+")))
        predrffit <- predict(rfFit(), newdata = data.frame(
          LotArea=c(input$LotArea), 
          OverallQual = c(input$OverallQual),
          OverallCond = c(input$OverallCond),
          YearBuilt=c(input$YearBuilt),
          GrLivArea = c(input$GrLivArea),
          FullBath = c(input$FullBath),
          HalfBath = c(input$HalfBath),
          BedroomAbvGr = c(input$Bedrooms),
          GarageCars = c(input$GarageCars)))
        results <- paste("The predicted value is", predrffit)
        print(results)    
        }
      })

# Create data table for data tab

  #Create a data set with the ID column and any column the user selects. Filter by user selected rows.
  data <- reactive({
    data <- house %>% select( Id, input$columns) %>% filter(Id >= input$firstrow &  Id <= input$lastrow)
  })

  #Code for showing the data table
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
