#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage(
  # Application title
  title = "HousingPrices",
  
  # About page panel
  tabPanel("About",
             img(src="house.png",height="75%", width="75%"),
           
           

           h1("Purpose"),
           h2("This app is an exploration of housing prices. Several numeric variables are able to be analyzed and used as predictors for the response, the sale price."),
           h1("Data description"),
           h2("This data was taken from a Kaggle competion. There are 1460 observations on 163 variables though only a subset are accessible through this app.",
              br(),
           a("Data Source",href="https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview")),
           h1("Page descriptions"),
           h2("Data Exploration"),
           h3("This page allows the user to explore the distribution of a selected variable and view summary statistics. It also allows the user to plot two variables on a scatterplot and find the line of best fit between the selected variables."),
           br(),
           h2("Modeling page"),
           h3("The modeling page consists of 3 sub pages. The first page allows the user to create a train and test data set and then apply that data set to a model of their choosing. There are three models that will be computed: Multiple linear regression, classification tree, and random forest. The user is able to choose the predictors for each model and the results will show how well each model performs on the test data set."),
           h3("The prediction page allows the use to choose one of their models and then make a prediction by inputting values for the variables in the model."),
           br(),
           h2("Data page"),
           h3("The user is able to construct a subset of the available data and download it for their own enjoyment."),
      ),#End the about panel
  
  #EDA Panel
  tabPanel("Data Exploration",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
      
      #Choose x variable for data exploration
      selectInput("xvar",
                  "Select X variable",
                  c("Sale Price" = "SalePrice",
                    "Lot Area" = "LotArea",
                    "Overall Quality" = "OverallQual",
                    "Overall Condition"="OverallCond",
                    "YearBuilt",
                    "Square footage" ="GrLivArea",
                    "Full Baths" = "FullBath",
                    "Half Baths" = "HalfBath",
                    "Bedrooms"= "BedroomAbvGr",
                    "Garage Cars" = "GarageCars")),
      
      #Choose y variable for exploration
      selectInput("yvar",
                  "Select Y variable",
                  c("Sale Price" = "SalePrice",
                    "Lot Area" = "LotArea",
                    "Overall Quality" = "OverallQual",
                    "Overall Condition"="OverallCond",
                    "YearBuilt",
                    "Square footage" ="GrLivArea",
                    "Full Baths" = "FullBath",
                    "Half Baths" = "HalfBath",
                    "Bedrooms"= "BedroomAbvGr",
                    "Garage Cars" = "GarageCars")),
      
      # Give options for graphs based on variable selected
      radioButtons("GraphSelect",
                  "Choose a graph",
                  c("Histogram of X Variable" = "1",
                    "Histogram of Y Variable" = "2",
                    "Scatter Plot of X VS Y" = "3")),
        #If Scatter plot is chosen give option of showing line of best fit
        conditionalPanel(
          condition = "input.GraphSelect == '3'",
          checkboxInput("LSRL", "Add best fit line", FALSE)),
      
        # Subset the data based on variable selected
        sliderInput("xValueselect", "Select a range for the x variable",
            min = 1, max = 10,  value = c(1,10), ticks=FALSE),

        sliderInput("yValueselect", "Select a range for the y variable",
            min = 1, max = 10,  value = c(1,10), ticks=FALSE),

       
      ), #This ends sidebarPanel
    

    mainPanel(
       #Show the selected graph
       plotOutput("edaPlot"),
       #Show the best fit line if selected
       textOutput("LSRL"),
       #Show the summary statistics
       tableOutput("LSRLsummary"),
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), tableOutput("xTable"), tableOutput("yTable"))
          )

    )# This ends mainPanel 
  ) # This ends Data Exploration tab
),

  #Create tabs for the modeling page
  navbarMenu("Modeling", 
    #Modeling info page
    tabPanel("Modeling Info", 
      sidebarLayout(
        sidebarPanel(
          h1("Words!")
                    ), #This ends sidebarPanel
        mainPanel(
                        
                  )#This ends mainPanel
                    ) #This ends sidebarLayout
            ), #This ends Modeling info tab
    tabPanel("Model Fitting", 
      sidebarLayout(
        sidebarPanel(
          #Input for creating the test/train proportions
          selectInput("train", 
                      label = "Select proportion of data to be used for training", c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), 
                      selected = .8),
          
          #Input to execute creation of test/train data 
          actionButton("splitbutton", 
                       "Click to create train and test data"),
          #Response variable
          selectInput("yvariable", "Response Variable", c("Sale Price" = "SalePrice")),
          #Predictor variable options for MLR model
          checkboxGroupInput('mlrmodelinputs',
                             'Variables for MLR model', 
                             choices = c("Lot Area" = "LotArea",
                                         "Overall Quality" = "OverallQual",
                                         "Overall Condition"="OverallCond",
                                         "YearBuilt",
                                         "Square footage" ="GrLivArea",
                                         "Full Baths" = "FullBath",
                                         "Half Baths" = "HalfBath",
                                         "Bedrooms"= "BedroomAbvGr",
                                         "Garage Cars" = "GarageCars")),
          #Precitor variable options for tree model
          checkboxGroupInput('treemodelinputs',
                              'Variables for Tree model',
                              choices = c("Lot Area" = "LotArea",
                                          "Overall Quality" = "OverallQual",
                                          "Overall Condition"="OverallCond",
                                          "YearBuilt",
                                          "Square footage" ="GrLivArea",
                                          "Full Baths" = "FullBath",
                                          "Half Baths" = "HalfBath",
                                          "Bedrooms"= "BedroomAbvGr",
                                          "Garage Cars" = "GarageCars")),
          #Predictor variable options for random forest model
          checkboxGroupInput('rfmodelinputs',
                              'Variables for random forest model',
                              choices = c("Lot Area" = "LotArea",
                                          "Overall Quality" = "OverallQual",
                                          "Overall Condition"="OverallCond",
                                          "YearBuilt",
                                          "Square footage" ="GrLivArea",
                                          "Full Baths" = "FullBath",
                                          "Half Baths" = "HalfBath",
                                          "Bedrooms"= "BedroomAbvGr",
                                          "Garage Cars" = "GarageCars")),
          #Button to execute model construction
          actionButton("button", "Run Models"),
                        
        ), #This ends sidebarPanel
          mainPanel(
            h1("Data set properties"),
            textOutput("datasets"),
            h1("MLR model results"),
            verbatimTextOutput("mlrformula"),
            verbatimTextOutput("mlrmodel"),
            h1("Tree model results"),
            verbatimTextOutput("treeformula"),
            verbatimTextOutput("treemodel"),
            h1("Random Forest model results"),
            verbatimTextOutput("rfformula"),
            verbatimTextOutput("rfmodel"),
            h1("Comparison of models using test data"),
            verbatimTextOutput("predictions"),
            verbatimTextOutput("result")
          )#This ends mainPanel
        )#This ends sidebarLayout
      ),#This ends Modeling info tab
    tabPanel("Prediction", 
      sidebarLayout(
        sidebarPanel(
          #Option for user to select a model to make predictions
          selectInput("model",
                      label = "Select model for prediction", 
                      c("MLR" = "1", "Tree"="2", "Random Forest"="3")),
          #Options for inputing values for variables to make predictions
          numericInput("LotArea", "Lot Area", value = NULL),
          numericInput("OverallQual", "Overall Quality", value = NULL),
          numericInput("OverallCond", "Overall Condition", value = NULL),
          numericInput("YearBuilt", "Year Built", value = NULL),
          numericInput("GrLivArea", "Square Footage", value = NULL),
          numericInput("FullBath", "Full Bath", value = NULL),
          numericInput("HalfBath", "Half Bath", value = NULL),
          numericInput("Bedrooms", "Bedrooms", value = NULL),
          numericInput("GarageCars", "Garage Cars", value = NULL),
        ), #This ends sidebarPanel
        
        mainPanel(
          h1("Model selected"),
          textOutput("predmlrFit"),
        )#This ends mainPanel
      )#This ends sidebarLayout
    ),#This ends Prediction tab
  ),

         
    tabPanel("Data",
      sidebarLayout(
        sidebarPanel(
          #Give the user options for columns to display   
          checkboxGroupInput('columns',
                             'Columns to select',
                             choices = c("Lot Area" = "LotArea",
                                         "Overall Quality" = "OverallQual",
                                         "Overall Condition"="OverallCond",
                                         "YearBuilt",
                                         "Square footage" ="GrLivArea",
                                         "Full Baths" = "FullBath",
                                         "Half Baths" = "HalfBath",
                                         "Bedrooms"= "BedroomAbvGr",
                                         "Garage Cars" = "GarageCars")),
           numericInput("firstrow", "Select first row by Id number", value = 1),
           numericInput("lastrow", "Select last row by Id number", value = 10),
        ), #This ends sidebarPanel
        mainPanel(
          DT::dataTableOutput("fancyTable")
        )#This ends mainPanel
      ) #This ends sidebarLayout
    )#This ends Data tab 
  ) #This ends navbarPage
))#This ends ShinyUI Fluidpage
