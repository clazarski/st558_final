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
           h1("This is the about page")
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
          selectInput("yvariable", "Response Variable", c("Sale Price" = "SalePrice")),
          #Input to execute creation of test/train data 
          actionButton("splitbutton", 
                       "Click to create train and test data"),
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
