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
  tabPanel("About",
           h1("This is the about page")
           #End the about panel
           ),
  tabPanel("Data Exploration",
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Choose variables for data exploration
      selectInput("xvar", "Select X variable",c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr")),
      
      selectInput("yvar", "Select Y variable",c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr")),
      # Give graph options based on variable selected
      radioButtons("GraphSelect",
                  "Choose a graph",
                  c("Histogram of X Variable" = "1", "Histogram of Y Variable" = "2","Scatter Plot of X VS Y" = "3")),
      conditionalPanel(
        condition = "input.GraphSelect == '3'",
        checkboxInput("LSRL", "Add best fit line", FALSE)),
      
# Subset the data

sliderInput("xValueselect", "Select a range for the x variable",
            min = 1, max = 10,  value = c(1,10), ticks=FALSE),

sliderInput("yValueselect", "Select a range for the y variable",
            min = 1, max = 10,  value = c(1,10), ticks=FALSE),

       
    ), #This ends sidebarPanel
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("edaPlot"),
       textOutput("check"),
       textOutput("LSRL"),
       tableOutput("LSRLsummary"),
       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"), tableOutput("xTable"), tableOutput("yTable"))
       )

    )# This ends mainPanel 
  ) # This ends Data Exploration tab
),
navbarMenu("Modeling", 
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
                        selectInput("train", label = "Select proportion of data to be used for training", c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), selected = .8),
                        actionButton("splitbutton", "Create data sets"),
                        selectInput("yvariable", label = "Y variable", "SalePrice"),
                        checkboxGroupInput('mlrmodelinputs','Variables for MLR model', choices = c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr")),
                        checkboxGroupInput('treemodelinputs','Variables for Tree model', choices = c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr")),
                        checkboxGroupInput('rfmodelinputs','Variables for random forest model', choices = c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr")),
                        actionButton("button", "Run Models"),
                        
                        
                        
                       
                      ), #This ends sidebarPanel
                      mainPanel(
                        h1("MLR model results"),
                        verbatimTextOutput("mlrmodel"),
                        h1("Tree model results"),
                        verbatimTextOutput("treemodel"),
                        h1("Random Forest model results"),
                        verbatimTextOutput("rfmodel"),
                        h1("Comparison of models using test data"),
                        verbatimTextOutput("predictions"),
                        verbatimTextOutput("result")
                      )#This ends mainPanel
                    ) #This ends sidebarLayout
           ), #This ends Modeling info tab
           tabPanel("Prediction", 
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        h1("Words!")
                      ), #This ends sidebarPanel
                      mainPanel(
                        
                      )#This ends mainPanel
                    ) #This ends sidebarLayout
           ), #This ends Prediction tab
),

         
tabPanel("Data",
         sidebarLayout(
           sidebarPanel(
             
             selectInput("Column", label = "Choose column", c("Sale Price" = "SalePrice","Lot Area" = "LotArea", "Neighborhood", "Building Type"="BldgType",  "YearBuilt", "Square footage" ="GrLivArea", "Full Baths" = "FullBath", "Half Baths" = "HalfBath", "Bedrooms"= "BedroomAbvGr"), multiple = TRUE),
           ), #This ends sidebarPanel
           mainPanel(
             DT::dataTableOutput("fancyTable")
           )#This ends mainPanel
         ) #This ends sidebarLayout
         
)#This ends Data tab 

  #This ends navbarPage
  )
  #This ends ShinyUI Fluidpage
))
