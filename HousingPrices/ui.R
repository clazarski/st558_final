#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

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
tabPanel("Modeling",
         sidebarLayout(
           sidebarPanel(
             h1("Words!")
           ), #This ends sidebarPanel
           mainPanel(
             plotOutput("distPlotmodel")
           )#This ends mainPanel
          ) #This ends sidebarLayout
        
        ), #This ends Modeling tab

tabPanel("Data",
         sidebarLayout(
           sidebarPanel(
             h1("Words!")
           ), #This ends sidebarPanel
           mainPanel(
             h1("More words!")
           )#This ends mainPanel
         ) #This ends sidebarLayout
         
) #This ends Data tab         
  #This ends navbarPage
  )
  #This ends ShinyUI Fluidpage
))
