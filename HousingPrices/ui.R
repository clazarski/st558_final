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
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )# This ends SidebarPanel 
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
