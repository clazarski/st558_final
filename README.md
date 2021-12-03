# st558_final

# Purpose of this App

<<<<<<< HEAD
This app should inspire the minds and hearts of all who see it. At a minimum, it should allow the user to explore how housing sales prices are related to several numeric variables. It offers the opportunity to compare various models for prediction Sale Prices and also allows the user to make predictions. 

Why only numeric variables you ask? Because I only had time to figure that out. Categorical variables tend to break things.
=======
This app should inspire the minds and hearts of all who see it. At a minimum, it should allow the user to explore how housing sales prices are related to several numeric variables. It offers the opportunity to compare various models for predicting Sale Prices and also allows the user to make predictions.
>>>>>>> 12ff87740be0f9c113645904e4cd26ced23f7871

# Packages to run the app

library(shiny)
library(tidyverse)
library(DT)
library(tree)
library(caret)
library(randomForest)

# Install packages

my_packages <- c("shiny", "tidyverse", "DT", "tree", "caret", "randomForest")  
lapply(my_packages, require, character.only = TRUE) 

# Code to run app

shiny::runApp('HousingPrices')
