# st558_final

# Purpose of this App

This app should inspire the minds and hearts of all who see it. At a minimum, it should allow the user to explore how housing sales prices are related to several numeric variables. It offers the opportunity to compare various models for predicting Sale Prices and also allows the user to make predictions.

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
