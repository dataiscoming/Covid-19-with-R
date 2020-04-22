#source("packages.R")

#curl/openssl/httr/rmarkdown
library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("shinymaterial")
library("shinydashboard")

source("D:/#1_ART/2020/week_X_Covid_19_with_R/codes/data.R",encoding = "UTF-8")
source("D:/#1_ART/2020/week_X_Covid_19_with_R/codes/world.R",encoding = "UTF-8")

options(shiny.reactlog = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output){

    #################################################
    # Module Data
    df <- callModule(module = data, id = "data")
    
    #################################################
    # Module world
    callModule(module = world, id = "world", data = df)
    
    #################################################
    # Module France
    callModule(module = france, id = "france", data = df)
    
    #################################################
    # Module Other Country
    
    #################################################
    # Module About
})
