# Call of the packages needed, define UI 
# Call of the function data
# Call of the 4 modules : World, France, Other country and About

# packages
#curl/openssl/httr/rmarkdown
library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("shinymaterial")
library("data.table")

# modules
source("./codes/functions/data.R",encoding = "UTF-8")
source("./codes/modules/world.R",encoding = "UTF-8")
source("./codes/modules/france.R",encoding = "UTF-8")
source("./codes/modules/other_country.R",encoding = "UTF-8")
source("./codes/modules/about.R",encoding = "UTF-8")

# React log to follow the reactivity 
options(shiny.reactlog = TRUE)

# Define server 
shinyServer(function(input, output){

    #################################################
    # Function Data
    df <- data()
    
    #################################################
    # Module world
    callModule(module = world, id = "world", data = df)
    
    #################################################
    # Module France
    callModule(module = france, id = "france", data = df)
    
    #################################################
    # Module Other Country
    callModule(module = other_country, id = "other_country", data = df)
    
    #################################################
    # Module About
    callModule(module = about, id = 'about',data=df)
})
