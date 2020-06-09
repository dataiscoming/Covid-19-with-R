# Call of the packages needed, define UI 
# Call of the function data
# Call of the 4 modules : World, France, Other country and About

# packages
#curl/openssl/httr/rmarkdown
library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr", warn.conflicts = FALSE)
library("plotly")
library("shinymaterial")
library("data.table")
library("lubridate")
library("shinycustomloader")
library("RCurl")

log_file <- "applog.log"
file_logger <- logger("INFO", appenders = file_appender(log_file))
info(file_logger, paste("1 - Begin the app - ready to get files."))

# modules
source("./codes/functions/data2.R",encoding = "UTF-8")
#source("./codes/modules/world.R",encoding = "UTF-8")
#source("./codes/modules/france.R",encoding = "UTF-8")
#source("./codes/modules/other_country.R",encoding = "UTF-8")
#source("./codes/modules/about.R",encoding = "UTF-8")

info(file_logger, paste("2 - Begin the app - files got."))

# React log to follow the reactivity 
options(shiny.reactlog = TRUE)

# Define server 
shinyServer(function(input, output){
    
    info(file_logger, paste("3 - Begin the app server."))
    
    #################################################
    # Function Data
    df <- data()
    
    # Hiding the loading page and show the tabs
    useShinyjs()
    shinyjs::hide(id="loading_page",animType = "fade")
    shinyjs::show(id="main_content")
    
    #################################################
    # Module world
    #callModule(module = world, id = "world", data = df)
    
    #################################################
    # Module France
    #callModule(module = france, id = "france", data = df)
    
    #################################################
    # Module Other Country
    #callModule(module = other_country, id = "other_country", data = df)
    
    #################################################
    # Module About
    #callModule(module = about, id = 'about',data=df)
})
