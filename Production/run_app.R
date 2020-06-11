# Packages
library(reactlog)
library(profvis)

# tell shiny to log all reactivity
options("shiny.reactlog" = TRUE)

# Record the test
recordTest('D:/#1_ART/2020/week_X_Covid_19_with_R')

# Deploy locally 
shiny::runApp('D:/#1_ART/2020/week_X_Covid_19_with_R',port = 6012)

# once app has closed, display reactlog from shiny
shiny::reactlogShow()

# check the traceback of the app
traceback()

#################################################################

# Deploy on shinyapps.io
readRenviron("../Production/var.env")

rsconnect::setAccountInfo(name=Sys.getenv("NAME"),
                          token=Sys.getenv("TOKEN"),
                          secret=Sys.getenv("SECRET"))

rsconnect::listBundleFiles("D:/#1_ART/2020/week_X_Covid_19_with_R")

rsconnect::deployApp(account = 'dataiscoming', 
                     appDir ="D:/#1_ART/2020/week_X_Covid_19_with_R/COVID_19_R",
                     appName = "Covid-19",
                     appTitle = "Covid-19")

###################################################################
#creating a shortcut
library(shinyShortcut)
shinyShortcut()
