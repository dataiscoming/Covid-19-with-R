library(jsonlite)
library(shinytest)
library(reactlog)

# tell shiny to log all reactivity
options("shiny.reactlog" = TRUE)

# Record the test
recordTest('D:/#1_ART/2020/week_X_Covid_19_with_R')

#Deploy locally 
shiny::runApp('D:/#1_ART/2020/week_X_Covid_19_with_R')

# once app has closed, display reactlog from shiny
shiny::reactlogShow()

# Deploy on shinyapps.io
rsconnect::setAccountInfo(name='dataiscoming',
                          token='28B9F8B8BD3D676F954B6101C96F5E8F',
                          secret='4CPl7sZB2/CMoxh8ZS2t3XMw2s1RGti8hF+tZDkq')

rsconnect::deployApp(account = 'dataiscoming', "D:/#1_ART/2020/week_X_Covid_19_with_R", appName = "Covid-19",
                     appTitle = "Covid-19")

# test the app

usethis::use_test('D:/#1_ART/2020/week_X_Covid_19_with_R/codes/data.R')

