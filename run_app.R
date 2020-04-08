#Deploy locally 
shiny::runApp('D:/#1_ART/2020/week_X_Covid_19_with_R/#2_codes')

# Deploy on shinyapps.io
rsconnect::setAccountInfo(name='dataiscoming',
                          token='28B9F8B8BD3D676F954B6101C96F5E8F',
                          secret='4CPl7sZB2/CMoxh8ZS2t3XMw2s1RGti8hF+tZDkq')

rsconnect::deployApp("D:/#1_ART/2020/week_X_Covid_19_with_R/codes", appName = "Covid-19",
                     appTitle = "Covid-19")
