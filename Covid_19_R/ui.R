# Call of the packages needed
# Define UI for the left nav bar
# Call of the 4 modules : World, France, Other country and About

# Packages
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
library("log4r")

# modules
source("./codes/modules/world.R",encoding = "UTF-8")
source("./codes/modules/france.R",encoding = "UTF-8")
source("./codes/modules/other_country.R",encoding = "UTF-8")
source("./codes/modules/about.R",encoding = "UTF-8")

# UI definition
shinyUI(material_page(
    
    useShinyjs(),
    
    # Nav bar at the rop
    title = "Covid-19",
    nav_bar_color = "Orange",
    nav_bar_fixed = FALSE,
    
    # favicon
    tags$head(tags$link(rel="shortcut icon", href="logo.jpg")),
    
    # Side nav panel
    material_side_nav(
        fixed = FALSE, 
        background_color = "White",
            
        # Logo of the blog
        material_card(
            depth = 0,
            HTML("
            <center>
                <a href='http://www.dataiscoming.fr'>
                    <img style='height:160px;width:160px;' src='logo.jpg'>
                </a>
            </center>
                 ")
        ),

        # Tab for the different panels in the side bar
        material_side_nav_tabs(
            side_nav_tabs = c(
                "World" = "world_map",
                "France" = "france",
                "Other Country" = "other_country",
                "About" = "about"
            )
        )
    ),
    
    # 
    div(
        id = "loading_page",
        withLoader(h1(),type="html", loader="loader3")
    ),
        
    #
    hidden(
        div(id = "main_content",
            
        # Module World
        worldUI('world'),
    
        # Module France
        franceUI('france'),
        
        # Module Other country
        other_countryUI('other_country'),
        
        # Module About
        aboutUI('about')  )
    )
))
