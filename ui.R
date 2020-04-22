#source("packages.R")

library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("shinymaterial")
library("shinydashboard")

source("D:/#1_ART/2020/week_X_Covid_19_with_R/codes/world.R",encoding = "UTF-8")
source("D:/#1_ART/2020/week_X_Covid_19_with_R/codes/france.R",encoding = "UTF-8")

shinyUI(material_page(
    
        nav_bar_fixed = FALSE,
        nav_bar_color = "Orange",
        
        title = "Covid-19",
        shinyjs::useShinyjs(),
        tags$head(
        tags$head(tags$link(rel="shortcut icon", href="logo.jpg"))
        ),
        div(id = "wait", style = "    position: fixed;
        top: 0rem;
        margin: auto;
        height: 100px;
        width: 100px;
        margin: 20% auto; /* Will not center vertically and won't work in IE6/7. */
        left: 0;
        right: 0;
        opacity: 1;"),
        
        #---- material_side_nav ----
        material_side_nav(
            fixed = FALSE, 
            background_color = "white",
            image_source = "logo.jpg",
            
            # Place side-nav tabs within side-nav
            material_side_nav_tabs(
                side_nav_tabs = c(
                    "World" = "world_map",
                    "France" = "france",
                    "Other Country" = "other_country",
                    "About" = "about"
                )
            )
        ),
        
        #---- UI objects ----
        worldUI('world'),
        #tableOutput('table'),
        
        # Module France
        franceUI('france')
    )
)
