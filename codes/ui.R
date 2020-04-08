#source("packages.R")

library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("shinymaterial")

shinyUI(material_page(
        nav_bar_color = "blue",
        
        title = "Covid-19",
        shinyjs::useShinyjs(),
        
        div(id = "wait", style = "    position: fixed;
        top: 0rem;
        margin: auto;
        height: 100px;
        width: 100px;
        margin: 20% auto; /* Will not center vertically and won't work in IE6/7. */
        left: 0;
        right: 0;
        /* display: none; */
        opacity: 1;"),
        
        #---- material_side_nav ----
        material_side_nav(
            fixed = TRUE, 
            background_color = "white",
        
            # Place side-nav tabs within side-nav
            material_side_nav_tabs(
                side_nav_tabs = c(
                    "World Map" = "world_map",
                    "France" = "france"
                )
            )
        ),
        
        #---- UI objects ----
        material_side_nav_tab_content(
            side_nav_tab_id = "world_map",
            tagList(
                material_row(
                    material_card(
                        title = "World map",
                    ),
                material_row(
                   material_card(
                       material_radio_button(input_id = "indicator_radio_button",
                                          label = "Indicator : ",
                                          choices = c(
                                              "Confirmed cases" = "cc",
                                              "Active cases" = "ac",
                                              "Death" = "d",
                                              "Recovered" = "r"
                                          ),
                                          color = "#ef5350")
                   ))
                ),
                material_row(
                    material_column(width=12,
                                    plotlyOutput(outputId = "WorldMap",width = "100%", height = "1000")
                    )
                )
            ),
            
            tagList(
                tags$head(
                    tags$style(
                        "#indicator_radio_button {display: flex;}"
                    )
                )
            )
        ),
        material_side_nav_tab_content(
            side_nav_tab_id = "france",
            #ns <- NS(id)
            tagList(
                material_row(
                    material_card(
                        title = "France",
                    )
                ),
                material_row(
                    material_column(width=12,
                                    plotlyOutput(outputId = "France_barplot",width = "100%", height = "800")
                    )
                )
            )
        )
        
    )
)
