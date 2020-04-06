source("packages.R")

shinyUI(material_page(
        nav_bar_color = "blue",
        
        title = "Covid-19",
        shinyjs::useShinyjs(),
        #tags$head(
         #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          #  tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
           # tags$script(type = "text/javascript", src = "jquery_browser.js")
        #),
        #tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
        
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
            #image_source = "img/corona-4938929_1920.jpg",
            #tags$a(class="sidenav-close", "data-target"="slide-out", href="#", "x"),
            # Place side-nav tabs within side-nav
            material_side_nav_tabs(
                side_nav_tabs = c(
                    "World Map" = "world_map",
                    "France" = "france"
                )#,
                #icons = c("map", "format_list_bulleted", "insert_chart", "exposure", "local_pizza", "toys", "info_outline")
            )#,
            #div(style="height:50px"),
            #material_card(
                
             #   htmlOutput("last_modified")
            #)
        ),
        
        #---- UI objects ----
        material_side_nav_tab_content(
            side_nav_tab_id = "world_map",
            #ns <- NS(id)
            tagList(
                material_row(
                    material_card(
                        title = "World map",
                        #tags$p("Please note: On 2019-03-23 the level of details changed for the USA. The explosion in cases you will see is due to changes
               #in geographical details in ", tags$a(href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "CSSE Date", target="_new"), " not to a real explosion of cases."),
                        
                        #uiOutput(ns("slider"))
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
                        #tags$p("Please note: On 2019-03-23 the level of details changed for the USA. The explosion in cases you will see is due to changes
                        #in geographical details in ", tags$a(href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "CSSE Date", target="_new"), " not to a real explosion of cases."),
                        
                        #uiOutput(ns("slider"))
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
