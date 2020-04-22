# world module

library(dplyr)
library(shinydashboard)

worldUI <- function(id) {
  ns <- NS(id)
  
  material_side_nav_tab_content(
    side_nav_tab_id = "world_map",
    
    tags$br(),
    
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          material_column(
            width = 3,div(style = "text-align:center",
            material_card(title = "Total cases", depth = 3, 
                          div(style = "height:75px",uiOutput(ns("wm1")))))
          ),
          material_column(
            width = 3,div(style = "text-align:center",
            material_card(title = "Active cases",depth = 3, 
                          div(style = "height:75px",uiOutput(ns("wm2")))))
            ),
          material_column(
            width = 3,div(style = "text-align:center",
            material_card(title = "Deaths", depth = 3, 
                          div(style = "height:75px",uiOutput(ns("wm3")))))
          ),
          material_column(
            width = 3,div(style = "text-align:center",
            material_card(title = "Recovered", depth = 3, 
                          div(style = "height:75px",uiOutput(ns("wm4")))))
          )
          ))),
    
    tags$br(),
    
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          material_column(
            width = 8,
            material_card(depth = 3,div(style = "height:100px",sliderInput(inputId = ns("slider_date"),
                        label="Date:",
                        min = as.Date("2020-02-01"),
                        max = as.Date(Sys.Date()-1),
                        value = as.Date(Sys.Date()-1),
                        timeFormat="%Y-%m-%d",
                        animate = animationOptions(interval = 200,playButton = "Play",pauseButton = "Pause"),width = "100%"),
                        align = "center"))
          ),
          material_column(
            width = 4,
            material_card(depth = 3,div(style = "height:100px",selectInput(inputId = ns("c1"), label = "Variable:",
                        c("Total cases" = "cc",
                          "Active cases" = "ac",
                          "Death" = "d",
                          "Recovered" = "r",
                          "New confirmed cases"="ncc",
                          "New active cases"="nac",
                          "New death"="nd",
                          "New recovered"="nr"
                        ))),align = "center"
          )
        )))),
    
      material_row(material_column(width=10,offset = 1,
        material_card(
                      #tableOutput("table"),
                      div(plotlyOutput(outputId = ns("WorldMap"),width = "100%", height = "100%"),align = "center")
        ))
      ),
    
    material_row(material_column(width=10,offset = 1,
                                 material_card(
                                   div(plotlyOutput(outputId = ns("World_barplot"),width = "100%", height = "100%"),
                                       align = "center")
                                 ))
    )
    
  )
}

###############################################

world <- function(input, output, session, data = df){
  # Function Definition to create the world map
  df2 <- reactive({
    df1 <- data %>%
      filter(date==input$slider_date) %>%
      mutate(show = case_when(input$c1 == 'cc' ~ value_confirmed,
                              input$c1 == 'ac' ~ value_active,
                              input$c1 == 'd' ~ value_death,
                              input$c1 == 'r' ~ value_recovered,
                              input$c1 == 'ncc' ~ new_confirmed_case,
                              input$c1 == 'nac' ~ new_active_case,
                              input$c1 == 'nd' ~ new_death,
                              input$c1 == 'nr' ~ new_recovered))
    return(df1)
  })
  
  title <- reactive({
    if(input$c1 == 'cc'){title = "total cases"
    }else if(input$c1 == 'ac'){title = "active cases"
    }else if(input$c1 == 'd'){title = "death"
    }else if(input$c1 == 'r'){title = "recovered"
    }else if(input$c1 == 'ncc'){title = "new confirmed cases"
    }else if(input$c1 == 'nac'){title = "new active cases"
    }else if(input$c1 == 'nd'){title = "new death"
    }else if(input$c1 == 'nr'){title = "new recovered"}
    return(title)
  })
  
  #output$table = renderTable(df2())
  
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  #RColorBrewer::brewer.pal("Greens",n=9)
  
  col <- reactive({
  if(input$c1 %in% c('cc','ncc')){col = list(c(0, "#FFF5F0"), c(0.5, "#EF3B2C"), c(1, "#67000D"))
  }else if(input$c1 %in% c('ac','nac')){col = list(c(0, "#FFF5F0"), c(0.5, "#EF3B2C"), c(1, "#67000D"))
  }else if(input$c1 %in% c('d','nd')){col = list(c(0, "#FFFFFF"), c(0.5, "#737373"), c(1, "#000000"))
  }else if(input$c1 %in% c('r','nr')){col = list(c(0, "#F7FCF5"), c(0.5, "#41AB5D"), c(1, "#00441B"))}
  return(col)
  })
  
  output$WorldMap <- renderPlotly({
    df2<-df2()
    title <- paste0("Map of ",title())
    
    fig_wm <- plot_geo(df2())  %>% 
      add_trace(
        z = ~show, 
        color = ~show,
        colorscale = col(),
        text = ~Country, 
        locations = ~Alpha.3.code,
        marker = list(line = l),
        colorbar = list(title = 'Number', tickprefix = '',limits = c(0, max(df2()$show)),
                        x=1,y=0.75,len=0.5)
      ) %>%
      layout(title=title)
    #layout(geo = g)
    
    return(fig_wm)
  })
  
  ###########################################################################
  
   df_inter <- data %>%
    group_by(date) %>%
    summarise(max_cc = sum(value_confirmed),
              max_ac = sum(value_active),
              max_d = sum(value_death),
              max_r = sum(value_recovered),
              max_ncc = sum(new_confirmed_case),
              max_nac = sum(new_active_case),
              max_nd = sum(new_death),
              max_nr = sum(new_recovered))
  
  df_world_max <- df_inter %>%
    filter(date == max(data$date))
  
  output$wm1 <- renderUI({
  HTML(
    paste0(
      "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_cc,big.mark =" "),
      "</span></div>"
    ))
  })

  output$wm2 <- renderUI({
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_ac,big.mark =" "),
        "</span></div>"
      ))
  })
  
  output$wm3 <- renderUI({
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_d,big.mark =" "),
        "</span></div>"
      ))
  })
  
  output$wm4 <- renderUI({
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_r,big.mark =" "),
        "</span></div>"
      ))
  })
  
  ##########################################################
  
  # bar plot
  
  df3 <- reactive({
    df_inter %>%
      mutate(show = case_when(input$c1 == 'cc' ~ max_cc,
                              input$c1 == 'ac' ~ max_ac,
                              input$c1 == 'd' ~ max_d,
                              input$c1 == 'r' ~ max_r,
                              input$c1 == 'ncc' ~ max_ncc,
                              input$c1 == 'nac' ~ max_nac,
                              input$c1 == 'nd' ~ max_nd,
                              input$c1 == 'nr' ~ max_nr))
  })
  
  col2 <- reactive({
    if(input$c1 %in% c("cc","ncc","ac","nac")){col = '#EF3B2C'
    }else if(input$c1 %in% c("d","nd")){col = "#737373"
    }else if(input$c1 %in% c("r","nr")){col= "#41AB5D"}
    return(col)
  })
  
  output$World_barplot <- renderPlotly({
    df3 <- df3()
    title <- paste0("Daily ",title())
    col2 <- col2()
    
    fig_wbp <- plot_ly(data = df3,
                       x = ~date,
                       y = ~show,
                       name = "",
                       type = "bar",
                       marker = list(color = col2)
    ) %>% 
      layout(title = title,
        yaxis = list(
        title = "Number of cases",
        zeroline=F
      ))
    return(fig_wbp)
  })
  
  
}

