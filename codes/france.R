# Module France

franceUI <- function(id) {
  ns <- NS(id)
  
  material_side_nav_tab_content(
    side_nav_tab_id = "france",
    
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
                                   div(plotlyOutput(outputId = ns("france_barplot"),width = "100%", height = "100%"),
                                       align = "center")
                                 ))
    )
  )
  
}
  

###########################################################

france <- function(input,output,session,data = df){
  
  ################################################
  # Common steps
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
  
  ################################################
  # Number information
  df_inter <- data %>%
    filter(Alpha.3.code == "FRA") %>%
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
  
  ##############################################
  
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
  
  output$france_barplot <- renderPlotly({
    df3 <- df3()
    title <- paste0("Daily ",title(), " in France")
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