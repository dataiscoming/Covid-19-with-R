# Other module :
# UI --> 1 input to select a country
#    --> 4 outputs to show figures
#    --> 2 inputs to select date and variables 
#    --> 1 output that is the barplot
# SERVER --> Define the reactive UI to choose a country
#        --> use data from the module data
#        --> define reactive values : title, color, country data
#        --> 5 outputs : 4 figures and 1 barplot

# Define the server
other_countryUI <- function(id) {
  ns <- NS(id)
  
  material_side_nav_tab_content(
    side_nav_tab_id = "other_country",
    
    tags$br(),
    
    # Select input to choose the country
    material_row(
      material_column(
        width = 10,
        offset = 1,
        div(style = "text-align:center",
            material_card(
              title = "Country", 
              depth = 3, 
              align = "center",
              div(style = "height:75px",
                  uiOutput(outputId = ns("choose_country_input"))
                  ))))
    ),
    
    # Four box in a row to show figures
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          # Show the number of total confirmed cases in a box
          material_column(
            width = 3,
            div(style = "text-align:center",
                material_card(
                  title = "Total cases",
                  depth = 3, 
                  div(style = "height:75px",uiOutput(ns("wm1")))))
          ),
          
          # Show the number of total actives cases in a box
          material_column(
            width = 3,
            div(style = "text-align:center",
                material_card(
                  title = "Active cases",
                  depth = 3, 
                  div(style = "height:75px",uiOutput(ns("wm2")))))
          ),
          
          # Show the number of Death actives cases in a box
          material_column(
            width = 3,
            div(style = "text-align:center",
                material_card(
                  title = "Deaths", 
                  depth = 3, 
                  div(style = "height:75px",uiOutput(ns("wm3")))))
          ),
          
          # Show the number of total recovered cases in a box
          material_column(
            width = 3,
            div(style = "text-align:center",
                material_card(
                  title = "Recovered", 
                  depth = 3, 
                  div(style = "height:75px",uiOutput(ns("wm4")))))
    )))),
    
    tags$br(),
    
    # Two boxes in a row select the input
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          
          # Input : slider date to select a date for the map and plot
          material_column(
            width = 8,
            material_card(
              depth = 3,
              align = "center",
              div(
                style = "height:100px",
                sliderInput(
                  inputId = ns("slider_date"),
                  label="Date:",
                  min = as.Date("2020-02-01"),
                  max = as.Date(Sys.Date()-1),
                  value = as.Date(Sys.Date()-1),
                  timeFormat="%Y-%m-%d",
                  animate = animationOptions(
                    interval = 200,
                    playButton = "Play",
                    pauseButton = "Pause"),
                  width = "100%")))
          ),
        
          # Input : Select one input that is the variable used for the map and plot 
          material_column(
            width = 4,
            material_card(
              depth = 3,
              align = "center",
              div(
                style = "height:100px",
                selectInput(
                  inputId = ns("c1"), 
                  label = "Variable:",
                  selectize = TRUE,
                  c("Total cases" = "cc",
                    "Active cases" = "ac",
                    "Death" = "d",
                    "Recovered" = "r",
                    "New confirmed cases"="ncc",
                    "New active cases"="nac",
                    "New death"="nd",
                    "New recovered"="nr"))))
            )))
    ),
    
    # Output : Show the barplot according to the input selected above
    material_row(
      material_column(
        width=10,
        offset = 1,
          material_card(
            div(
              align = "center",
              plotlyOutput(
                outputId = ns("other_barplot"),
                width = "100%", 
                height = "100%")
              )))
        )
  )
} # End of UI definition

###########################################################

# Server  definition
other_country <- function(input,output,session,data = df){
  ns <- session$ns
    
  ################################################
  # Common steps
  
  # The select input for the country at the top of the UI
  output$choose_country_input = renderUI({
    countries <- unique(data$Country)
    selectInput(
      inputId = ns("choose_country"),
      label = "",
      choices = countries) 
  })
  
  # Define the data used in the country selected (not reactive) for the infobox and barplot
  df_inter <- reactive({
    data %>%
      filter(Country == input$choose_country) %>%
      group_by(date) %>%
      summarise(max_cc = sum(value_confirmed),
                max_ac = sum(value_active),
                max_d = sum(value_death),
                max_r = sum(value_recovered),
                max_ncc = sum(new_confirmed_case),
                max_nac = sum(new_active_case),
                max_nd = sum(new_death),
                max_nr = sum(new_recovered))
  })
  
  # Define the reactive title
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
  
  # Define the max value orther different variables from df_inter
  df_world_max <- reactive({
    df_inter() %>%
      filter(date == max(data$date))
  }) 
  
  # Box defintion 1 : total confirmed cases
  output$wm1 <- renderUI({
    df_world_max <- df_world_max()
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_cc,big.mark =" "),
        "</span></div>"
      ))
  })
  
  # Box definition 2 : total active cases
  output$wm2 <- renderUI({
    df_world_max <- df_world_max()
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_ac,big.mark =" "),
        "</span></div>"
      ))
  })
  
  # Box definition 3 : total death cases
  output$wm3 <- renderUI({
    df_world_max <- df_world_max()
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_d,big.mark =" "),
        "</span></div>"
      ))
  })
  
  # Box definition 4 : total recovered cases
  output$wm4 <- renderUI({
    df_world_max <- df_world_max()
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(df_world_max$max_r,big.mark =" "),
        "</span></div>"
      ))
  })
  
  ################################################
  # Barplot
  
  # Define the REACTIVE variable to show, thanks to the selected input in the UI
  df3 <- reactive({
    df_inter() %>%
      mutate(show = case_when(input$c1 == 'cc' ~ max_cc,
                              input$c1 == 'ac' ~ max_ac,
                              input$c1 == 'd' ~ max_d,
                              input$c1 == 'r' ~ max_r,
                              input$c1 == 'ncc' ~ max_ncc,
                              input$c1 == 'nac' ~ max_nac,
                              input$c1 == 'nd' ~ max_nd,
                              input$c1 == 'nr' ~ max_nr))
  })
  
  # Define the REACTIVE colors of the bars in the barplot
  col2 <- reactive({
    if(input$c1 %in% c("cc","ncc","ac","nac")){col = '#EF3B2C'
    }else if(input$c1 %in% c("d","nd")){col = "#737373"
    }else if(input$c1 %in% c("r","nr")){col= "#41AB5D"}
    return(col)
  })
  
  # barplot
  output$other_barplot <- renderPlotly({
    df3 <- df3()
    title <- paste0("Daily ",title(), " in ",input$choose_country)
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
} # End of server definition