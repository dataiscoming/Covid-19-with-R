# World module :
# UI --> 4 outputs to show figures
#    --> 2 inputs to select date and variables 
#    --> 2 output that is the world map and barplot
# SERVER --> use data from the module data
#        --> define reactive values : title, color, france data
#        --> 6 outputs : 4 figures, 1 map and 1 barplot

source("./codes/functions/renderBOX.R",encoding = "UTF-8")
source("./codes/functions/renderBOXui.R",encoding = "UTF-8")
source("./codes/functions/reactive_color.R",encoding = "UTF-8")
source("./codes/functions/reactive_var.R",encoding = "UTF-8")
source("./codes/functions/reactive_barplot.R",encoding = "UTF-8")

# Define the UI
worldUI <- function(id) {
  ns <- NS(id)
  
  material_side_nav_tab_content(
    side_nav_tab_id = "world_map",
    
    tags$br(),
    
    # Four box in a row to show figures
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          
          # Show the number of total confirmed cases in a box
          renderBOXui(id,"Total cases","wm1"),
          
          # Show the number of total actives cases in a box
          renderBOXui(id,"Active cases","wm2"),
          
          # Show the number of Death actives cases in a box
          renderBOXui(id,"Deaths","wm3"),
          
          # Show the number of total recovered cases in a box
          renderBOXui(id,"Recovered","wm4"),
    ))),
    
    tags$br(),
    
    # Two bow in a row select the input
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
              div(
                style = "height:100px",
                align = "center",
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
                  width = "100%")
                )
              )
          ),
          
          # Input : Select one input that is the variable used for the map and plot 
          material_column(
            width = 4,
            material_card(
              depth = 3,
              div(
                style = "height:100px",
                align = "center",
                selectInput(
                  inputId = ns("c1"), 
                  label = "Variable:",
                  c("Total cases" = "cc",
                    "Active cases" = "ac",
                    "Death" = "d",
                    "Recovered" = "r",
                    "New confirmed cases"="ncc",
                    "New active cases"="nac",
                    "New death"="nd",
                    "New recovered"="nr"
                  )
                )
              )
          )
        )
      ))),
    
      # Output : Show the map according to the input selected above
      material_row(
        material_column(
          width=10,
          offset = 1,
          material_card(
            div(
              align = "center",
              plotlyOutput(
                outputId = ns("WorldMap"),
                width = "100%", 
                height = "100%")
              )
          )
        )
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
                outputId = ns("World_barplot"),
                width = "100%", 
                height = "100%")
            )
          )
        )
      )
  )
} # End of UI definition

###############################################

# Define server
world <- function(input, output, session, data = df){
  
  ################################################
  #### Common steps
  
  # Define the reactive data used
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
  
  # Define the data used in France (not reactive) for the infobox and barplot
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
  
  ################################################
  ### Number information in boxes
  
  # Define the max value orther different variables from df_inter
  df_world_max <- df_inter %>%
    filter(date == max(data$date))
  
  # Box defintion 1 : total confirmed cases
  output$wm1 <- renderBOX(df_world_max$max_cc)
  
  # Box definition 2 : total active cases
  output$wm2 <- renderBOX(df_world_max$max_ac)
  
  # Box definition 3 : total death cases
  output$wm3 <- renderBOX(df_world_max$max_d)
  
  # Box definition 4 : total recovered cases
  output$wm4 <- renderBOX(df_world_max$max_r)
  
  ##############################################
  ### World map
  
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # Define the REACTIVE colors of the countries in the map
  col <- reactive({
  if(input$c1 %in% c('cc','ncc')){col = list(c(0, "#FFF5F0"), c(0.5, "#EF3B2C"), c(1, "#67000D"))
  }else if(input$c1 %in% c('ac','nac')){col = list(c(0, "#FFF5F0"), c(0.5, "#EF3B2C"), c(1, "#67000D"))
  }else if(input$c1 %in% c('d','nd')){col = list(c(0, "#FFFFFF"), c(0.5, "#737373"), c(1, "#000000"))
  }else if(input$c1 %in% c('r','nr')){col = list(c(0, "#F7FCF5"), c(0.5, "#41AB5D"), c(1, "#00441B"))}
  return(col)
  })
  
  # World map definition
  output$WorldMap <- renderPlotly({

    # Reactive variables Needed : data df2, the title of the plot
    df2<-df2()
    title <- paste0("Map of ",title())
    
    # World map
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

    # Result
    return(fig_wm)
  })
  
  ##############################################
  ### Barplot 
  
  # Define the REACTIVE variable to show, thanks to the selected input in the UI
  df_BP <- reactive({reactive_var(data_frame = df_inter, input = input$c1)})
  
  # Define the REACTIVE colors of the bars in the barplot
  color <- reactive({reactive_color(input = input$c1)})
  
  # barplot
  output$World_barplot <- renderPlotly({reactive_barplot(df = df_BP(), col =color(), title=title())})
  
} # End of server definition