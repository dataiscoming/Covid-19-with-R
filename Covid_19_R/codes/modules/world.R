# World module :
# UI --> 4 outputs to show figures
#    --> 2 inputs to select date and variables 
#    --> 2 output that is the world map and barplot
# SERVER --> use data from the module data
#        --> define reactive values : title, color, france data
#        --> 6 outputs : 4 figures, 1 map and 1 barplot

# Functions
source("./codes/functions/renderBOX.R",encoding = "UTF-8")
source("./codes/functions/renderBOXui.R",encoding = "UTF-8")
source("./codes/functions/reactive_color.R",encoding = "UTF-8")
source("./codes/functions/reactive_var.R",encoding = "UTF-8")
source("./codes/functions/select_varINPUTui.R",encoding = "UTF-8")
source("./codes/functions/reactive_barplot.R",encoding = "UTF-8")
source("./codes/functions/barPLOTui.R",encoding = "UTF-8")
source("./codes/functions/reactive_title.R",encoding = "UTF-8")
source("./codes/functions/df_max_date.R",encoding = "UTF-8")

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
          renderBOXui(id,"Recovered","wm4")
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
              title="Date",
              div(
                style = "height:100px",
                align = "center",
                uiOutput(outputId = ns("date_selector"))
                )
              )
          ),
          
          # Input : Select one input that is the variable used for the map and plot 
          select_varINPUTui(id,"c1",4)
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
    barPLOTui(id,"world_barplot")
  )
} # End of UI definition

###############################################

# Define server
world <- function(input, output, session, data = df){
  ns <- session$ns
  
  ################################################
  #### Common steps
  
  # The select input for the date in the UI
  output$date_selector = renderUI({
    sliderInput(
      inputId = ns("slider_date"),
      label="",
      min = as.Date("2020-02-01"),
      max = as.Date(data$max_date),
      value = as.Date(data$max_date),
      timeFormat="%Y-%m-%d",
      animate = animationOptions(
        interval = 200,
        playButton = "Play",
        pauseButton = "Pause"),
      width = "100%")
  })
  
  # Define the reactive data used
  df2 <- reactive({
    df1 <- data$df_grp_all_country %>%
      filter(date==input$slider_date) %>%
      mutate(show = case_when(input$c1 == 'cc' ~ max_cc,
                              input$c1 == 'ac' ~ max_ac,
                              input$c1 == 'd' ~ max_d,
                              input$c1 == 'r' ~ max_r,
                              input$c1 == 'ncc' ~ max_ncc,
                              input$c1 == 'nac' ~ max_nac,
                              input$c1 == 'nd' ~ max_nd,
                              input$c1 == 'nr' ~ max_nr))
    return(df1)
  })
  
  # Define the reactive title
  title <- reactive({reactive_title(input=input$c1)})
  
  ################################################
  ### Number information in boxes
  
  # Define the max value orther different variables from df_inter
  df_world_max <- df_max_date(data$df_grp_world, data$max_date) 
  
  # Box defintion 1 : total confirmed cases
  output$wm1 <- renderUI(renderBOX(number=df_world_max$max_cc))
  
  # Box definition 2 : total active cases
  output$wm2 <- renderUI(renderBOX(number=df_world_max$max_ac))
  
  # Box definition 3 : total death cases
  output$wm3 <- renderUI(renderBOX(number=df_world_max$max_d))
  
  # Box definition 4 : total recovered cases
  output$wm4 <- renderUI(renderBOX(number=df_world_max$max_r))
  
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
    
    # Message while data is loading
    validate(
      need(is.Date(input$slider_date), "Data is loading")
    )

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
  df_BP <- reactive({reactive_var(data_frame = data$df_grp_world, input = input$c1)})

  # Define the REACTIVE colors of the bars in the barplot
  color <- reactive({reactive_color(input = input$c1)})
  
  # barplot definition
  output$world_barplot <- renderPlotly({reactive_barplot(df = df_BP(), col =color(), title=title())})

} # End of server definition