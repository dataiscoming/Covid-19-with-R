# Other module :
# UI --> 1 input to select a country
#    --> 4 outputs to show figures
#    --> 1 inputs to select the variable 
#    --> 1 output that is the barplot
# SERVER --> Define the reactive UI to choose a country
#        --> use data from the module data
#        --> define reactive values : title, color, country data
#        --> 5 outputs : 4 figures and 1 barplot

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
          renderBOXui(id,"Total cases","wm1"),
          
          # Show the number of total actives cases in a box
          renderBOXui(id,"Active cases","wm2"),
          
          # Show the number of Death actives cases in a box
          renderBOXui(id,"Deaths","wm3"),
          
          # Show the number of total recovered cases in a box
          renderBOXui(id,"Recovered","wm4")
    ))),
    
    tags$br(),
    
    # Two boxes in a row select the input
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
        
          # Input : Select one input that is the variable used for the map and plot 
          select_varINPUTui(id,"c1",12)
    ))),
    
    # Output : Show the barplot according to the input selected above
    barPLOTui(id,"other_barplot")
  )
} # End of UI definition

###########################################################

# Server  definition
other_country <- function(input, output, session, data = df){
  ns <- session$ns
    
  ################################################
  # Common steps
  
  # The select input for the country at the top of the UI
  output$choose_country_input = renderUI({
    cc <- data$Country
    selectInput(
      inputId = ns("choose_country"),
      label = "",
      choices = cc,
      selected = "US") 
  })
  
  # Define the data used in the country selected (not reactive) for the infobox and barplot
  df_grp_date_COUNTRY <- reactive({
    validate(
      need(input$choose_country %in% data$Country, "Data is loading")
    )
    data$df_grp_all_country %>%
      filter(Country == input$choose_country)
  })
  
  # Define the reactive title
  title <- reactive({reactive_title(input=input$c1)})
  
  ################################################
  # Number information
  
  # Define the max value orther different variables from df_inter
  df_world_max <- reactive({
    df_max_date(df=df_grp_date_COUNTRY(), max_date=data$max_date) 
  })
  
  # Box defintion 1 : total confirmed cases
  output$wm1 <- renderUI(renderBOX(number=df_world_max()$max_cc))
  
  # Box definition 2 : total active cases
  output$wm2 <- renderUI(renderBOX(number=df_world_max()$max_ac))
  
  # Box definition 3 : total death cases
  output$wm3 <- renderUI(renderBOX(number=df_world_max()$max_d))
  
  # Box definition 4 : total recovered cases
  output$wm4 <- renderUI(renderBOX(number=df_world_max()$max_r))
  
  ################################################
  # Barplot
  
  # Define the REACTIVE variable to show, thanks to the selected input in the UI
  df_BP <- reactive({reactive_var(data_frame = df_grp_date_COUNTRY(), input = input$c1)})
  
  # Define the REACTIVE colors of the bars in the barplot
  color <- reactive({reactive_color(input = input$c1)})
  
  # barplot
  output$other_barplot <- renderPlotly({reactive_barplot(df = df_BP(), col =color(), title=title())})
  
} # End of server definition