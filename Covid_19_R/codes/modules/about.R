# About module :
# UI --> 1 output that is the date of he update of the data
# SERVER --> get the maximum date of update of the data

# Define UI
aboutUI <- function(id) {
  ns <- NS(id)
  
  material_side_nav_tab_content(
    side_nav_tab_id = "about",
    
    tags$br(),
    
    # First row : Data and date of update
    material_row(
      material_column(
        width = 10,
        offset = 1,
        h2("Data"),
        tags$div(
          HTML("<p>The data comes from the John Hopkins dataset stored in the <a href='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'>github repository</a></p>")
        ),
        textOutput(outputId = ns("update"))
        )),
    
    # Second row : data is coming
    material_row(
      material_column(
        width = 10,
        offset = 1,
        h2("Data is coming"),
        p("Data is coming is a blog about data Science, Game Of Thrones and other hot topics."),
        a(href="http://www.dataiscoming.fr","Data is coming blog"),
        tags$div(
          HTML("<p>All the codes are available at this <a href='https://github.com/dataiscoming/Covid-19-with-R'>github repository</a>.</p>")
        )
      ))
  )
} # End of UI definition

####################################

# Server definition
about <- function(input,output,session,data=df){
  
  # maximum date of the update
  output$update <- renderText(
    paste0("The data are updated to the ",as.character(max(data$date)),".")
  )
  
} # End of server definition
