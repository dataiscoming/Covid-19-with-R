# This function will show ui part for the function reactive_var to render an input to select a variable
# Input :  session ID, name of the input (designed in the server part), width of the UI
# ouput : an UI

select_varINPUTui <- function(id, NAME,WIDTH){
  ns <- NS(id)
  return(
    material_column(
      width = WIDTH,
      material_card(
        depth = 3,
        align = "center",
        title = "Variable", 
        div(
          style = "height:100px",
          selectInput(
            inputId = ns(NAME), 
            selectize = TRUE,
            label="",
            choices=c("Total cases" = "cc",
              "Active cases" = "ac",
              "Death" = "d",
              "Recovered" = "r",
              "New confirmed cases"="ncc",
              "New active cases"="nac",
              "New death"="nd",
              "New recovered"="nr"))))
    )
  )
}