# The ui part for the function renderBOX to render some figures

renderBOXui <- function(id,TITLE, OUTPUT){
  ns <- NS(id)
  return(material_column(
    width = 3,
    div(style = "text-align:center",
        material_card(
          title = TITLE,
          depth = 3, 
          div(style = "height:75px",uiOutput(ns(OUTPUT)))))
  ))
}
