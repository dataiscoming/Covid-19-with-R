# This function will render the UI created with the function renderBOX
# Input : a session id, a title meaning the variable shown, the output name from UI created with the function renderBOX
# ouput : UI 

renderBOXui <- function(id,TITLE, OUTPUT){
  # Namespace ID
  ns <- NS(id)
  
  # UI rendered
  return(material_column(
    width = 3,
    div(style = "text-align:center",
        material_card(
          title = TITLE,
          depth = 3, 
          div(style = "height:75px",uiOutput(ns(OUTPUT)))))
  ))
}
