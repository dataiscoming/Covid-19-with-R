# The ui part for the function barplot to render a reactive barplot
# input : Id of the session to get the name of the plot from the server side / Output : the name of plot created
# output : the ui that render the baplot

barPLOTui <- function(id, OUTPUT){
  # Namespaced IDs
  ns <- NS(id)
  
  # UI rendered : 1 column with one plot inside 
  return(material_row(
    material_column(
      width=10,
      offset = 1,
      material_card(
        div(
          align = "center",
          plotlyOutput(
            outputId = ns(OUTPUT),
            width = "100%", 
            height = "100%")
        )))
  ))
}