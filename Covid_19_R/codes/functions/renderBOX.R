# Funtion to render a box with a number inside

renderBOX <- function(Number){
  return(renderUI({
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(Number,big.mark =" "),
        "</span></div>"
      ))
  })
  )
}