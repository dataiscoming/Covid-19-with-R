# This function will create an UI with a box with a figure inside 
# Input :  a figure
# ouput : box with a figure

renderBOX <- function(number){
  return(
    HTML(
      paste0(
        "<div class='text-right'><span style='font-size:28px'>",prettyNum(number,big.mark =" "),"</span></div>"
      ))
  )
}