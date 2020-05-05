# reactive bar pot definition

reactive_barplot <- function(df, col, title){

  return(
    plot_ly(
      data = df,
      x = ~date,
      y = ~show,
      name = "",
      type = "bar",
      marker = list(color = col)
  ) %>% 
    layout(title = paste0("Daily ",title),
           yaxis = list(
             title = "Number of cases",
             zeroline=F
           )))
    
}