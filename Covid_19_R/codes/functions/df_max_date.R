# This function compute filter a dataframe with a date, which is the maximum date in the dataframe
# Input :  a dataframe, a date  
# ouput : a filtered dataframe by the date 

df_max_date <- function(df,max_date){
  return(
    df %>%
      filter(date == max_date)
  )
}