# This function will create a reactive variable shown in the Barplot
# Input : a dataframe, a variable selected with the function select_varINPUTui 
# ouput : a dataframe with a new column or a modified column

reactive_var <- function(data_frame,input){
  return(
    data_frame %>%
      mutate(show = case_when(input == 'cc' ~ max_cc,
                              input == 'ac' ~ max_ac,
                              input == 'd' ~ max_d,
                              input == 'r' ~ max_r,
                              input == 'ncc' ~ max_ncc,
                              input == 'nac' ~ max_nac,
                              input == 'nd' ~ max_nd,
                              input == 'nr' ~ max_nr))
  )
}