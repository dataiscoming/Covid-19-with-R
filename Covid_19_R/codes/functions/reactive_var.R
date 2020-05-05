# Reactive variable to show in the Barplot

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