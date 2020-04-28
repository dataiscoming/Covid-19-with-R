# Module data :  
# this module get the data from John Hopkins github repository
# And join all dataframe into one data frame 

data <- function(input, output, session){
  
  # Import the data for john-hopkins-hospital
  PATH <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  df_confirmed <- read.csv(paste0(PATH,"time_series_covid19_confirmed_global.csv"), stringsAsFactors = FALSE)
  df_death <- read.csv(paste0(PATH,"time_series_covid19_deaths_global.csv"), stringsAsFactors = FALSE)
  df_recovered <- read.csv(paste0(PATH,"time_series_covid19_recovered_global.csv"), stringsAsFactors = FALSE)
  rm(PATH)
  
  # Import mapping table for countries for the code ISO 3166 ALPHA-3
  df_mapping <- read.csv(
    "https://raw.githubusercontent.com/dataiscoming/Covid-19-with-R/master/input/countries_codes_and_coordinates.csv",
    stringsAsFactors = FALSE)
  
  # Data manipulation
  df <- 
    # Select only needed variables in df_mapping
    df_mapping %>% 
    select(Country,Alpha.3.code) %>% 
    mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
    
    # Join the confirmed data 
    left_join(df_confirmed %>% 
                # Create the variables Date because there are one variable for each date
                group_by(Country.Region, Province.State) %>%
                pivot_longer(cols = colnames(df_confirmed)[5:length(colnames(df_confirmed))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                
                # Sum of the variables (confirmed cases, death and recovered) grouped by the country names and not the province
                group_by(Country.Region, date) %>%
                summarise(value_confirmed = sum(value)) %>%
                mutate(date2 = gsub("X","0",date),
                       date3 = sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                       date4 = paste0(sapply(strsplit(date2,"[.]"), "[[" , 1),"-",
                                      sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                                      "-",
                                      sapply(strsplit(date2,"[.]"), "[[" , 3)),
                       date = as.Date(date4,format="%m-%d-%y")
                ) %>%
                
                # Select and arrange the data
                select(-date2,-date3,-date4) %>%
                arrange(Country.Region, date)
              ,
              by=c("Country"="Country.Region"),
              keep=FALSE) %>%
    
    # Join the death data with the same process than above
    left_join(df_death %>% 
                group_by(Province.State, Country.Region) %>%
                pivot_longer(cols = colnames(df_death)[5:length(colnames(df_death))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                group_by(Country.Region, date) %>%
                summarise(value_death = sum(value)) %>%
                mutate(date2 = gsub("X","0",date),
                       date3 = sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                       date4 = paste0(sapply(strsplit(date2,"[.]"), "[[" , 1),"-",
                                      sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                                      "-",
                                      sapply(strsplit(date2,"[.]"), "[[" , 3)),
                       date = as.Date(date4,format="%m-%d-%y")
                ) %>%
                select(-date2,-date3,-date4) %>%
                arrange(Country.Region, date),
              by=c("Country"="Country.Region","date"="date"),
              keep=FALSE) %>%
    
    # Join the recovered data  with the same process than above
    left_join(df_recovered %>%
                group_by(Province.State, Country.Region) %>%
                pivot_longer(cols = colnames(df_recovered)[5:length(colnames(df_recovered))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                group_by(Country.Region, date) %>%
                summarise(value_recovered = sum(value)) %>%
                mutate(date2 = gsub("X","0",date),
                       date3 = sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                       date4 = paste0(sapply(strsplit(date2,"[.]"), "[[" , 1),"-",
                                      sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                                      "-",
                                      sapply(strsplit(date2,"[.]"), "[[" , 3)),
                       date = as.Date(date4,format="%m-%d-%y")
                ) %>%
                select(-date2,-date3,-date4) %>%
                arrange(Country.Region, date),
              by=c("Country"="Country.Region","date"="date"),
              keep=FALSE) %>%
    
    # Delete country duplicated
    group_by(Alpha.3.code) %>%
    mutate(compte = length(Alpha.3.code),
           keep = case_when(compte == 1 ~ "OK",
                            compte > 1 & is.na(value_confirmed)  ~ "KO",
                            compte > 1 & !is.na(value_confirmed) ~ "OK")) %>%
    filter(keep == 'OK') %>%
    select(-compte, -keep) %>% 
    ungroup() %>%
    
    # Change the NA values to 0 and of the active cases variables
    mutate(value_confirmed = replace_na(value_confirmed,0),
           value_death = replace_na(value_death,0),
           value_recovered = replace_na(value_recovered,0),
           date = replace_na(date,max(na.omit(date),na.rm = TRUE)),
           value_active = value_confirmed-value_death-value_recovered
    ) %>%
    
    # Create new variables about new cases every day
    group_by(Country) %>%
    mutate(new_confirmed_case = value_confirmed-lag(value_confirmed),
           new_death = value_death-lag(value_death),
           new_recovered = value_recovered-lag(value_recovered),
           new_active_case = value_active-lag(value_active),
           new_active_case = case_when(new_active_case < 0 ~ 0,
                                       new_active_case >= 0 ~ new_active_case),
           new_confirmed_case = case_when(new_confirmed_case < 0 ~ 0,
                                          new_confirmed_case >= 0 ~ new_confirmed_case),
           new_death = case_when(new_death < 0 ~ 0,
                                 new_death >= 0 ~ new_death),
           new_recovered = case_when(new_recovered < 0 ~ 0,
                                     new_recovered >= 0 ~ new_recovered)) %>%
    ungroup() %>%
    
    # Change the format of variables
    mutate(Country = as.factor(Country),
           Alpha.3.code = as.factor(Alpha.3.code)
           )

  # Remove files
  rm(df_confirmed)
  rm(df_death)
  rm(df_recovered)
  rm(df_mapping)
  
  # Results
  return(df)
}
