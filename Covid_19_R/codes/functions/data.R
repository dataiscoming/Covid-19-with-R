# Function data :  
# this module get the data from John Hopkins github repository
# And join all dataframe into one data frame 
# It creates sepcific dataframe for every modules and store a list a the countries and the date maximum

data <- function(){
  
  log_file <- "applog.log"
  file_logger <- logger("INFO", appenders = file_appender(log_file))
  info(file_logger, paste("4 - Begin the data function."))
  
  print("data import begin")
  
  #mtry1 <- tryCatch(read.csv(paste0(PATH,"time_series_covid19_confirmed_global.csv")), silent = TRUE)
  #mtry2 <- try(read.csv(paste0(PATH,"time_series_covid19_deaths_global.csv")), silent = TRUE)
  #mtry3 <- try(read.csv(paste0(PATH,"time_series_covid19_recovered_global.csv")), silent = TRUE)
  
  #if(class(mtry) == "try-error"){
  #  read.csv(paste0(PATH,"time_series_covid19_confirmed_global.csv"))
  #}else{
  #  message("File doesn't exist, please check")
  #}
  
  #url.exists(paste0(PATH,"time_series_covid19_confirmed_global.csv"))
  
  df_confirmed <- fread("./input/time_series_covid19_confirmed_global.csv",header=T, sep=',',
                        stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  
  if(class(df_confirmed)[1] == "data.table"){
    print("Import data confirmed case OK.")
  } else {
    print("Import data confirmed case KO !")
  }
  
  #df_confirmed <- fread("./input/time_series_covid19_confirmed_global.csv",header=T, sep=',',
   #                 stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  
  df_death <- fread("./input/time_series_covid19_deaths_global.csv",header=T, sep=',',
                    stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
 
  df_recovered <- fread("./input/time_series_covid19_recovered_global.csv",header=T, sep=',',
                    stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  info(file_logger, paste("5 - inish the importing data from csv."))
  
  setkey(df_confirmed, "Country/Region", "Province/State")
  setkey(df_death, "Country/Region", "Province/State")
  setkey(df_recovered, "Country/Region", "Province/State")
  
  # Import the data for john-hopkins-hospital
  #PATH <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  #df_confirmed <- fread(paste0(PATH,"time_series_covid19_confirmed_global.csv"),header=T, sep=',',
  #                  stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  #setkey(df_confirmed, "Country/Region", "Province/State")
  #df_death <- fread(paste0(PATH,"time_series_covid19_deaths_global.csv"),header=T, sep=',',
  #                  stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  #setkey(df_death, "Country/Region", "Province/State")
  #df_recovered <- fread(paste0(PATH,"time_series_covid19_recovered_global.csv"),header=T, sep=',',
  #                  stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE)
  #setkey(df_recovered, "Country/Region", "Province/State")
  #rm(PATH)
  
  # Import mapping table for countries for the code ISO 3166 ALPHA-3
  df_mapping <- read.csv("./input/countries_codes_and_coordinates.csv")
  
  # Data manipulation
  options(warn = -1)
  df_grp_all_country <- 
    # Select only needed variables in df_mapping
    df_mapping %>% 
    select(Country,Alpha.3.code) %>% 
    mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
    
    # Join the confirmed data 
    left_join(df_confirmed %>% 
                # Create the variables Date because there are one variable for each date
                rename('Country.Region'='Country/Region', 'Province.State'='Province/State') %>%
                group_by(Country.Region, Province.State) %>%
                pivot_longer(cols = colnames(df_confirmed)[5:length(colnames(df_confirmed))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                
                # Sum of the variables (confirmed cases, death and recovered) grouped by the country names and not the province
                group_by(Country.Region, date) %>%
                summarise(max_cc = sum(value)) %>%
                mutate(
                  date = as.Date(
                    paste0(
                      substring(date,1,nchar(date)-2),
                      "20",
                      substring(date,nchar(date)-1,nchar(date)))
                    ,format="%m/%d/%Y")) %>%
                
                # Select and arrange the data
                arrange(Country.Region, date)
              ,
              by=c("Country"="Country.Region"),
              keep=FALSE) %>%
    
    # Join the death data with the same process than above
    left_join(df_death %>% 
                rename('Country.Region'='Country/Region', 'Province.State'='Province/State') %>%
                group_by(Province.State, Country.Region) %>%
                pivot_longer(cols = colnames(df_death)[5:length(colnames(df_death))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                group_by(Country.Region, date) %>%
                summarise(max_d = sum(value)) %>%
                mutate(
                  date = as.Date(
                    paste0(
                      substring(date,1,nchar(date)-2),
                      "20",
                      substring(date,nchar(date)-1,nchar(date)))
                    ,format="%m/%d/%Y")) %>%
                arrange(Country.Region, date),
              by=c("Country"="Country.Region","date"="date"),
              keep=FALSE) %>%
    
    # Join the recovered data  with the same process than above
    left_join(df_recovered %>%
                rename('Country.Region'='Country/Region', 'Province.State'='Province/State') %>%
                group_by(Province.State, Country.Region) %>%
                pivot_longer(cols = colnames(df_recovered)[5:length(colnames(df_recovered))], names_to = "date") %>%
                select(-Lat,-Long) %>%
                group_by(Country.Region, date) %>%
                summarise(max_r = sum(value)) %>%
                mutate(
                  date = as.Date(
                    paste0(
                      substring(date,1,nchar(date)-2),
                      "20",
                      substring(date,nchar(date)-1,nchar(date)))
                    ,format="%m/%d/%Y")) %>%
                arrange(Country.Region, date),
              by=c("Country"="Country.Region","date"="date"),
              keep=FALSE) %>%
    
    # Delete country duplicated
    group_by(Alpha.3.code) %>%
    mutate(compte = length(Alpha.3.code),
           keep = case_when(compte == 1 ~ "OK",
                            compte > 1 & is.na(max_cc)  ~ "KO",
                            compte > 1 & !is.na(max_cc) ~ "OK")) %>%
    ungroup() %>%
    filter(keep == 'OK') %>%
    select(-compte, -keep) %>% 
    
    # Change the NA values to 0 and of the active cases variables
    mutate(max_cc = replace_na(max_cc,0),
           max_d = replace_na(max_d,0),
           max_r = replace_na(max_r,0),
           date = replace_na(date,max(na.omit(date),na.rm = TRUE)),
           max_ac = max_cc-max_d-max_r
    ) %>%
    
    # Create new variables about new cases every day
    group_by(Country) %>%
    mutate(max_ncc = max_cc-lag(max_cc),
           max_nd = max_d-lag(max_d),
           max_nr = max_r-lag(max_r),
           max_nac = max_ac-lag(max_ac),
           max_nac = case_when(max_nac < 0 ~ 0,
                               max_nac >= 0 ~ max_nac),
           max_ncc = case_when(max_ncc < 0 ~ 0,
                               max_ncc >= 0 ~ max_ncc),
           max_nd = case_when(max_nd < 0 ~ 0,
                              max_nd >= 0 ~ max_nd),
           max_nr = case_when(max_nr < 0 ~ 0,
                              max_nr >= 0 ~ max_nr)) %>%
    ungroup() %>%
    
    # Change the format of variables
    mutate(Alpha.3.code = as.factor(Alpha.3.code)) #%>% # for the test
    #filter(date <= "2020-03-01") # for the test
  options(warn = 0)

  # Define the data used in the world (not reactive) for the map 
  df_grp_world <- df_grp_all_country %>%
    group_by(date) %>%
    summarise(max_cc = sum(max_cc),
              max_ac = sum(max_ac),
              max_d = sum(max_d),
              max_r = sum(max_r),
              max_ncc = sum(max_ncc),
              max_nac = sum(max_nac),
              max_nd = sum(max_nd),
              max_nr = sum(max_nr))
  
  # Define the data used in France (not reactive) for the infobox and barplot
  df_grp_FRA <- df_grp_all_country %>%
    filter(Alpha.3.code == "FRA")
  
  # List of the contry names
  Country <- unique(df_grp_all_country$Country)
  
  # Date maximun
  max_date <- max(df_grp_all_country$date)
  
  # Remove files
  rm(df_confirmed)
  rm(df_death)
  rm(df_recovered)
  rm(df_mapping)
  
  # Results
  res = NULL
  res$df_grp_world = df_grp_world
  res$df_grp_all_country = df_grp_all_country
  res$df_grp_FRA = df_grp_FRA 
  res$Country = Country
  res$max_date = max_date
  info(file_logger, paste("6 - Finish the data function."))
  return(res)
}
