# Optimization

##############################################################
# Import CSV mapping data
micro <- microbenchmark::microbenchmark(
  df_mapping1 <- read.csv(
    "https://raw.githubusercontent.com/dataiscoming/Covid-19-with-R/master/input/countries_codes_and_coordinates.csv",
    stringsAsFactors = FALSE),
  df_mapping2 <- read.csv(
    "input/countries_codes_and_coordinates.csv",
    stringsAsFactors = FALSE),
  df_mapping3 <- readRDS(
    "input/countries_codes_and_coordinates.rds"),
  df_mapping4 <- readr::read_csv(
    "input/countries_codes_and_coordinates.csv"),
  #arrow
  #feather
  #fst
  times = 100
)
micro # choose second solution
rm(list=ls())

# solution 3: RDS

##############################################################
# Import other CSV

PATH <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

micro2 <- microbenchmark::microbenchmark(
  df_confirmed <- read.csv(paste0(PATH,"time_series_covid19_confirmed_global.csv"), stringsAsFactors = FALSE),
  df_confirmed2 <- data.table::fread(paste0(PATH,"time_series_covid19_confirmed_global.csv"),header=T, sep=',',
                                     stringsAsFactors = FALSE,data.table = TRUE,showProgress = FALSE),
  #df_confirmed3 <- bigmemory::read.big.matrix(paste0(PATH,"time_series_covid19_confirmed_global.csv"), header = T),
  df_confirmed4 <- ff::read.csv.ffdf(file = paste0(PATH,"time_series_covid19_confirmed_global.csv"), header = T),
  df_confirmed4 <- sqldf::read.csv.sql(paste0(PATH,"time_series_covid19_confirmed_global.csv")),
  #df_confirmed5 <- iotools::read.csv.raw(paste0(PATH,"time_series_covid19_confirmed_global.csv"),header=T,nrows=100000),
  df_confirmed6 <- readr::read_table(paste0(PATH,"time_series_covid19_confirmed_global.csv")),
  df_confirmed7 <- vroom::vroom(paste0(PATH,"time_series_covid19_confirmed_global.csv")),
  #df_confirmed8 <- arrow::read_csv_arrow(paste0(PATH,"time_series_covid19_confirmed_global.csv")),
  #df_confirmed9 <- arrow::read_parquet(paste0(PATH,"time_series_covid19_confirmed_global.csv"),as_data_frame = FALSE),
  times = 100
)
micro2

# solution 2 : fread from data table

##############################################################
# data manipulation

df_confirmed2b <- df_confirmed2
data.table::setkey(df_confirmed2b, "Country/Region", "Province/State")

micro3 <- microbenchmark::microbenchmark(
  df <- df_confirmed %>% 
    group_by(Country.Region, Province.State) %>%
    pivot_longer(cols = colnames(df_confirmed)[5:length(colnames(df_confirmed))], names_to = "date") %>%
    select(-Lat,-Long) %>%
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
    select(-date2,-date3,-date4) %>%
    arrange(Country.Region, date),
  
  df2 <- df_confirmed2 %>% 
    rename('Country_Region'='Country/Region', 'Province_State'='Province/State') %>%
    group_by(Country_Region, Province_State) %>%
    pivot_longer(cols = colnames(df_confirmed2)[5:length(colnames(df_confirmed2))], names_to = "date") %>%
    select(-Lat,-Long) %>%
    ungroup() %>%
    group_by(Country_Region, date) %>%
    summarise(value_confirmed = sum(value)) %>%
    mutate(
      date = as.Date(
        paste0(
          substring(date,1,nchar(date)-2),
          "20",
          substring(date,nchar(date)-1,nchar(date)))
        ,format="%m/%d/%Y")) %>%
    arrange(Country_Region, date),
  
  df2 <- df_confirmed2b %>% 
    rename('Country_Region'='Country/Region', 'Province_State'='Province/State') %>%
    group_by(Country_Region, Province_State) %>%
    pivot_longer(cols = colnames(df_confirmed2b)[5:length(colnames(df_confirmed2b))], names_to = "date") %>%
    select(-Lat,-Long) %>%
    ungroup() %>%
    group_by(Country_Region, date) %>%
    summarise(value_confirmed = sum(value)) %>%
    mutate(
      date = as.Date(
        paste0(
          substring(date,1,nchar(date)-2),
          "20",
          substring(date,nchar(date)-1,nchar(date)))
        ,format="%m/%d/%Y")) %>%
    arrange(Country_Region, date),
  
  times= 10
)

micro3

# solution 3: data table with key
 
