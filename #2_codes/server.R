library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output){

    # Import the data for john-hopkins-hospital
    PATH <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    df <- read.csv(PATH, stringsAsFactors = FALSE)
    rm(PATH)
    
    # Import mapping table for countries
    df_mapping <- read.csv(
        "D:/#1_ART/2020/week_X_Covid_19_with_R/#1_input/countries_codes_and_coordinates.csv",
        stringsAsFactors = FALSE)
    
    # Data mnipulation
    df2 <- df_mapping[,c("Country","Alpha.3.code")] %>%
        mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
        left_join(df %>% group_by(Province.State, Country.Region) %>%
                      pivot_longer(cols = colnames(df)[5:length(colnames(df))], names_to = "date") %>%
                      select(-Lat,-Long) %>%
                      group_by(Country.Region, date) %>%
                      summarise(value = sum(value)) %>%
                      mutate(date2 = gsub("X","0",date),
                             date3 = sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                             date4 = paste0(sapply(strsplit(date2,"[.]"), "[[" , 1),"-",
                                            sprintf(fmt="%02d",as.numeric(sapply(strsplit(date2,"[.]"), "[[" , 2))),
                                            "-",
                                            sapply(strsplit(date2,"[.]"), "[[" , 3)),
                             date = as.Date(date4,format="%m-%d-%y")
                      ) %>%
                      select(-date2,-date3,-date4) %>%
                      arrange(Country.Region, date) %>%
                      filter(date == max(date)),
                  by=c("Country"="Country.Region"),
                  keep=FALSE) %>%
        group_by(Alpha.3.code) %>%
        mutate(compte = length(Alpha.3.code),
               keep = case_when(compte == 1 ~ "OK",
                                compte > 1 & is.na(value)  ~ "KO",
                                compte > 1 & !is.na(value) ~ "OK")) %>%
            filter(keep == 'OK') %>%
        select(-compte, -keep) %>% 
        ungroup() %>%
        mutate(value = replace_na(value,0),
               date = replace_na(date,max(na.omit(date),na.rm = TRUE))
               ) 
        
    # World map
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
    )
    
    fig_wm <- plot_geo(df2)
    fig_wm <- fig_wm %>% add_trace(
        z = ~value, 
        color = ~value, 
        colors = 'Reds',
        text = ~Country, 
        locations = ~Alpha.3.code,
        marker = list(line = l)
    )
    fig_wm <- fig_wm %>% colorbar(title = 'Number', tickprefix = '')
    fig_wm <- fig_wm %>% layout(
        #title = 'Confirmed cases<br>Source:<a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv">CSSEGISandData</a>',
        geo = g
    )
    
    output$WorldMap <- renderPlotly({fig_wm})
    
    
    #################################################
    # Data mnipulation
    df3 <- df_mapping[,c("Country","Alpha.3.code")] %>%
        mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
        left_join(df %>% group_by(Province.State, Country.Region) %>%
                      pivot_longer(cols = colnames(df)[5:length(colnames(df))], names_to = "date") %>%
                      select(-Lat,-Long) %>%
                      group_by(Country.Region, date) %>%
                      summarise(value = sum(value)) %>%
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
                  by=c("Country"="Country.Region"),
                  keep=FALSE) %>%
        filter(Alpha.3.code == "FRA")
    
    fig_fbp <- plot_ly(data = df3,
        x = ~date,
        y = ~value,
        name = "SF Zoo",
        type = "bar"
    )
    
    output$France_barplot <- renderPlotly({fig_fbp})

})
