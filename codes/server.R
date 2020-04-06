source("packages.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output){

    # Import the data for john-hopkins-hospital
    PATH <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    df_confirmed <- read.csv(paste0(PATH,"time_series_covid19_confirmed_global.csv"), stringsAsFactors = FALSE)
    df_death <- read.csv(paste0(PATH,"time_series_covid19_deaths_global.csv"), stringsAsFactors = FALSE)
    df_recovered <- read.csv(paste0(PATH,"time_series_covid19_recovered_global.csv"), stringsAsFactors = FALSE)
    rm(PATH)
    
    # Import mapping table for countries
    df_mapping <- read.csv(
        "https://raw.githubusercontent.com/dataiscoming/Covid-19-with-R/master/%231_input/countries_codes_and_coordinates.csv",
        stringsAsFactors = FALSE)
    
    # Data mnipulation
    df <- df_mapping[,c("Country","Alpha.3.code")] %>%
        mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
        left_join(df_confirmed %>% 
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
                      arrange(Country.Region, date) %>%
                      filter(date == max(date)),
                  by=c("Country"="Country.Region"),
                  keep=FALSE) %>%
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
                      arrange(Country.Region, date) %>%
                      filter(date == max(date)),
                  by=c("Country"="Country.Region","date"="date"),
                  keep=FALSE) %>%
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
                      arrange(Country.Region, date) %>%
                      filter(date == max(date)),
                  by=c("Country"="Country.Region","date"="date"),
                  keep=FALSE) %>%
        group_by(Alpha.3.code) %>%
        mutate(compte = length(Alpha.3.code),
               keep = case_when(compte == 1 ~ "OK",
                                compte > 1 & is.na(value_confirmed)  ~ "KO",
                                compte > 1 & !is.na(value_confirmed) ~ "OK")) %>%
        filter(keep == 'OK') %>%
        select(-compte, -keep) %>% 
        ungroup() %>%
        mutate(value_confirmed = replace_na(value_confirmed,0),
               value_death = replace_na(value_death,0),
               value_recovered = replace_na(value_recovered,0),
               date = replace_na(date,max(na.omit(date),na.rm = TRUE)),
               value_active = value_confirmed-value_death-value_recovered
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
    
    fig_wm_cc <- plot_geo(df) %>% 
        add_trace(
            z = ~value_confirmed, 
            color = ~value_confirmed, 
            colors = 'Reds',
            text = ~Country, 
            locations = ~Alpha.3.code,
            marker = list(line = l)
        ) %>% 
        colorbar(title = 'Number', tickprefix = '') %>%
        layout(
            #title = 'Confirmed cases<br>Source:<a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv">CSSEGISandData</a>',
            geo = g
        )
    
    fig_wm_ac <- plot_geo(df) %>% 
        add_trace(
            z = ~value_active, 
            color = ~value_active, 
            colors = 'Reds',
            text = ~Country, 
            locations = ~Alpha.3.code,
            marker = list(line = l)
        ) %>% 
        colorbar(title = 'Number', tickprefix = '') %>%
        layout(
            #title = 'Confirmed cases<br>Source:<a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv">CSSEGISandData</a>',
            geo = g
        )
    
    fig_wm_d <- plot_geo(df) %>% 
        add_trace(
            z = ~value_death, 
            color = ~value_death, 
            colors = 'Reds',
            text = ~Country, 
            locations = ~Alpha.3.code,
            marker = list(line = l)
        ) %>% 
        colorbar(title = 'Number', tickprefix = '') %>%
        layout(
            #title = 'Confirmed cases<br>Source:<a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv">CSSEGISandData</a>',
            geo = g
        )
    
    fig_wm_r <- plot_geo(df) %>% 
        add_trace(
            z = ~value_recovered, 
            color = ~value_recovered, 
            colors = 'Reds',
            text = ~Country, 
            locations = ~Alpha.3.code,
            marker = list(line = l)
        ) %>% 
        colorbar(title = 'Number', tickprefix = '') %>%
        layout(
            #title = 'Confirmed cases<br>Source:<a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv">CSSEGISandData</a>',
            geo = g
        )
    
    output$WorldMap <- renderPlotly({
        if(input$indicator_radio_button=="cc"){
            fig_wm_cc
        }else if(input$indicator_radio_button=="ac"){
            fig_wm_ac
        }else if(input$indicator_radio_button=="d"){
            fig_wm_d
        }else if(input$indicator_radio_button=="r"){
            fig_wm_r
        }
    })
    #################################################
    # Data mnipulation
    df3 <- df_mapping[,c("Country","Alpha.3.code")] %>%
        mutate(Alpha.3.code =  trimws(Alpha.3.code)) %>%
        left_join(df_confirmed %>% group_by(Province.State, Country.Region) %>%
                      pivot_longer(cols = colnames(df_confirmed)[5:length(colnames(df_confirmed))], names_to = "date") %>%
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
