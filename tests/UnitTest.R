# Unit test of functions

# Packages needed
library("shinyjs")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("shinymaterial")

# Test for function data
source("../Covid_19_R/codes/functions/data.R",encoding = "UTF-8")
df <- data()
# test for the global dataframe
ncol(df$df_grp_all_country) == 11
nrow(df$df_grp_all_country) == 7400
df$df_grp_all_country[df$df_grp_all_country$Country=="France"&df$df_grp_all_country$date=="2020-03-01","max_cc"] == 130
# test for the world datafrale
ncol(df$df_grp_world) == 9
nrow(df$df_grp_world) == 40
df$df_grp_world[df$df_grp_world$date=="2020-03-01","max_cc"] == 87664
# test for the France data
ncol(df$df_grp_FRA) == 11
nrow(df$df_grp_FRA) == 40
df$df_grp_FRA[df$df_grp_FRA$date=="2020-03-01","max_cc"] == 130
# Test the country name list
length(df$Country) == 185
duplicated(df$Country)

# test for the date
df$max_date > "2020-01-21"

# Test for function df_max_date
source("../Covid_19_R/codes/functions/df_max_date.R",encoding = "UTF-8")
df2 <- df_max_date(df = df$df_grp_all_country, max_date = df$max_date)
max(df2$date) == df$max_date
max(df2$date) == "2020-03-01"

# Test for function reactive_color
source("../Covid_19_R/codes/functions/reactive_color.R",encoding = "UTF-8")
reactive_color(input = "cc") == "#EF3B2C"
reactive_color(input = "nd") == "#737373"
reactive_color(input = "r") == "#41AB5D"

# Test for the function reactive_var
source("../Covid_19_R/codes/functions/reactive_var.R",encoding = "UTF-8")
df2 <- reactive_var(data_frame = df$df_grp_FRA, input= "ac")
df2[df2$date=="2020-03-01","show"] == 116

# Test for the function reactive_title
source("../Covid_19_R/codes/functions/reactive_title.R",encoding = "UTF-8")
reactive_title(input = "cc") == "total cases"
reactive_title(input = "nd") == "new death"

