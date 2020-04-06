# packages

script <- RCurl::getURL(
  "https://raw.githubusercontent.com/dataiscoming/import_load_packages_function/master/0001%20-%20packages.R",
  ssl.verifypeer = FALSE)

eval(parse(text = script));rm(script)

install_load_packages(list_packages = c("shinyjs","shiny","tidyr","dplyr","plotly","shinymaterial"))

rm(install_load_packages)