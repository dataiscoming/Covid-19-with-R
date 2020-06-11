![](img/img_app.png)

# Covid-19 Dashboard with R Shiny

This project is to built a dashboard to show figures of the covid-19 crisis, and render it on a map.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

You will need to install R and an IDE (RStudio for example) by clicking to the links below. 

### Installing and launching the app

After installing the softwares, you can clone the repository. Then, you may click on the project to open it (week_X_Covid_19_with_R - Shortcut.Rproj). 

Finally, you may try the app: 

```
shiny::run_app()
```

The data used by the app are in the input folder, but it is used from the repository of [John Hopkins university](https://github.com/CSSEGISandData/COVID-19). There are no special need to add data for the first try of the app. 

## Running the tests

Run the files in the folder tests.
Have to be finalised.

### Break down into end to end tests

There are two type of test :

* Unit test for functions
* Functional test for the whole app

### And coding style tests

For both type of test, I fixed the maximum date of the data to the value '2020-03-01' in order to have always same figures. 
For the unit test, some test are written and render a value True or False. 

```
# Test for function df_max_date
source("../Covid_19_R/codes/functions/df_max_date.R",encoding = "UTF-8")
df2 <- df_max_date(df = df$df_grp_all_country, max_date = df$max_date)
max(df2$date) == df$max_date
max(df2$date) == "2020-03-01"
```

For the functional test, I proceeded twice. The first one was a code to save a picture of the app to compare later with other pictures when the app have been changed.
The second way is to write a test text file with several cases.Then, I just try the app ad check the results. Theses tests are in the specification file. 

## Deployment

There are several ways to deploy this dashboard :

* use [shinyapps.io](https://www.shinyapps.io/)
* use [shiny server](https://rstudio.com/products/shiny/shiny-server/)
* use [shiny shortcut](https://rdrr.io/cran/shinyShortcut/man/shinyShortcut.html)

You may find the production version at  :

* on [Shinyapps.io]()
* on [my blog](http://shiny.dataiscoming.fr/Covid-19-with-R/)
* The shortcut in the production folder.

## Built With

* [R](https://www.r-project.org/) - Language used 
* [RStudio](https://rstudio.com/) - IDE used

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/dataiscoming/Covid-19-with-R/tags).

## Lifecycle

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

## Authors

* **Quentin Mouton** - *Initial work* - [Dataiscoming](https://github.com/dataiscoming/)

See also the list of [contributors](https://github.com/dataiscoming/Covid-19-with-R/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

This app is inspired from two dashboards :

* [the John Hopkins university dashboard](https://coronavirus.jhu.edu/map.html)
* [the Sebastian Wolf dashboard](https://sebastianwolf.shinyapps.io/Corona-Shiny/)

