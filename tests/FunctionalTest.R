
# Packages
library(shinytest)

# Recod the different tests
recordTest("../Covid_19_R/")

# Test modules 
testApp(appDir = "../Covid_19_R/", testnames = "test_21052020_world")

testApp(appDir = "../Covid_19_R/", testnames = "test_21052020_france")

#testApp(appDir = "../Covid_19_R/", testnames = "test_21052020_other") did not work

testApp(appDir = "../Covid_19_R/", testnames = "test_21052020_about")
