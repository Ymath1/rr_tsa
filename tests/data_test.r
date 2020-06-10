library(shinytest)
library(testthat)

context("Test Shiny app")

# open Shiny app and PhantomJS
getwd()
app <- ShinyDriver$new("D:/OneDrive/current_projects/rr_tsa/app.R")

test_that("output is correct", {
    #set input data
    app$setInputs(fileinput = 'sample_data/demo.csv')
    expect_s4_class(myData(), "tsExt")  
})

# stop the Shiny app
app$stop()