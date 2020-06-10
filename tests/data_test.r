library(shinytest)
library(testthat)

context("Test Shiny app")

# open Shiny app and PhantomJS
app <- ShinyDriver$new("../")

test_that("data is correct", {
    #set input data
    app$uploadFile(fileinput = '../sample_data/demo.csv')
    a = app$getTitle()
    print(a)
    expect(as.character(a) == "Time series analysis", "Application loaded properly")  
})


test_that("plotting time series", {
    #set input data
    app$uploadFile(fileinput = '../sample_data/demo.csv')
    app$setInputs(sidebarItemExpanded = "Overview")
    app$setInputs(title = "asd")
    app$setInputs(tabs = "Overview_menu")
    expectUpdate(app, output = 'general_plot',timeout=6000)
})
# stop the Shiny app
app$stop()