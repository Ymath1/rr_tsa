---
title: "rr_tsa_shinyapp"
author: "Mateusz Kijewski"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{rr_tsa_shinyapp}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```


## General description

The project enables user to run Shiny app for Time series analysis. Also it implements
S4 class tsExt which extends the xts class with more methods for plotting or testing the time series.


## User step by step guide

The first page of the application is the Home page – here a user can upload a .CSV file which
will be used in further steps. The file needs to be the time series format (first column is data index
and second one is time series) – we provide a sample dataset to test the application (demo.csv).
There is also a “Create RMarkdown report” button on this page, which can be used to generate a
file with the analysis with default parameters.

In the second tab (Overview) there are presented some information about the time series –
at the top of the page there is a plot which presents the time series, and at the bottom – basic
statistics like minimum and maximum value, mean, frequency of the data and range of selected days.
The title of the plot, as well as frequency and range of the data, can be set at the sidebar on the left.

The next two pages – Seasonality and Stationarity – are the tabs that enable to decompose
the time series. A user can choose a seasonal component, select test to perform (Kruskal-Wallis, QS,
KPSS test, Augmented Dickey-Fuller) or differentiate the time series the desired number of times.
The last tab is a tool for forecasting – here a user can choose a number of observations for
the testing sample used in prediction, and the method of estimation: Naïve, ARIMA, or EMA. 

## S4 Class description

The tsExt class has following methods:

``` {r}
# Setting values for class attributes
f = "set_values", signature = c(object="tsExt", dates="ANY", freq="ANY", name="ANY")

# Summarize the time series
f = "summary", signature = "tsExt"

# Plot the time series using dygraphs
f = "plot_ts", signature = c("tsExt")

# Plot decomposition of time series or differenced time series depending on type argument
f = "plot_test", signature = c(object="tsExt", type="ANY", n_dif="ANY", freq="ANY", seas_type="ANY")

# Plot decomposition of time series or differenced time series depending on type argument
f = "plot_test", signature = c(object="tsExt", type="ANY", n_dif="ANY", freq="ANY", seas_type="ANY")

# Plot decomposition of time series or differenced time series depending on type argument
f = "plot_test", signature = c(object="tsExt", type="ANY", n_dif="ANY", freq="ANY", seas_type="ANY")

# Tests the stationarity of time series
f = "test_station", signature = c("tsExt", "character", "numeric")

# Tests the seasonality of time series
f = "test_season", signature = c("tsExt", "character")
```

tsExt instance example:

``` {r}
test <- read.table('data.csv',header=TRUE,sep=',',colClasses=c("Date", rep("numeric", 1)))
test <- as.xts(test[-1], order.by=test[[1]])
data <- tsExt(test, 'Time series', 'daily')

plot_ts(data)

summary(data)

test_season(data, 'all)

plot_test(data, type = 'season', seas_type='multiplicative')

test_station(data, input$station_test, 1)

plot_test(data, type = 'station', n_dif=1)

```

