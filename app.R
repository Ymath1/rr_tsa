
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinydashboard")
#library("tidyverse")
library("quantmod")
library('dygraphs')
library('dplyr')
library('lubridate')
library('seasonal')
library('tseries')
library('forecast')
library('assertthat')

## defining class
check_input <- function(object) {
  errors <- character()
  if (nrow(object@ts) == 0) {
    msg <- "No valid data provided, 0 complete cases"
    errors <- c(errors, msg)
  }
  if (!(object@freq %in% c('daily', 'weekly', 'monthly', 'quarterly', 'yearly'))) {
    msg <- "Wrong data frequency, choose from: 'daily', 'weekly', 'monthly', 'quarterly', 'yearly' "
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}
tsExt <- setClass('tsExt',
                  slots = c(ts = "xts",
                            name = "character",
                            freq = "character",
                            base_ts = "xts"),
                  prototype = c(name = "", freq="daily", base_ts=xts()),
                  validity = check_input
)

#init
setMethod(f = "initialize", signature = "tsExt",
          function( .Object, ts, name, freq) 
          { 
            .Object@ts <- ts
            .Object@name <- name
            .Object@freq <- freq
            if(length(names(.Object@ts)) != 1)
              warning('Wrong input format, selected only first column')
            .Object@ts <- .Object@ts[,1]
            .Object@ts[,1] <- as.numeric(.Object@ts[,1])
            .Object@ts <- .Object@ts[complete.cases(.Object@ts ),]
            #create copy of base ts for further analysis
            .Object@base_ts <- .Object@ts
            
            .Object@ts <- get(paste0('apply.',.Object@freq))(.Object@base_ts, FUN=mean)
            return(.Object)
          } 
)

#set values
setGeneric(name = "set_values",function(object, dates, freq, name) standardGeneric("set_values"))
setMethod(f = "set_values", signature = c(object="tsExt", dates="ANY", freq="ANY", name="ANY"), 
          definition = function(object, dates, freq, name){
            if(!missing(freq)) object@freq <- freq
            if(!missing(name)) object@name <- name
            if(!missing(dates)) object@ts <- get(paste0('apply.',object@freq))(object@base_ts[dates], FUN=mean)
            else object@ts <- get(paste0('apply.',object@freq))(object@base_ts, FUN=mean)
            return(object)
          })
#new summary
setMethod(f = "summary", signature = "tsExt",
          function(object) 
          { 
            data.frame(
              name = object@name,
              min = min(object@ts),
              max = max(object@ts),
              mean = mean(object@ts),
              freq = object@freq,
              days = as.numeric(max(index(object@ts)) - min(index(object@ts)))
              
            )
          } 
)

#plot
# plot time series
setGeneric(name = "plot_ts",function(object) standardGeneric("plot_ts"))
setMethod(f = "plot_ts", signature = c("tsExt"),
          function(object) 
          { 
            dygraph(object@ts, main=object@name, width = (1000), height = (500)) %>%
              dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                     ticker="function(a, b, pixels, opts, dygraph, vals) {
                     return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
              dyAxis("y", label = "Values")
            } 
)

# plot stationarity/seasonality
setGeneric(name = "plot_test",function(object, type, n_dif, freq, seas_type) standardGeneric("plot_test"))

setMethod(f = "plot_test", signature = c(object="tsExt", type="ANY", n_dif="ANY", freq="ANY", seas_type="ANY"),
          definition = function(object, type, n_dif, freq, seas_type){
            if(missing(type)) type <- 'station'
            if(type == 'station'){
              if(missing(n_dif)) n_dif <- 1
              validate_that(n_dif%%1==0,msg="n_dif must be an integer!")
              validate_that(nrow(object@ts)>0,msg="Number of rows must be higher than number of diff")
              data <- object@ts
              print(data[1:5])
              if(n_dif>=1){
                for( i in 1:n_dif)
                  data <- diff(data)
              }
              dygraph(data, main=paste(object@name, 'diff:',n_dif), width = (1000), height = (500)) %>%
                dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                       ticker="function(a, b, pixels, opts, dygraph, vals) {
                       return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
                dyAxis("y", label = "Values")
              
          } else if(type == 'season'){
            if(missing(seas_type)) seas_type <- 'additive'
            if(missing(freq)) freq <- object@freq
            freq_tab <-  c(365,52,12,4,2)
            freq_names <- c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
            validate_that(freq %in% freq_names,
                          msg="Wrong data frequency, choose from: 'daily', 'weekly', 'monthly', 'quarterly', 'yearly'")
            validate_that(seas_type %in% c('multiplicative', 'additive'), msg="Wrong seasonality type, choose from: 'multiplicative', 'additive'")
            validate_that(match(freq,freq_names)>=match(object@freq,freq_names), msg="Wrong seasonality type, choose from: 'multiplicative', 'additive'")
            plot(decompose(ts(object@ts, frequency = freq_tab[match(object@freq,freq_names)]), type=seas_type))
          } else {
            stop("Wrong type, choose from: 'station', 'season'")
          }
            
          }
)



# provide statistics for stationarity/seasonality

setGeneric(name = "test_station",function(object, test, n_dif) standardGeneric("test_station"))

setMethod(f = "test_station", signature = c("tsExt", "character", "numeric"),
          definition = function(object, test, n_dif){
            data <- object@ts
            if(n_dif>=1){
              for( i in 1:n_dif)
                data <- diff(data)
            }
            data <- data[(n_dif+1):nrow(data)]
            print(data[1:5])
            if(test == 'all'){
              a_test <- adf.test(data)
              k_test <- kpss.test(data)
              data.frame(
                name = c("ADF test","KPSS test"),
                null = c("Non-stationary data", "Stationary data"),
                stat_val = c(as.numeric(a_test$statistic),as.numeric(k_test$statistic)),
                pval = c(as.numeric(a_test$p.value),as.numeric(k_test$p.value))
              )
            } else if(test == 'kpss'){
              k_test <- kpss.test(data)
              data.frame(
                name = c("KPSS test"),
                null = c("Non-stationary data"),
                stat_val = c(as.numeric(k_test$statistic)),
                pval = c(as.numeric(k_test$p.value))
              )
            } else if(test == 'adf'){
              a_test <- adf.test(data)
              data.frame(
                name = c("ADF test"),
                null = c("Non-stationary data"),
                stat_val = c(as.numeric(a_test$statistic)),
                pval = c(as.numeric(a_test$p.value))
              )
            }  else {
              stop("Wrong test, choose from: 'all', 'adf', 'kpss")
            }
          }
)

# provide statistics for stationarity/seasonality

# provide statistics for stationarity/seasonality

setGeneric(name = "test_season",function(object, test) standardGeneric("test_season"))

setMethod(f = "test_season", signature = c("tsExt", "character"),
          definition = function(object, test){
            a <- as.numeric(object@ts)
            freq_tab <-  c(365,52,12,4,2)
            freq_names <- c('daily', 'weekly', 'monthly', 'quarterly', 'yearly')
            f <- freq_tab[match(object@freq, freq_names)]
            b <- rep(1:f,length(a))[1:length(a)]
            
            if(test == 'all'){
              qs <- qs(seas(ts(apply.monthly(object@ts,FUN=mean),start=1950,frequency=12)))
              c <- kruskal.test(a ~ b)
              data.frame(
                name = c("Kruskal-Wallis test","QS test"),
                null = c("Seasonal component don't exists","Seasonal component don't exists"),
                stat_val = c(as.numeric(c$statistic),as.numeric(qs[,1][1])),
                pval = c(as.numeric(c$p.value),as.numeric(qs[,2][1]))
              )
            } else if(test == 'kw'){
              c <- kruskal.test(a ~ b)
              data.frame(
                name = "Kruskal-Wallis test",
                null = "Seasonal component don't exists",
                stat_val = as.numeric(c$statistic),
                pval = as.numeric(c$p.value)
              )
            } else if(test == 'qs'){
              qs <- qs(seas(ts(apply.monthly(object@ts,FUN=mean),start=1950,frequency=12)))
              data.frame(
                name = "QS test",
                null = "Seasonal component don't exists",
                stat_val = as.numeric(qs[,1][1]),
                pval = as.numeric(qs[,2][1])
              )
            } else {
              stop("Wrong type, choose from: 'all', 'kw', 'qs'")
            }
          }
)
# predict values

setGeneric(name = "predict_ts",function(object, steps, method, graph) standardGeneric("predict_ts"))

setMethod(f = "predict_ts", signature = c("tsExt", "numeric", "character", "logical"),
          definition = function(object, steps, method, graph){
            data = object@ts
            split = nrow(object@ts)-steps
            training = as.numeric(object@ts[1:split])
            testing = as.numeric(object@ts[(split+1):nrow(object@ts)])
            if(method == 'all'){
              pred1 = as.numeric(ses(training,steps)$mean)
              data$ema <- c(training,pred1)
              err1 = testing - pred1
              mod = auto.arima(training)
              pred2 = as.numeric(forecast(mod,h=steps)$mean)
              data$arima <- c(training,pred2)
              err2 = testing - pred2 
              pred3 = as.numeric(naive(training,steps)$mean)
              data$naive <- c(training,pred3)
              err3 = testing - pred3
              if(graph){
                return({
                  dygraph(data[(nrow(data)-2*steps):nrow(data)], main=paste("Comparison of three methods for",object@name), width = (1000), height = (500)) %>%
                    dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                           ticker="function(a, b, pixels, opts, dygraph, vals) {
                           return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
                    dyAxis("y", label = "Values")
              })
              }
              statistics <- data.frame()
              for(j in list(err1,err2,err3)){
                i <- j[1]
                MSE <- mean(abs(i)^2)
                RMSE <- sqrt(MSE)
                MAE <- mean(abs(i))
                MAPE <- mean(abs(i)/testing)
                next_row <- data.frame(MSE,RMSE,MAE,MAPE)
                statistics <- rbind(statistics,next_row)
              }
              statistics
              } else if(method == 'ema'){
                pred = as.numeric(ses(training,steps)$mean)
                err = testing - pred
                data$ema <- c(training,pred)
                if(graph){
                  return({
                    dygraph(data[(nrow(data)-2*steps):nrow(data)], main=paste("Exponential smoothing for ",object@name), width = (1000), height = (500)) %>%
                      dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                             ticker="function(a, b, pixels, opts, dygraph, vals) {
                             return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
                      dyAxis("y", label = "Values")
                })
                }
                
                statistics <- data.frame()
                for(j in list(err)){
                  i <- j[1]
                  Name <- 'EMA'
                  MSE <- mean(abs(i)^2)
                  RMSE <- sqrt(MSE)
                  MAE <- mean(abs(i))
                  MAPE <- mean(abs(i)/testing)
                  next_row <- data.frame(MSE,RMSE,MAE,MAPE)
                  statistics <- rbind(statistics,next_row)
                }
                statistics
                } else if(method == 'arima'){
                  mod = auto.arima(training)
                  pred = as.numeric(forecast(mod,h=steps)$mean)
                  err = testing - pred
                  data$arima <- c(training,pred)
                  if(graph){
                    return({
                      dygraph(data[(nrow(data)-2*steps):nrow(data)], main=paste("Arima prediction for ",object@name), width = (1000), height = (500)) %>%
                        dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                               ticker="function(a, b, pixels, opts, dygraph, vals) {
                               return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
                        dyAxis("y", label = "Values")
                  })
                  }
                  statistics <- data.frame()
                  for(j in list(err)){
                    i <- j[1]
                    Name <- 'ARIMA'
                    MSE <- mean(abs(i)^2)
                    RMSE <- sqrt(MSE)
                    MAE <- mean(abs(i))
                    MAPE <- mean(abs(i)/testing)
                    next_row <- data.frame(MSE,RMSE,MAE,MAPE)
                    statistics <- rbind(statistics,next_row)
                  }
                  statistics
                  } else if(method == 'naive'){
                    pred = as.numeric(naive(training,steps)$mean)
                    err = testing - pred
                    data$naive <- c(training,pred)
                    if(graph){
                      return({
                        dygraph(data[(nrow(data)-2*steps):nrow(data)], main=paste("Naive prediction for ",object@name), width = (1000), height = (500)) %>%
                          dyAxis("x", label = "Date", axisLabelFormatter = "function(d) { return d.getFullYear() }", 
                                 ticker="function(a, b, pixels, opts, dygraph, vals) {
                                 return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}") %>%
                          dyAxis("y", label = "Values")
                    })
                    }
                    statistics <- data.frame()
                    for(j in list(err)){
                      i <- j[1]
                      Name <- 'Naive'
                      MSE <- mean(abs(i)^2)
                      RMSE <- sqrt(MSE)
                      MAE <- mean(abs(i))
                      MAPE <- mean(abs(i)/testing)
                      next_row <- data.frame(MSE,RMSE,MAE,MAPE)
                      statistics <- rbind(statistics,next_row)
                    }
                    statistics
                    } else {
                      stop("Wrong type, choose from: 'arima', 'ema', 'naive'")
                    }
            
                  }
)

##

sidebarwidth = 250

data <- reactive({ })

# -----------------------------------------------------------------------
# MY APP
# -----------------------------------------------------------------------

ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(title = "Time series analysis", titleWidth = sidebarwidth),

  dashboardSidebar(
  
    width = sidebarwidth, 
    
    
    sidebarMenu(
      
      id = "tabs",
      
      menuItem("Home", tabName = "Home", icon = icon("home")),
      
      menuItem("Overview", icon = icon("eye"), tabName = "Overview_menu",
               

               textInput("title", "Title of the series", ""),
               
               selectInput("data_freq","Select data frequency:",     
                           c("Daily" = 'daily',"Weekly" = 'weekly',"Monthly" = 'monthly',
                             "Quarterly" = 'quarterly')),
               
               uiOutput("dates_slide"),
              
               menuSubItem('Show results',
                           tabName = 'Overview_menu',
                           icon = icon('line-chart'))
               
               
      ),
      menuItem("Seasonality", icon = icon("chart-line"), tabName = "Seasonality",
               
               
               selectInput("seas_mult","Select seasonal component:",     
                           c("Multiplicative" = 'multiplicative',"Additive" = 'additive')),
               
               selectInput("seas_test","Select test:",     
                           c("All" = 'all',"KruskalWallis" = 'kw',"QS-test" = 'qs')),
               
               
               menuSubItem('Show results',
                           tabName = 'Seasonality',
                           icon = icon('line-chart'))
               
               
      ),
      menuItem("Stationarity", icon = icon("bars"), tabName = "Stationarity",
               
               
               selectInput("station_diff","Number of differentiations:",     
                           c("0" = 0,"1" = 1,"2" = 2, "3"=3)),
               
               selectInput("station_test","Select test:",     
                           c("All" = 'all',"KPSS test" = 'kpss',"ADF test" = 'adf')),
               
               
               menuSubItem('Show results',
                           tabName = 'Stationarity',
                           icon = icon('line-chart'))
               
               
      ),
      menuItem("Forecasting", icon = icon("arrow-right"), tabName = "Forecasting",
               
               numericInput("forecast_steps","Number of observations for the testing sample:",     
                           1,1,30,1),
               
               selectInput("forecast_method","Select method:",     
                           c("All" = 'all',"Naive" = 'naive',"ARIMA" = 'arima',"EMA" = 'ema')),
               
               
               menuSubItem('Show results',
                           tabName = 'Forecasting',
                           icon = icon('line-chart'))
               
               
      )
      
      # badgeLabel, badgeColor = "green" = "new" daje zieloną ikonkę new
      
      
    ) # sidebar menu
    
  ), #dashboard sidebar
  

  



    # -----------------------------------------------------------------------   
    
  
dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "Home",
              h2("Welcome to our application!"),
              h4("Please upload your CSV file below:"),
            
              fileInput(
                inputId = "fileinput",
                label = " ",
                multiple = FALSE,
                accept = c(".csv"),
                width = "300px",
                placeholder = "Search...",
                buttonLabel = icon("folder")
              ),
              
              
              # textOutput('contents')
      ),
      
      tabItem(tabName = "Overview_menu",
              h2("Overview of input data"),
              dygraphOutput('general_plot'),
              h3('Simple statistics of time series'),
              tableOutput('general_summary')

      ),
      
      tabItem(tabName = "Seasonality",
              h3("Testing seasonality of time series"),
              tableOutput('seas_stat'),
              h3('Decomposition into trend, seasonal and random component'),
              plotOutput('seas_decomp')
      ),
      tabItem(tabName = "Stationarity",
              h3("Testing stationarity of time series"),
              tableOutput('station_res'),
              h3("Time series after differantiation"),
              dygraphOutput('station_plot')
      ),
      
      tabItem(tabName = "Forecasting",
              h3("Forecasting graph"),
              dygraphOutput('forecast_graph'),
              h3('Forecasting errors'),
              tableOutput('forecast_err')
              
      )
      
      # tabItem(tabName = "menu1", 
      #         h4("this is the chart1 tab page")
      # ),
      # 
      # tabItem(tabName = "menu2", 
      #         h4("this is the chart2 tab page"))
      
      
      ) # Tabitems


  ) # dashboard body

) # dashboard page



# -----------------------------------------------------------------------
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {


  infile <- reactive({
    infile <- input$fileinput
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    test = read.table(infile$datapath, header=TRUE, sep=',', colClasses=c("Date", rep("numeric", 6)))
    data = as.xts(test[-1], order.by=test[[1]])
    data = tsExt(data, name='Test', 'daily')
    return(data)
  })
  
  myData <- reactive({
    df<-infile()
    if (is.null(df)) return(NULL)
    df <- set_values(df, 
                     dates = paste0(input$dates_slide[1],'/',input$dates_slide[2]),
                     name = input$title,
                     freq = input$data_freq)
    return(df)
  })
  observe({
    inFile <- input$fileinput
    print(inFile)
    
    if(is.null(inFile))
      return(NULL)
    
    test = read.table(inFile$datapath, header=TRUE, sep=',', colClasses=c("Date", rep("numeric", 6)))
    data = as.xts(test[-1], order.by=test[[1]])
    data1 <- isolate(data)
    output$select_print <- renderTable({data})
  })
  
  output$dates_slide <- renderUI({
    if(!is.null(myData()) && is.Date(as.Date(min(index(myData()@base_ts))))){
      sliderInput("dates_slide",
                  "Dates:",
                  min = as.Date(min(index(myData()@base_ts)),"%Y-%m-%d"),
                  max = as.Date(max(index(myData()@base_ts)),"%Y-%m-%d"),
                  value = c(as.Date(min(index(myData()@ts)),"%Y-%m-%d"), as.Date(max(index(myData()@ts)),"%Y-%m-%d")),
                  timeFormat="%Y-%m-%d")
    } else {
      sliderInput("dates_slide",
                  "Dates:",
                  min = as.Date("2016-01-03"),
                  max = as.Date("2016-12-03"),
                  value = c(as.Date("2016-01-03"), as.Date("2016-03-12")),
                  timeFormat="%Y-%m-%d")
    }
    
  })
  
  #rmarkdown::render("analysis.R")
  output$general_plot <- renderDygraph({
    plot_ts(myData())
  })
  
  output$general_summary <- renderTable({
    summary(myData())
  })
  
  output$seas_stat <- renderTable({
    test_season(myData(), input$seas_test)
  })
  output$seas_decomp <- renderPlot({
    plot_test(myData(), type = 'season', seas_type=input$seas_mult)
  })
  
  output$station_plot <- renderDygraph({
    plot_test(myData(), type = 'station', n_dif=as.integer(input$station_diff))
  })
  
  output$station_res <- renderTable({
    test_station(myData(), input$station_test, as.integer(input$station_diff))
  })
  
  output$forecast_graph <- renderDygraph({
    predict_ts(myData(), steps = as.integer(input$forecast_steps), method = input$forecast_method, graph=TRUE)
  })
  
  output$forecast_err <- renderTable({
    predict_ts(myData(), steps = as.integer(input$forecast_steps), method = input$forecast_method, graph=FALSE)
  })
  
  

}



# --------------------------------------------------------------------------------------


# Create Shiny app ----
shinyApp(ui = ui, server = server)

