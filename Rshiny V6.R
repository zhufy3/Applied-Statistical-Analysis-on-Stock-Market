library(shiny)
library(shinydashboard)
library(TTR)
library(plotly)
library(pdfetch)
library(yfinance)
library(quantmod)
library(ggplot2)
library(tseries)
library(dplyr)
library(caTools)
library(forecast)
library(pdfetch)
library(shinythemes)
library(dashboardthemes)
set.seed(1)


###Analysis Section Code
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
###Hypothesis Testing
#get adjclose price growth rate of the stock
get_adjclose <- function(ps,sd,td,inte){
  ps_df = data.frame(pdfetch_YAHOO(c(ps),from = sd, interval = inte))
  adj_ps = ps_df[5]
  names(adj_ps) = c("adjclose")
  i <- 1
  adj_ps$growth_rate[1] <- 0
  while (i < dim(adj_ps)[1]){
    adj_ps$growth_rate[i+1] <- (adj_ps$adjclose[i+1]-adj_ps$adjclose[i])/adj_ps$adjclose[i]
    i <- i+1
  }
  return (adj_ps)
}
#justify if the sample is normal distributed
normal <- function(sample){
  test <- ks.test(sample,'pnorm')
  if (test$p.value>0){
    return (TRUE)
  } else{
    return (FALSE)
  }
}
#justify samples homogeneity
homogeneity <- function(sample){
  homo <- leveneTest(sample$growth_rate~sample$stock_name)
  if (homo$p.value<0.05){
    return (TRUE)
  } else{
    return (FALSE)
  }
}
#one sample statistic test
one_sample_test <- function(adj_ps,ex,ci){
  days <- dim(adj_ps)[1]
  if (!normal(adj_ps$growth_rate)){
    wilcox.test(adj_ps$growth_rate,mu=ex,alternative = "greater",conf.level = ci)
  } else if (days >= 30){
    z.test(adj_ps$growth_rate,alternative = "greater",mu = ex, sigma.x = sqrt(var(adj_ps$growth_rate)), conf.level = ci)
  } else {
    t.test(adj_ps$growth_rate, alternative = "greater", mu = ex, sigma.x = sqrt(var(adj_ps$growth_rate)), conf.level = ci)
  }
}
#two sample statistic test
two_sample_test <- function(market,sd,td,ps,inte,ci){
  adj_market <- get_adjclose(market,sd,td,inte)
  adj_ps <- get_adjclose(ps,sd,td,inte)
  days <- dim(adj_ps)[1]
  if (!normal(adj_ps$growth_rate)){
    wilcox.test(adj_ps$growth_rate,adj_market$growth_rate,alternative = "greater",paired = FALSE, exact = NULL, correct = FALSE,
                conf.int = TRUE, conf.level = ci)
  } else if (days >= 30){
    z.test(adj_ps$growth_rate,adj_market$growth_rate, alternative = "greater", sigma.x = sqrt(var(adj_ps$growth_rate)), conf.level = ci)
  } else {
    t.test(adj_ps$growth_rate,adj_market$growth_rate, alternative = "greater", sigma.x = sqrt(var(adj_ps$growth_rate)), conf.level = ci)
  }
}
#anova test
anova_test <- function(company_list,sd,td,inte){
  #set stock matrix
  stock_matrix <- data.frame(matrix(nrow=0,ncol=2))
  names(stock_matrix) = c('growth_rate','stock_name')
  company_list <- unlist(strsplit(company_list,split = ','))
  for (i in company_list){
    ns = i
    ns_df <- get_adjclose(ns,"2022-05-01","2022-10-01","1wk")
    temp <- data.frame(ns_df$growth_rate,rep(ns))
    names(temp) = c('growth_rate','stock_name')
    stock_matrix = rbind(stock_matrix,temp)
  }

  if ((!normal(stock_matrix$growth_rate))){
    kruskal.test(growth_rate~stock_name, data =stock_matrix)
  }else{
    res.aov <- aov(growth_rate~stock_name , data =stock_matrix)
    summary(res.aov)
    TukeyHSD(res.aov)
  }
}
###Prediction Section Code
#Training - return model
fitted_model <- function(ticker, num_days = 100){
  ticker <- toString(ticker)
  num_days <- as.integer(num_days)
  
  df_ticker <- getSymbols(ticker,src='yahoo', auto.assign=FALSE)
  df_price_all <- df_ticker[,4] #closed price
  df <- tail(df_price_all, num_days)
  
  tr_len = ceiling(0.85*num_days) #85% train data - 15% test
  ts_len = num_days - tr_len
  
  train_data <<- df[1:tr_len]
  test_data <<- df[(tr_len + 1):num_days]
  full_data <<- df[1:num_days]
  model_fit <<- auto.arima(train_data, stationary = FALSE,
                           ic = c("aicc", "aic", "bic"),
                           trace = TRUE, lambda = 'auto', nmodels = 100)
  #return(model_fit)
}

#Summary - return summary of trained model
model_summary <- function(ticker){
  model <- model_fit
  return(summary(model))
}

#Summary - return summary of trained model residual
residual_summary <- function(ticker){
  model <- model_fit
  return(checkresiduals(model))
}

#Forecast - Test set
forecast_output_test <- function(ticker,n_days = length(test_data)){
  model <- model_fit
  forecast_model_test <<- forecast(model, h = n_days, level = c(0.15,0.30,0.80,0.95))
  return(forecast_model_test)
}

#Forecast - return summary of forecast test
forecast_summary_test <- function(ticker){
  return(summary(forecast_model_test))
}

#Forecast - return summary of forecast test residual
forecast_residual_summary_test <- function(ticker){
  return(checkresiduals(forecast_model_test))
}

#Forecast - return summary of forecast test summary
forecast_test_accurracy <- function(ticker){
  return (accuracy(forecast_model_test,test_data))
}

#Forecast - return graph of forecast plot
forecast_graph_test <- function(ticker){
  forecast_values_test <<- forecast_model_test
  # plot(forecast_values, 
  #      xlab = "Day", ylab = "Stock Price ($USD)", main = paste("Stock Price Prediction for", ticker),
  #      fcol = 'red', include = 30  )
  
  Full_Data <- ts(full_data)
  Train_Data <- ts(train_data)
  
  forecast_values_test %>% autoplot(xlab = "Day", ylab = "Stock Price ($USD)", 
                                    main = paste("Stock Price Prediction for", ticker), include = 40) + 
    autolayer(Full_Data) + 
    autolayer(Train_Data)
}

#Prediction
forecast_actual <- function(ticker,n_days = 15){
  pp <<- length(model_fit$model$phi)
  qp <<- length(model_fit$model$theta)
  dp <<- model_fit$model$Delta[1]
  
  if(length(dp)==0){
    dp <<- 0
  }
  
  model_final <<- arima(x = full_data, order = c(pp, dp, qp))
  forecast_values_actual <<- forecast(model_final, h = n_days, level = c(0.15,0.30,0.80,0.95))
  
  Full_Data <- ts(full_data)
  Train_Data <- ts(train_data)
  
  forecast_values_actual %>% autoplot(xlab = "Day", ylab = "Stock Price ($USD)",
                                      main = paste("Stock Price Prediction for", ticker), include = 40, ) +
    autolayer(forecast_values_test) +
    autolayer(Full_Data) +
    autolayer(Train_Data)
}

###################################################################
###################################################################

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Financial Analysis",
    tags$li(class = "dropdown", 
            uiOutput("S")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Historical Data", tabName = "historical", icon = icon("chart-line")),
      menuItem("Comparison", tabName = "comparison", icon = icon("table")),
      menuItem("Hypothesis", tabName = "hypothesis", icon = icon("list")),
      menuItem("Predict", tabName = "predict", icon = icon("flag",lib = "glyphicon"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    tabItems(
      #Tab 1 - Historical
      tabItem(tabName = 'historical',
              fluidRow (
                column(10,h1("Company Historical Analysis")),
                column(10,h5("This page provides you with an overview of the historical price and core financial ratio metrics of the company")),
                column(10,h5(" * Fields are required. Please enter US stocks only.")),
                column(4, 
                       textInput("Company", label="Company Ticker*"),
                ),
                column(5,
                       selectInput('type','Data Type',
                                   c('Open','Close','Low','High','Adjusted Close','Volume')
                       )
                ),
                column(10,
                       dateRangeInput("dateRange", label = "Date Range", start = "2010-01-01", end = Sys.Date(),
                                      min = "2010-01-01", max = Sys.Date(),
                                      separator = " - ", format = "dd/mm/yyyy")
                ),
                column(12, 
                       align = "left",
                       column(3,actionButton("start_historical", label = "Set"))
                ),
                column(10,
                       plotlyOutput("plot1", width = "100%", height = "35%")),
                column(2,
                       align = "left",
                       textOutput("Mean"),
                       textOutput("Median"),
                       textOutput("Mode"),
                       textOutput("Range"),
                       textOutput("IQR"),
                       textOutput("Var"),
                       br(),
                       textOutput("NWC"),
                       textOutput("CR"),
                       textOutput("DB"),
                       textOutput("ROA"),
                       textOutput("ROE"),
                       
                )
              )
              ),
      
      #Tab 2 - Comparison
      tabItem(tabName = 'comparison',
              fluidRow (
                column(10,h1("Charting Comparison")),
                column(10,h5("This page allows your to make a performance comparison between the companies")),
                column(10,h5(" * Fields are required. Please enter US stocks only.")),
                column(4, 
                       textInput("Company1", label="Company 1 Ticker*"),
                       
                ),
                column(1,
                       align = 'left',
                       strong("VS")
                ),
                column(4, 
                       textInput("Company2", label="Company 2 Ticker*"),
                ),
                column(5,
                       selectInput('type2','Data Type',
                                   c('Open','Close','Low','High','Adjusted Close','Volume')
                       )
                ),
                column(10,
                       dateRangeInput("dateRange2", label = "Date Range", start = "2010-01-01", end = Sys.Date(),
                                      min = "2010-01-01", max = Sys.Date(),
                                      separator = " - ", format = "dd/mm/yyyy")
                ),
                column(12, 
                       align = "left",
                       column(3,actionButton("start_comparison", label = "Set"))
                ),
                column(8,
                       plotOutput("plot2", width = "100%", height = "400px")),
                column(2,
                       align = "left",
                       textOutput("CompanyName1"),
                       textOutput("Mean1"),
                       textOutput("Median1"),
                       textOutput("Mode1"),
                       textOutput("Range1"),
                       textOutput("IQR1"),
                       textOutput("Var1"),
                       br(),
                       textOutput("NWC1"),
                       textOutput("CR1"),
                       textOutput("DB1"),
                       textOutput("ROA1"),
                       textOutput("ROE1"),
                ),
                column(2,
                       align = "left",
                       textOutput("CompanyName2"),
                       textOutput("Mean2"),
                       textOutput("Median2"),
                       textOutput("Mode2"),
                       textOutput("Range2"),
                       textOutput("IQR2"),
                       textOutput("Var2"),
                       br(),
                       textOutput("NWC2"),
                       textOutput("CR2"),
                       textOutput("DB2"),
                       textOutput("ROA2"),
                       textOutput("ROE2"),
                       tags$head(tags$style("#CompanyName2{color: red;}",
                                            "#Mean2{color: red;}",
                                            "#Median2{color: red;}",
                                            "#Mode2{color: red;}",
                                            "#Range2{color: red;}",
                                            "#IQR2{color: red;}",
                                            "#Var2{color: red;}",
                                            "#NWC2{color: red;}",
                                            "#CR2{color: red;}",
                                            "#DB2{color: red;}",
                                            "#ROA2{color: red;}",
                                            "#ROE2{color: red;}"
                                            )
                                 )
                )
              )
      ),
      #Tab 3 - Hypothesis Testing
      tabItem(tabName = 'hypothesis',
              fluidRow(
                column(10,h1("Hypothesis Testing")),
                column(10,h5("This page would provide you with statistical insight of your preffered stock, as well as comparison between your preffered stock and the market trends.")),
                column(10,h5(" * Fields are required.")),
                column(7,h4("Enter your preffered stock(US stock)")),
                column(7, textInput("preferred_stock", label="Company Ticker*")),
                column(10,
                       dateRangeInput("dateRange", label = "Date range", start = "2010-01-01", end = Sys.Date(),
                                      min = "2010-01-01", max = Sys.Date(),
                                      separator = " - ", format = "dd/mm/yyyy")
                ),
                column(5,
                       selectInput('interval','Moniter Interval',
                                   c('1d',"1wk",'1mo')
                       )
                ),
                column(10,h3("Historical Growth Rate Performance")),
                column(7, numericInput("expected", label="Expected Growth Rate*", value =0.05, step=1)),
                column(7, numericInput("ci", label="Confidence Level*", value =0.95, step=1)),
                column(10,actionButton("one_sample_test",label="Submit")),
                column(12,verbatimTextOutput("one_sample_test_result")),
                column(10,h3("Market Performance")),
                column(5,
                       selectInput('market_index','Market Index',
                                   c('^ndx',"^ixic","^dji","^oex","^spx","^rut","^dja","^dju","^nya","^xax","^vix","^djt")
                       )
                ),
                column(7, numericInput("ci2", label="Confidence Level*", value =0.95, step=1)),
                column(10,actionButton("two_sample_test",label="Submit")),
                column(12,verbatimTextOutput("two_sample_test_result")),
                column(10,h3("Peer Performance")),
                column(10,h4("Please enter intereted company tickers below, separated by commas")),
                column(7, textInput("anova_companys", label="Company Ticker*")),
                column(7,actionButton("anova",label="Conduct Anova Test")),
                column(12,verbatimTextOutput("anovas")),
                
              ),
      ),
      #Tab 4 - Prediction
      tabItem(tabName = "predict",
              fluidRow(
                #Input UI
                column(10,h1("Stock Price Prediction")),
                column(10,h5("This page would provide you with a stock price range forecast. Accuracy and performance is dependent on the parameters you input.")),
                column(10,h5(" * Fields are required. Please enter US stocks only.")),
                column(3, textInput("ticker", label="Company Ticker*")),
                column(10, numericInput("days_to_train", label = "Number of Past Closed Prices to Consider", value = 100, step = 1)),
                column(10, numericInput("days_to_forecast", label = "Number of Closed Prices to Predict", value = 15, step = 1)),
                column(3,actionButton("start_prediction", label = "Submit"))
              ),
              fluidRow(
                #Output UI
                column(10,h3("Suggested ARIMA Model:")),
                column(8, verbatimTextOutput("summary_trained")),
                column(8, verbatimTextOutput("get_model_summary")),
                column(10,h4("Residual Independence Test (Training Set)")),
                column(8, verbatimTextOutput("get_resi_summary")),
                column(10,h3("Model Performance on Test Data:")),
                column(10,plotOutput("get_fore_test_graph")),
                column(10,h4("Model Evaluation")),
                column(10,verbatimTextOutput("get_model_accur")),
                column(10,h4("Price Estimates of Test Data")),
                column(10,verbatimTextOutput("get_forecast_test")),
                column(10,h4("Residual Independence Test (Test Set)")),
                column(8, verbatimTextOutput("get_fore_resi_summary")),
                column(10,h3("Price Forecast:")),
                column(10,plotOutput("get_fore_graph")),
                column(10, verbatimTextOutput("get_fore_values")),

              )

              )
 
    ) #close tab items

  ) #clsoe dashboard body
) # close dashboard page


# Define server logic required to draw a histogram
server <- function(input, output, session = session) {

# Part 1 Analysis Code  
  dataInput <- reactive({
    getSymbols(input$Company, src = "yahoo",
               from = input$dateRange[1],
               to = input$dateRange[2],
               auto.assign = FALSE)
  }) 
  
  finan_company_his <- reactive({
    get_financials(input$Company, report_type = "quarterly")
  })
  
  select_datatype <- reactive({
    switch(input$type, 
           "Open" = 1, "High" = 2, "Low" = 3, "Close" = 4, "Volume" = 5, "Adjusted Close" = 6)
  }) 
  
  chart <- eventReactive(input$start_historical, {
    
    df <- data.frame(Date=index(dataInput()),coredata(dataInput()))
    bbands <- BBands(dataInput()[,c(2),c(3),c(6)])
    
    df <- subset(cbind(df, data.frame(bbands[,1:3])),input$dateRange[1] <= Date & Date <= input$dateRange[2])
    
    for (i in 1:length(df[,1])) {
      if (df[i,7] >= df[i,2]) {
        df$direction[i] = 'Increasing'
      } else {
        df$direction[i] = 'Decreasing'
      }
    }
    
    i <- list(line = list(color = "red"))
    d <- list(line = list(color = "forestgreen"))
    
    fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                          open = df[,c(2)], close = df[,c(7)],
                          high = df[,c(3)], low = df[,c(4)], name = input$Company,
                          increasing = i, decreasing = d) 
    fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                             line = list(color = '#ccc', width = 0.5),
                             legendgroup = "Bollinger Bands",
                             hoverinfo = "none", inherit = F) 
    fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                             line = list(color = '#ccc', width = 0.5),
                             legendgroup = "Bollinger Bands", inherit = F,
                             showlegend = FALSE, hoverinfo = "none") 
    fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                             line = list(color = '#E377C2', width = 0.5),
                             hoverinfo = "none", inherit = F) 
    fig <- fig %>% layout(yaxis = list(title = "Price"))
    
    fig2 <- df 
    fig2 <- fig2 %>% plot_ly(x=~Date, y=df[,c(6)], type='bar', name = paste(input$Company,"Volume"),
                             color = ~direction, colors = c("red","forestgreen")) 
    fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                 list(count=1,
                      label='RESET',
                      step='all'),
                 list(count=1,
                      label='1 YR',
                      step='year',
                      stepmode='backward'),
                 list(count=3,
                      label='3 MO',
                      step='month',
                      stepmode='backward'),
                 list(count=1,
                      label='1 MO',
                      step='month',
                      stepmode='backward')
               ))
    
    # subplot with shared x axis
    fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                   shareX = TRUE, titleY = TRUE)
    fig <- fig %>% layout(title = paste(input$Company,":", input$dateRange[1], "to",input$dateRange[2]),
                          xaxis = list(rangeselector = rs),
                          legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))
    
    return (fig)
  })
  output$plot1 <- renderPlotly({chart()})
  
  
  stk_mean <- eventReactive(input$start_historical,{
    c("Mean:",round(mean(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$Mean <- renderText({stk_mean()})
  
  stk_median <- eventReactive(input$start_historical,{
    c("Median:",round(median(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$Median <- renderText({stk_median()})
  
  stk_mode <- eventReactive(input$start_historical,{
    c("Mode:",round(getmode(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$Mode <- renderText({stk_mode()})
  
  stk_range <- eventReactive(input$start_historical,{
    c("Range:",round(range(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$Range <- renderText({stk_range()})
  
  stk_IQR <- eventReactive(input$start_historical,{
    c("IQR:",round(IQR(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$IQR <- renderText({stk_IQR()})
  
  stk_Var <- eventReactive(input$start_historical,{
    c("Var:",round(var(dataInput()[,c(select_datatype())]), digits = 2))
  })
  output$Var <- renderText({stk_Var()})
  
  stk_NWC <- eventReactive(input$start_historical,{
    c("Net Working	Capital:",round(finan_company_his()$totalCurrentAssets[4]-finan_company_his()$totalCurrentLiabilities[4], digits = 2))
  })
  output$NWC <- renderText({stk_NWC()})  
  
  stk_CR <- eventReactive(input$start_historical,{
    c("Current Ratio:",round(finan_company_his()$totalCurrentAssets[4]/finan_company_his()$totalCurrentLiabilities[4], digits = 2))
  })
  output$CR <- renderText({stk_CR()})
  
  stk_DB <- eventReactive(input$start_historical,{
    c("Debt Ratio:",round(finan_company_his()$totalLiab[4]/finan_company_his()$totalAssets[4], digits = 2))
  })
  output$DB <- renderText({stk_DB()}) 
  
  stk_ROA <- eventReactive(input$start_historical,{
    c("Return on Assets:",round(finan_company_his()$netIncome[4]/finan_company_his()$totalAssets[4], digits = 2))
  })
  output$ROA <- renderText({stk_ROA()}) 
  
  stk_ROE <- eventReactive(input$start_historical,{
    c("Return on Equity:",round(finan_company_his()$netIncome[4]/finan_company_his()$totalStockholderEquity[4], digits = 2))
  })
  output$ROE <- renderText({stk_ROE()})

  ########################################################################################################
  
  # Part 2 Comparison Code
  
  CompanyData1 <- reactive({
    getSymbols(input$Company1, src = "yahoo",
               from = input$dateRange2[1],
               to = input$dateRange2[2],
               auto.assign = FALSE)
  })
  
  CompanyData2 <- reactive({
    getSymbols(input$Company2, src = "yahoo",
               from = input$dateRange2[1],
               to = input$dateRange2[2],
               auto.assign = FALSE)
  })
  
  finan_company_com1 <- reactive({
    get_financials(input$Company1, report_type = "quarterly")
  })
  
  finan_company_com2 <- reactive({
    get_financials(input$Company2, report_type = "quarterly")
  })
  
  select_datatype_com <- reactive({
    switch(input$type2, 
           "Open" = 1, "High" = 2, "Low" = 3, "Close" = 4, "Volume" = 5, "Adjusted Close" = 6)
  }) 
  
  chart_com <- eventReactive(input$start_comparison, {
    figure <- plot(CompanyData1()[,c(select_datatype_com())],
                   ylim=range(c(CompanyData1()[,c(select_datatype_com())], CompanyData2()[,c(select_datatype_com())])),
                   main=paste(input$Company1, "VS",input$Company2),lwd=1.4)
    figure <- lines(CompanyData2()[,c(select_datatype_com())], col="red", lwd=1.4)
    return(figure)
    })
  output$plot2 <- renderPlot({chart_com()})
  
  stk_ComName1 <- eventReactive(input$start_comparison,{
    c(input$Company1)
  })
  output$CompanyName1 <- renderText({stk_ComName1()})
  
  stk_mean1 <- eventReactive(input$start_comparison,{
    c("Mean:",round(mean(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$Mean1 <- renderText({stk_mean1()})
  
  stk_median1 <- eventReactive(input$start_comparison,{
    c("Median:",round(median(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$Median1 <- renderText({stk_median1()})
  
  stk_mode1 <- eventReactive(input$start_comparison,{
    c("Mode:",round(getmode(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$Mode1 <- renderText({stk_mode1()})
  
  stk_range1 <- eventReactive(input$start_comparison,{
    c("Range:",round(range(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$Range1 <- renderText({stk_range1()})
  
  stk_IQR1 <- eventReactive(input$start_comparison,{
    c("IQR:",round(IQR(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$IQR1 <- renderText({stk_IQR1()})
  
  stk_Var1 <- eventReactive(input$start_comparison,{
    c("Var:",round(var(CompanyData1()[,c(select_datatype_com())]), digits = 2))
  })
  output$Var1 <- renderText({stk_Var1()})
  
  stk_NWC1 <- eventReactive(input$start_comparison,{
    c("Net Working	Capital:",round(finan_company_com1()$totalCurrentAssets[4]-finan_company_com1()$totalCurrentLiabilities[4], digits = 2))
  })
  output$NWC1 <- renderText({stk_NWC1()})  
  
  stk_CR1 <- eventReactive(input$start_comparison,{
    c("Current Ratio:",round(finan_company_com1()$totalCurrentAssets[4]/finan_company_com1()$totalCurrentLiabilities[4], digits = 2))
  })
  output$CR1 <- renderText({stk_CR1()})
  
  stk_DB1 <- eventReactive(input$start_comparison,{
    c("Debt Ratio:",round(finan_company_com1()$totalLiab[4]/finan_company_com1()$totalAssets[4], digits = 2))
  })
  output$DB1 <- renderText({stk_DB1()}) 
  
  stk_ROA1 <- eventReactive(input$start_comparison,{
    c("Return on Assets:",round(finan_company_com1()$netIncome[4]/finan_company_com1()$totalAssets[4], digits = 2))
  })
  output$ROA1 <- renderText({stk_ROA1()}) 
  
  stk_ROE1 <- eventReactive(input$start_comparison,{
    c("Return on Equity:",round(finan_company_com1()$netIncome[4]/finan_company_com1()$totalStockholderEquity[4], digits = 2))
  })
  output$ROE1 <- renderText({stk_ROE1()})
  
  stk_ComName2 <- eventReactive(input$start_comparison,{
    c(input$Company2)
  })
  output$CompanyName2 <- renderText({stk_ComName2()})
  
  stk_mean2 <- eventReactive(input$start_comparison,{
    c("Mean:",round(mean(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$Mean2 <- renderText({stk_mean2()})
  
  stk_median2 <- eventReactive(input$start_comparison,{
    c("Median:",round(median(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$Median2 <- renderText({stk_median2()})
  
  stk_mode2 <- eventReactive(input$start_comparison,{
    c("Mode:",round(getmode(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$Mode2 <- renderText({stk_mode2()})
  
  stk_range2 <- eventReactive(input$start_comparison,{
    c("Range:",round(range(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$Range2 <- renderText({stk_range2()})
  
  stk_IQR2 <- eventReactive(input$start_comparison,{
    c("IQR:",round(IQR(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$IQR2 <- renderText({stk_IQR2()})
  
  stk_Var2 <- eventReactive(input$start_comparison,{
    c("Var:",round(var(CompanyData2()[,c(select_datatype_com())]), digits = 2))
  })
  output$Var2 <- renderText({stk_Var2()})
  
  stk_NWC2 <- eventReactive(input$start_comparison,{
    c("Net Working	Capital:",round(finan_company_com2()$totalCurrentAssets[4]-finan_company_com2()$totalCurrentLiabilities[4], digits = 2))
  })
  output$NWC2 <- renderText({stk_NWC2()})  
  
  stk_CR2 <- eventReactive(input$start_comparison,{
    c("Current Ratio:",round(finan_company_com2()$totalCurrentAssets[4]/finan_company_com2()$totalCurrentLiabilities[4], digits = 2))
  })
  output$CR2 <- renderText({stk_CR2()})
  
  stk_DB2 <- eventReactive(input$start_comparison,{
    c("Debt Ratio:",round(finan_company_com2()$totalLiab[4]/finan_company_com2()$totalAssets[4], digits = 2))
  })
  output$DB2 <- renderText({stk_DB2()}) 
  
  stk_ROA2 <- eventReactive(input$start_comparison,{
    c("Return on Assets:",round(finan_company_com2()$netIncome[4]/finan_company_com2()$totalAssets[4], digits = 2))
  })
  output$ROA2 <- renderText({stk_ROA2()}) 
  
  stk_ROE2 <- eventReactive(input$start_comparison,{
    c("Return on Equity:",round(finan_company_com2()$netIncome[4]/finan_company_com2()$totalStockholderEquity[4], digits = 2))
  })
  output$ROE2 <- renderText({stk_ROE2()})
  
  ########################################################################################################
  
  # Part 3 Hypothesis testing
  
  one_sample_HT <- eventReactive(input$one_sample_test, {
    one_sample_test(get_adjclose(input$preferred_stock,input$dateRange[1],input$dateRange[2],input$interval),input$expected,input$ci)})
  output$one_sample_test_result <- renderPrint({one_sample_HT()})
  
  two_sample_HT <- eventReactive(input$two_sample_test, {
    two_sample_test(input$market_index,input$dateRange[1],input$dateRange[2],input$preferred_stock,input$interval,input$ci2)})
  output$two_sample_test_result <- renderPrint({two_sample_HT()})
  
  #stock_matrix <- eventReactive(input$one_sample_test, {set_stock_matrix(input$preferred_stock,input$dateRange[1],input$dateRange[2],input$interval)})
  
  anova_result <- eventReactive(input$anova, {anova_test(input$anova_companys,input$dateRange[1],input$dateRange[2],input$interval)})
  output$anovas <- renderPrint({anova_result()})
  #adj_ps <- get_adjclose(input$preferred_stock,input$dateRange[1],input$dateRange[2],input$interval)
  #stock_matrix <- data.frame(adj_ps$growth_rate)
  #stock_matrix$stock_name<-rep(input$preferred_stock)
  #names(stock_matrix) = c('adjclose','stock_name')
  #add_new <- eventReactive(input$add, {add_anova_stock(input$new_company,input$dateRange[1],input$dateRange[2],input$interval)})
  
  ########################################################################################################
  
  # Part 4 Prediction Code 
  
  train_model <- eventReactive(input$start_prediction, {
    fitted_model(input$ticker, input$days_to_train)
  })
  output$summary_trained <- renderPrint({train_model()})
  
  get_model_summary <- eventReactive(input$start_prediction, {
    model_summary(input$ticker)
  })
  output$get_model_summary <- renderPrint({get_model_summary()})
  
  get_forecast_test <- eventReactive(input$start_prediction, {
    forecast_output_test(input$ticker)
  })
  output$get_forecast_test <- renderPrint({get_forecast_test()})

  get_resi_summary <- eventReactive(input$start_prediction, {
    residual_summary(input$ticker)
  })
  output$get_resi_summary <- renderPrint({get_resi_summary()})
  #output$get_resi_summary_plot <- renderImage({get_resi_summary()})
  
  get_fore_resi_summary <- eventReactive(input$start_prediction, {
    forecast_residual_summary_test(input$ticker)
  })
  output$get_fore_resi_summary <- renderPrint({get_fore_resi_summary()})
  
  get_model_accur <- eventReactive(input$start_prediction, {
    forecast_test_accurracy(input$ticker)
  })
  output$get_model_accur <- renderPrint({get_model_accur()})
  
  get_fore_test_graph <- eventReactive(input$start_prediction, {
    forecast_graph_test(input$ticker)
  })
  output$get_fore_test_graph <- renderPlot({get_fore_test_graph()})
  
  get_fore_graph <- eventReactive(input$start_prediction, {
    forecast_actual(input$ticker)
  })
  output$get_fore_graph <- renderPlot({get_fore_graph()})
  
  get_fore_values <- eventReactive(input$start_prediction, {
    forecast_values_actual
  })
  output$get_fore_values <- renderPrint({get_fore_values()})

  # forecast_output_test('SE')
  # forecast_residual_summary_test('SE')
  # forecast_test_accurracy('SE')
  # forecast_graph_test("SE")
  # forecast_actual('SE')
  
}

# Run the application 
shinyApp(ui = ui, server = server)
