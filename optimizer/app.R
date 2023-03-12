library(shiny)
library(shinythemes)
library(lubridate)
library(quantmod)
library(shinyWidgets)
library(scales)
library(ggplot2)
# library(nloptr)
library(data.table)
library(shinyjs)

# Sortino ratio functions
sortino_ratio <- function(weights,returns, benchmark_returns,average_yearly_benchmark_return, target_return,rfr) {
  weights <- weights/sum(weights)
  downside_betas <- numeric(ncol(returns))
  for (i in 1:ncol(returns)) {
    downside_betas[i] <- coef(lm(data.frame(returns[, ..i], benchmark_returns[, 1]), subset=benchmark_returns[, 1]<0))[2]
  }
  DCAPM_expected_return <- sum(weights*(rfr+downside_betas*(average_yearly_benchmark_return-rfr)))
  
  target_daily_return <- (1+target_return)^(1/252)-1
  historical_returns <- data.table(historical_return = rowSums(returns * weights))
  historical_yearly_return <- (sum(weights*apply(returns,MARGIN=2, function(columns){prod(1 + columns) - 1}))+1)^(252/nrow(returns))-1
  zero_data_table <- as.data.table(rep(0,nrow(returns)))
  downside_deviation <- sqrt(sum(pmax(target_daily_return-historical_returns, zero_data_table)^2) /(nrow(returns) - 1)*252)
  
  return(-(historical_yearly_return - target_return) / downside_deviation)
}




# Define the objective function to be minimized (the negative Sortino ratio)
objective <- function(weights, returns, benchmark_returns, average_yearly_benchmark_return, target_return, rfr) {
  sortino_ratio(weights, returns, benchmark_returns, average_yearly_benchmark_return, target_return, rfr)
}

url <- "https://raw.githubusercontent.com/SimonPodstavek/Post-modern-portfolio-theory-/main/doc_files/stock_underlying.csv"
data <- read.csv(url)
colnames(data) <- c("Volatilita","Výnos")






  # Define UI
  ui <- fluidPage(theme = shinytheme("superhero"),
                  
                  tags$head(
                    tags$style(HTML("
              body{
                     font-family: 'Myriad Pro', Myriad, 'Liberation Sans', 'Nimbus Sans L', 'Helvetica Neue', Helvetica, Arial, sans-serif;
              }      
                    
             table, th, td {
              # width: 100%;
              border-collapse: collapse;
              margin: 1em;
              table-layout: fixed;
              font-family: 'Times New Roman', Times, serif;
            }

               th, td {
              text-align: center;
              border-left: none;
            }


             td {
              background-color: #4E5D6C;
              box-shadow: 0px 0px 0px 1px #fff;
            }

               tr:nth-child(even) td {
              background-color: #7C91A3;
            }
          .dates tr{
          background-color: #red!important;
          }

               #optimal_allocation_table td:first-child:first-child{
                background-color: #1D2933;
                font-weight: bold;
              font-size: 1.2em;
              text-align:left;
               }
            
             #optimal_portfolio_indicators_tablez td:first-child:first-child{
                background-color: #1D2933;
                font-weight: bold;
              font-size: 1.2em;
              text-align:left;
             }
            
             #selected_financial_indicators_table td:first-child:first-child{
                background-color: #1D2933;
                font-weight: bold;
              font-size: 1.2em;
              text-align:left;
            }

               th:first-child{
                text-align:left!important;
            }

               th{
              background-color: #1D2933;
              font-weight: bold;
              font-size: 1.2em;
              text-align: center !important;
            }


            #optimal_allocation_table table{
                     width: 100% !important;
            }
            #optimal_allocation_table th, td{
                     width: 16%;
            }
              #selected_financial_indicators_table th,#selected_financial_indicators_table td{
                    width:14%;
              }

            .reset_data_btn{
                color: #fff;
                background-color: #337ab7;
                border-color: #2e6da4;
            }


    "))),
                  
                  
    useShinyjs(),             
    navbarPage(
      "pmpt.online",
      tabPanel("Optimalizácia portfólia",
               sidebarPanel(
                 
                 
                 tags$h3("Optimalizácia portfólia"),
                 br(),
                 dateRangeInput("dates", label = h5("Historický rozsah dát:"),start = Sys.Date() %m-% years(5), language = 'sk'),
                 # numericInput("rfr_input", "Bezriziková sadzba:", value=3.51, step=0.01, width=80),
                 
                 
                 
                 br(),
                 tags$h5("Informacie o investorovi:"),
                   fluidRow(
                     column(width = 6, formatNumericInput("RFR_input", "Bezriziková sadzba", value=0.0351, width=NULL, format = "percentageEU2dec"),),
                     column(width = 6, formatNumericInput("MAR_input", "Minimálny  výnos", value=0.05, width=NULL, format = "percentageEU2dec"),),
                   ),
                 currencyInput("currency",label = "Veľkosť portfólia",value=10000,format = "dollar",width = NULL,align = "center"),
                 
                 
                 
                 
                 br(),
                 tags$h5("Nastavenia optimalizácie:"),
                 sliderInput("monte_carlo_slider", label = "Počet simulácií", min = 10, 
                             max = 500, value = 200),
                 sliderInput("assets_slider", label = "Počet aktív", min = 2, 
                             max = 7, value = 2),
                 selectInput(inputId = "ticker_benchmark",label = "Benchmark",choices = c("S&P 500 (USD)", "NASDAQ (USD)","Dow Jones Industrial Average (USD)", "Wilshire 5000 (USD)")),
                 
                 
                 br(),
                 tags$h5("Zadajte zamýšlané aktíva v portfóliu:"),
                 uiOutput("fields"),
                 actionButton("submit_optimization", "Stiahni Dáta", class = "btn-success"),
                 hidden(
                   div(id="selected_tickers", ""),
                   actionButton("reset_data_btn", "Zmeniť tickery", class = "reset_data_btn")
                 ),
               ),
               mainPanel(hidden(div(id="optimalization_output",
              
                            h1("Výsledok optimalizácie"),
                            h5("Optimálna alokácia:"),
                            tableOutput("optimal_allocation_table"),
                            h5("Vybrané štatistické vlastnosti optimálneho portfólia:"),
                            tableOutput("optimal_portfolio_indicators_table"),
                            h5("Vybrané finančné ukazovateľe finančných aktív:"),
                            tableOutput("selected_financial_indicators_table"),
                            div(id="dcml_slope", ""),
                            h5("Efektívna hranica ako výsledok simulácie:"),
                            plotOutput("efficient_frontier_plot"),
                            uiOutput("Console")

               )))
               
      ), # Navbar 1, tabPanel
      tabPanel("Manuál", "This panel is intentionally left blank"),
      tabPanel("O pmpt.online", "This panel is intentionally left blank"),
      tabPanel("Autor", "This panel is intentionally left blank"),
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {
    observeEvent(input$button, {
      toggle("hello")
    })
    
    asset_names <- reactive({
      names <- c()
    for (i in 1:input$assets_slider) {
      input_name <- paste0("ticker_", i)
      if (!is.null(input[[input_name]])){
        names <- c(names, input[[input_name]])
      }
    }
      toupper(names)
  })
    
    benchmark_name <<- reactive({
      indexes_names <- c("S&P 500 (USD)", "NASDAQ (USD)","Dow Jones Industrial Average (USD)", "Wilshire 5000 (USD)")
      indexes_tickers <- c("SPY", "QQQ","DIA","SDY")
      indexes_tickers[which(indexes_names==input$ticker_benchmark)]
    })
    
    
    
    
    
    

    #Adds assset fields based on a slider
    output$fields <- renderUI({
      
      num_assets_slider <- input$assets_slider
      field_list <- lapply(1:num_assets_slider, function(i) {
        textInput(paste0("ticker_", i), label = paste0("Aktívum ", i, ":"))
      })
      do.call(tagList, field_list)
    })


    
    
    
    #BUTTON to change securities 
    
    
    observeEvent(input$reset_data_btn, {
      shinyjs::hide("optimalization_output")
      for (i in 1:input$assets_slider) {
        shinyjs::enable(paste0("ticker_", i))
      }
      shinyjs::show("submit_optimization")
      shinyjs::hide("selected_tickers")
      shinyjs::hide("reset_data_btn")
      shinyjs::enable("assets_slider")
      shinyjs::enable("ticker_benchmark")
      shinyjs::enable("dates")
      html("selected_tickers", "")
      
    })
    
    
    
    
    
    
    
    #BUTTON PRESS PLot efficeintf rontier and download data
    
    
    observeEvent(input$submit_optimization, {
      for (i in 1:input$assets_slider) {
        shinyjs::disable(paste0("ticker_", i))
      }
      shinyjs::disable("dates")
      shinyjs::disable("assets_slider")
      shinyjs::disable("ticker_benchmark")
      shinyjs::hide("submit_optimization")
      shinyjs::show("selected_tickers")
      shinyjs::show("reset_data_btn")
      html("selected_tickers", paste("Pracujem s dátami pre: ", paste(asset_names(),collapse=", ")))

      
      
      getSymbols(asset_names(),source="yahoo", from=input[["dates"]][1], to=input[["dates"]][2])
      returns <- c()
      returns <- cbind(returns, do.call(cbind, lapply(asset_names(), function(i) {Delt(Cl(get(i)))})))
      
      # get benchmark data
      getSymbols(benchmark_name(), from=input[["dates"]][1], to=input[["dates"]][2])
      benchmark_returns <- Delt(Ad(get(benchmark_name())))
      
      
      returns <<- as.data.table(returns)[-1,-1]
      benchmark_returns <<- as.data.table(benchmark_returns)[-1,-1]
      
  
      average_yearly_benchmark_return <<- (as.numeric(last(Ad(get(benchmark_name()))))/as.numeric(first(Ad(get(benchmark_name())))))^(252/nrow(returns))-1
      # average_yearly_benchmark_return <<- (as.numeric(last(Ad(get(benchmark_name()))))/as.numeric(first(Ad(get(benchmark_name())))))^(1/(nrow(returns)/252))-1
      
      
      
      

      output$efficient_frontier_plot <- renderPlot({
        set.seed(sum(as.numeric(charToRaw(paste(asset_names(), collapse = "")))))
        subset_data <- data[sample(nrow(data), input$monte_carlo_slider),]
        req(input$submit_optimization)
        ggplot(subset_data, aes(Volatilita, Výnos)) +
          geom_point() +
          labs(title = "Efektívna hranica portfóia PMPT")
      })
      shinyjs::show("optimalization_output")
    })

    
    
    #Optimize and create optimal allocation table
    output$optimal_allocation_table <- renderTable({
      req(input$submit_optimization)
      
      initial_weights <- rep(1/ncol(returns), ncol(returns))
      target_return <- input$MAR_input
      rfr <- input$RFR_input
      
      
      # Set the initial weights
      initial_weights <- rep(1/ncol(returns), ncol(returns))
        
      # Define the lower and upper bounds for the weights
      lower_bounds <- c(rep(0, ncol(returns)))
      upper_bounds <- c(rep(1, ncol(returns)))
      print(apply(returns,2,function(x){prod(1+x)-1}))
      
      result = optim(
        par = initial_weights,
        fn = objective,
        returns = returns,
        benchmark_returns = benchmark_returns,
        average_yearly_benchmark_return = average_yearly_benchmark_return,
        target_return = target_return,
        rfr = rfr,
        method = "L-BFGS-B",
        lower = lower_bounds,
        upper = upper_bounds
      )
      
      optimal_weights <<- result$par
      optimal_weights <<- optimal_weights/sum(optimal_weights)
      
      #optimal sortino is inverse of result, as optimizer is minimizing the function
      Optimal_sortino <<- -result$value
      
      
      nominal_currency <- optimal_weights * input$currency
     
       data <- data.frame(
        asset_names(),
        asset_allocation <- percent(optimal_weights,accuracy=0.01),
        asset_nominal_allocation= dollar(nominal_currency, digits = 2)
      )
      data <- t(data)
      data <- cbind(c("Aktívum", "Zastúpenie", "Nominálne"),data)
      colnames(data) <- data[1,]
      data <- data[-1,]
      data 
    },class="formated_table")



    
    
    #Get expected yearly return (D-CAPM) -> c(eq.weight, optimal)
    
    expected_yearly_return <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      number_of_assets <- ncol(returns)
      
      downside_betas <- numeric(ncol(returns))
      for (i in 1:ncol(returns)) {
        downside_betas[i] <- coef(lm(data.frame(returns[, ..i], benchmark_returns[, 1]), subset=benchmark_returns[, 1]<0))[2]
      }
      
      equal_weighted_portfolio_expected_return <- sum(1/number_of_assets*(rfr+downside_betas*(average_yearly_benchmark_return-rfr)))
      optimal_portfolio_expected_return <- sum(optimal_weights*(rfr+downside_betas*(average_yearly_benchmark_return-rfr)))
      c(equal_weighted_portfolio_expected_return,optimal_portfolio_expected_return)
      
    })
    
    
    #Get historical yearly return (geo.mean p.d. based on downside beta) -> c(eq.weight, optimal)
    historical_yearly_return <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      number_of_assets <- ncol(returns)
      
      equal_weighted_portfolio_historical_return <- (sum(1/number_of_assets*apply(returns,MARGIN=2, function(columns){prod(1 + columns) - 1}))+1)^(252/nrow(returns))-1
      optimal_portfolio_historical_return <- (sum(optimal_weights*apply(returns,MARGIN=2, function(columns){prod(1 + columns) - 1}))+1)^(252/nrow(returns))-1
      c(equal_weighted_portfolio_historical_return,optimal_portfolio_historical_return)
      
    })
    
    
    
    
    #Get standard deviation -> c(eq.weight, optimal)
    portfolio_standard_deviation <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      number_of_assets <- ncol(returns)
      
      equal_weighted_portfolio_variance <- sd(apply(returns, 1, function(row){return(sum(row/number_of_assets))}))*sqrt(252)
      optimal_portfolio_variance <- sd(apply(returns, 1, function(row){return(sum(row*optimal_weights))}))*sqrt(252)
      c(equal_weighted_portfolio_variance,optimal_portfolio_variance)
      
    })
    
    
    
    
    
    
    #Get downside standard deviation and sortino (hist.return) -> c(DD.eq.weight,DD.optimal, Sortino eq.weight, Sortino optimal)
    portfolio_downside_standard_deviation_and_sortino <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      mar <- input$MAR_input
      number_of_assets <- ncol(returns)
      
      downside_betas <- numeric(ncol(returns))
      for (i in 1:ncol(returns)) {
        downside_betas[i] <- coef(lm(data.frame(returns[, ..i], benchmark_returns[, 1]), subset=benchmark_returns[, 1]<0))[2]
      }
      
      target_daily_return <- (1+mar)^(1/252)-1
      zero_data_table <- as.data.table(rep(0,nrow(returns)))
      
      
      historical_returns_equal_weight <- data.table(historical_return = rowSums(returns/number_of_assets))
      downside_deviation_equal_weighted_portfolio <- sqrt(sum(pmax(target_daily_return-historical_returns_equal_weight, zero_data_table)^2) /(nrow(returns) - 1)*252)
      equal_weight_sortino <- -sortino_ratio(1/ncol(returns),returns, benchmark_returns,average_yearly_benchmark_return, mar,rfr)
      
      historical_returns_optimal <- data.table(historical_return = rowSums(returns * optimal_weights))
      downside_deviation_optimal_portfolio <- sqrt(sum(pmax(target_daily_return-historical_returns_optimal, zero_data_table)^2) /(nrow(returns) - 1)*252)
      optimal_sortino <- -sortino_ratio(optimal_weights,returns, benchmark_returns,average_yearly_benchmark_return, mar,rfr)
      
    
      c(downside_deviation_equal_weighted_portfolio,downside_deviation_optimal_portfolio,equal_weight_sortino,optimal_sortino)
      
    })
    
    
    
    
    #Get variance, downside and upside variance and variance skewness -> c(var. eq.weight, var. optimal, upside var. eq.weight %, upside var. optimal %, downside var. eq.weight %, downside var. optimal %, vol. skewness eq.weight, vol. skewness optimal)
    portfolio_variance_measures <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      number_of_assets <- ncol(returns)
      zero_data_table <- as.data.table(rep(0,nrow(returns)))
      
      
      #Calculating variance
      historical_returns_equal_weight <- data.table(historical_return = rowSums(returns/number_of_assets))
      equal_weighted_portfolio_variance <- var(apply(returns, 1, function(row){return(sum(row/number_of_assets))}))*252
      
      

      historical_returns_optimal <- data.table(historical_return = rowSums(returns * optimal_weights))
      optimal_portfolio_variance <- var(apply(returns, 1, function(row){return(sum(row*optimal_weights))}))*252

      
      #Calculating upside variance
      equal_weighted_portfolio_upside_variance <- sum(pmax(historical_returns_equal_weight, zero_data_table)^2)/(nrow(returns) - 1)*252
      optimal_portfolio_upside_variance <- sum(pmax(historical_returns_optimal, zero_data_table)^2)/(nrow(returns) - 1)*252
      
      
      
      
      #Calculating upside variance as percentage of variance
      equal_weighted_portfolio_upside_variance_as_percentage_variance <- equal_weighted_portfolio_upside_variance/equal_weighted_portfolio_variance
      optimal_portfolio_upside_variance_as_percentage_variance <- optimal_portfolio_upside_variance/optimal_portfolio_variance
      
      #Calculating downside variance as percentage of variance
      equal_weighted_portfolio_downside_variance_as_percentage_variance <- 1-equal_weighted_portfolio_upside_variance_as_percentage_variance
      optimal_portfolio_downside_variance_as_percentage_variance <- 1-optimal_portfolio_upside_variance_as_percentage_variance
      
      #Calculating volatility skewness
      equal_weighted_portfolio_volatility_skewness <- equal_weighted_portfolio_upside_variance/(equal_weighted_portfolio_variance-equal_weighted_portfolio_upside_variance)
      optimal_portfolio_volatility_skewness <- optimal_portfolio_upside_variance/(optimal_portfolio_variance-optimal_portfolio_upside_variance)
      
      c(equal_weighted_portfolio_variance,optimal_portfolio_variance,equal_weighted_portfolio_upside_variance_as_percentage_variance,optimal_portfolio_upside_variance_as_percentage_variance,
        equal_weighted_portfolio_downside_variance_as_percentage_variance,optimal_portfolio_downside_variance_as_percentage_variance,equal_weighted_portfolio_volatility_skewness,optimal_portfolio_volatility_skewness)
    })
    
    
    
    
    
    
    asset_indicators <- reactive({
      req(input$submit_optimization)
      asset_and_benchmark_returns <- cbind(returns,benchmark_returns)
      rfr <- input$RFR_input
      benchmark_return <- apply(benchmark_returns,MARGIN=2, function(columns){prod(1 + columns)})^(252/nrow(returns))-1
      
      
      #Calculating average asset returns
      asset_average_yearly_return <- apply(asset_and_benchmark_returns,MARGIN=2, function(columns){prod(1 + columns)})^(252/nrow(returns))-1
      
      #Calculating asset beta
      asset_beta <-coefficients(lm(as.matrix(asset_and_benchmark_returns) ~ as.matrix(benchmark_returns)))[2,]
      
      #Calculating asset downside beta
      asset_downside_beta <- numeric(ncol(asset_and_benchmark_returns))
      for (i in 1:ncol(returns)) {
        asset_downside_beta[i] <- coef(lm(data.frame(asset_and_benchmark_returns[, ..i], benchmark_returns[, 1]), subset=benchmark_returns[, 1]<0))[2]
      }
      
      #Calculating asset D-CAPM
      asset_capm_return <- lapply(asset_beta, function(beta){rfr+beta*(benchmark_return-rfr)})
      #Calculating asset CAPM
      asset_dcapm_return <- lapply(asset_downside_beta, function(beta){rfr+beta*(benchmark_return-rfr)})
      list(asset_average_yearly_return,asset_beta,asset_downside_beta,asset_capm_return,asset_dcapm_return)
      
    })
    
    
    
    dcapm_slope <- reactive({
      req(input$submit_optimization)
      rfr <- input$RFR_input
      mar <- input$MAR_input
      target_daily_return <- (1+mar)^(1/252)-1
      zero_data_table <- as.data.table(rep(0,nrow(benchmark_returns)))
      
      benchmark_downside_deviation <- sqrt(sum(pmax(target_daily_return-benchmark_returns,zero_data_table)^2)/(nrow(benchmark_returns)-1)*252)
      
      (mar-rfr)/benchmark_downside_deviation
      
    })
    
    
    # Selected financial indicators table
    output$selected_financial_indicators_table <- renderTable({
      req(input$submit_optimization)
      
      html("dcml_slope", paste("Sklon DCML pre benchmark: ", dcapm_slope()))
      
      data <- data.frame(
        asset_names <- c(asset_names(),input$ticker_benchmark),
        asset_average_yearly_return <- percent(unlist(asset_indicators()[1]), accuracy = 0.01),
        asset_beta <- unlist(asset_indicators()[2]),
        asset_downside_beta <- unlist(asset_indicators()[3]),
        asset_capm_return <- percent(unlist(asset_indicators()[4]), accuracy = 0.01),
        asset_dcapm_return <- percent(unlist(asset_indicators()[5]), accuracy = 0.01)
      )
      data <- t(data)
      data <- cbind(c("Aktívum", "Priemerný ročný výnos", "Beta", "Spodná beta", "CAPM výnos", "D-CAPM výnos"),data)
      colnames(data) <- data[1,]
      data <- data[-1,]
      data
    },class="formated_table")
    

    
    

    # Optimal portfolio indicators table
    output$optimal_portfolio_indicators_table <- renderTable({
      req(input$submit_optimization)
      
      allocation_percent <- c(0.445, 0.109, 0, 0, 0.446)
        nominal_currency <- allocation_percent * input$currency
        target_return <- input$RFR_input
        rfr <- input$RFR_input
        number_of_assets <- ncol(returns)
      
      
      data <- data.frame(
        expected_yearly_return <- percent(expected_yearly_return(),accuracy = 0.01),
        historical_yearly_return <- percent(historical_yearly_return(),accuracy = 0.01),
        portfolio_volatility <- percent(portfolio_standard_deviation(), accuracy = 0.01),
        downside_volatility <- percent(portfolio_downside_standard_deviation_and_sortino()[1:2], accuracy = 0.01),
        sortino_ratio <- c(portfolio_downside_standard_deviation_and_sortino()[3:4]),
        portfolio_variance <- percent(portfolio_variance_measures()[1:2], accuracy = 0.001),
        upside_variance_percent <- percent(portfolio_variance_measures()[3:4], accuracy = 0.01),
        downside_variance_percent <- percent(portfolio_variance_measures()[5:6], accuracy = 0.01),
        variance_skewness <- percent(portfolio_variance_measures()[7:8], accuracy = 0.01)
      )
      # data <- t(data)
      data <- cbind(c("Rovnaké váhy", "Optimálne portfólio PMPT"),data)
      colnames(data) <- c("Ukazateľ","Očakávaný ročný výnos", "Historický ročny výnos","Volatilita", "Spodná volatilita", "Sortino", "Variancia","Horná variancia ", "Dolná variancia","Zošikmenie variancie")
      data
    },class="formated_table")


    
    
  }
  
  
  
  

  shinyApp(ui = ui, server = server)

  