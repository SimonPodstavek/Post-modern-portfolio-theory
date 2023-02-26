library(shiny)
library(shinythemes)
library(lubridate)
library(quantmod)
library(shinyWidgets)
library(scales)
library(ggplot2)

url <- "https://raw.githubusercontent.com/SimonPodstavek/Post-modern-portfolio-theory-/main/doc_files/stock_underlying.csv"
data <- read.csv(url)
colnames(data) <- c("Volatilita","Výnos")



  # Define UI
  ui <- fluidPage(theme = shinytheme("superhero"),
                  
                  tags$head(
                    tags$style(HTML("
             table, th, td {
              width: 100% !important;
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
          
               td:first-child:first-child {
                background-color: #1D2933;
                font-weight: bold;
              font-size: 1.2em;
              text-align:left;
            }
            
               th:first-child{
                text-align:left!important;
            }
            
               th {
              background-color: #1D2933;
              font-weight: bold;
              font-size: 1.2em;
              text-align: center !important;
            }
          
          
          
            #optimal_allocation_table th, td{
                     width: 16%;
            }
              #selected_financial_indicators_table th,#selected_financial_indicators_table td{
              
                    width:14%;
            }


    "))),
                  
                  
                  
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
                             max = 7, value = 5),
                 selectInput(inputId = "ticker_benchmark",label = "Benchmark",choices = c("S&P 500 (USD)", "NASDAQ (USD)","MSCI World (USD)","Dow Jones Industrial Average (USD)", "Russell 2000 (USD)")),
                 
                 
                 br(),
                 tags$h5("Zadajte zamýšlané aktíva v portfóliu:"),
                 uiOutput("fields"),
                 actionButton("submit_optimization", "Stiahni Dáta", class = "btn-success")
               ),
               mainPanel(
                            h1("Výsledok optimalizácie"),
                            h5("Optimálna alokácia:"),
                            tableOutput("optimal_allocation_table"),
                            h5("Vybrané štatistické vlastnosti optimálneho portfólia:"),
                            tableOutput("optimal_portfolio_indicators_table"),
                            h5("Vybrané finančné ukazovateľe finančných aktív:"),
                            tableOutput("selected_financial_indicators_table"),
                            h5("Efektívne hranica ako výsledok simulácie, CML:"),
                            plotOutput("efficient_frontier_plot"),
                            uiOutput("Console")

               )
               
      ), # Navbar 1, tabPanel
      tabPanel("Manuál", "This panel is intentionally left blank"),
      tabPanel("O pmpt.online", "This panel is intentionally left blank"),
      tabPanel("Autor", "This panel is intentionally left blank"),
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    
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
    
    
    
    
    # observeEvent(input$submit_optimization, {
    #   price_daily_change <- data.frame(date = index(getSymbols(asset_names()[[1]], auto.assign = FALSE, from = "2020-01-01")))
    #   for (i in seq_along(asset_names())) {
    #     asset <- asset_names()[i]
    #     prices_df <- getSymbols(asset, auto.assign = FALSE, from = "2020-01-01")
    #     funguj_preboa <- Delt(prices_df[, paste(asset, "Adjusted", sep = ".")])
    #     price_daily_change[asset] <- as.data.frame(funguj_preboa)["Delt.1.arithmetic"]
    #   }
    #   print(price_daily_change)
    # })
    # 
    

    #Adds assset fields based on a slider
    output$fields <- renderUI({
      num_assets_slider <- input$assets_slider
      field_list <- lapply(1:num_assets_slider, function(i) {
        textInput(paste0("ticker_", i), label = paste0("Aktívum ", i, ":"))
      })
      do.call(tagList, field_list)
    })



    
    #Optimal alocation table
    output$optimal_allocation_table <- renderTable({
      req(input$submit_optimization)
      allocation_percent <- c(0.445, 0.109, 0, 0, 0.446)
      nominal_currency <- allocation_percent * input$currency
      
      
      data <- data.frame(
        asset_names(),
        asset_allocation <- percent(allocation_percent),
        asset_nominal_allocation= dollar(nominal_currency, digits = 2)
      )
      data <- t(data)
      data <- cbind(c("Aktívum", "Zastúpenie", "Nominálne"),data)
      colnames(data) <- data[1,]
      data <- data[-1,]
      data
    },class="formated_table")



    
    
    

    # Optimal portfolio indicators table
    output$optimal_portfolio_indicators_table <- renderTable({
      req(input$submit_optimization)
      
      allocation_percent <- c(0.445, 0.109, 0, 0, 0.446)
        nominal_currency <- allocation_percent * input$currency
      
      
      data <- data.frame(
        expected_yearly_return <- percent(c(0.0880, 0.0966)),
        historical_yearly_return <- percent(c(0.1556,0.2192)),
        portfolio_volatility <- percent(c(0.2257,0.2572)),
        downside_volatility <- percent(c(0.1634,0.1808)),
        sortino_ratio <- c(0.1711,0.2023),
        # Check grammar
        portfolio_variance <- percent(c(0.000202,0.000262)),
        variance_skewness <- c(0.851156089,0.9350),
        upside_variance_percent <- percent(c(0.459797039,0.48319)),
        downside_variance_percent <- percent(c(0.540202961,0.51680))
      )
      # data <- t(data)
      data <- cbind(c("Rovnaké váhy", "Optimálne portfólio PMPT"),data)
      colnames(data) <- c("Ukazateľ","Očakávaný ročný výnos", "Historický ročny výnos","Volatilita", "Spodná volatilita", "Sortino", "Variancia","Zošikmenie variancie","Horná variancia ", "Dolná variancia")
      data
    },class="formated_table")
    
    
    # Selected fincial indicators table
    output$selected_financial_indicators_table <- renderTable({
      req(input$submit_optimization)
      
      allocation_percent <- c(0.445, 0.109, 0, 0, 0.446)
      nominal_currency <- allocation_percent * input$currency
      
      data <- data.frame(
        asset_names <- c(asset_names(),input$ticker_benchmark),
        asset_average_yearly_return <- percent(c(0.210982073, 0.22196575, 0.051543085,-0.075197085, 0.095830424,0.082335331)),
        asset_beta <- c(1.225, 1.134, 0.646, 1.115, 1.256,1.000),
        asset_downside_beta <- c(1.162, 1.071, 0.804, 1.239, 1.276,NA),
        asset_capm_return <- percent(c(0.097415, 0.091681, 0.060868, 0.090462, 0.099354,NA)),
        asset_dcapm_return <- percent(c(0.093380, 0.06767, 0.050883, 0.07833, 0.080809,NA))
      )
      data <- t(data)
      data <- cbind(c("Aktívum", "Priemerný ročný výnos", "Beta", "Spodná beta", "CAPM výnos", "D-CAPM výnos"),data)
      colnames(data) <- data[1,]
      data <- data[-1,]
      data
    },class="formated_table")
    

    # Efficient frontier plot
    
    
    observeEvent(input$submit_optimization, {
      output$efficient_frontier_plot <- renderPlot({
        subset_data <- data[sample(nrow(data), input$monte_carlo_slider),]
        
        
        req(input$submit_optimization)
        ggplot(subset_data, aes(Volatilita, Výnos)) +
          geom_point() +
          labs(title = "Efektívna hranica portfóia PMPT")
      })
    })
    
    
  }
  
  
  
  

  shinyApp(ui = ui, server = server)
