library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(forecast)
library(tseries)

ui <- fluidPage(
  titlePanel("Analiza e Serive Kohore dhe Modelet Parashikuese"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Ngarko skedarin Excel", accept = c(".xlsx")),
      hr(),
      helpText("Zgjidhni një skedar Excel që përmban të dhëna të serive kohore."),
      uiOutput("variable_ui") 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vizualizimi i Serisë Kohore",
                 plotlyOutput("timeSeriesPlot")  # Grafiku i serisë kohore
        ),
        tabPanel("Autokorrelacionet",
                 plotOutput("acfPlot")  # Autokorrelacioni
        ),
        tabPanel("Regresi Linear i Shumëfishtë",
                 uiOutput("regression_y_ui"), # Përzgjedhja e variablit të varur
                 uiOutput("regression_xs_ui"), # Përzgjedhja e variablit të pavarur
                 plotOutput("multipleRegressionPlot"),
                 verbatimTextOutput("multipleRegressionSummary")
        ),
        tabPanel("Mesatarja e Lëvizshme",
                 plotOutput("movingAveragePlot")  # Mesatarja e Lëvizshme
        ),
        tabPanel("Sheshimi Eksponencial",
                 plotOutput("exponentialSmoothingPlot")  # Sheshimi Eksponencial
        ),
        tabPanel("Model Parashikues, Saktësia dhe Mbetjet e Tij",
                 selectInput("model", "Zgjidhni Modelin e Parashikimit:",
                             choices = c("ARIMA", "Drift", "ETS", "TBATS")),
                 tableOutput("accuracy_table"),
                 plotOutput("forecastPlot")
        ),
        tabPanel(
          "Modelet me Regresorë të Jashtëm",
          selectInput("external_regressor", "Zgjidh regresorin e jashtëm:",
                      choices = NULL),
          selectInput("hybrid_model", "Zgjidh Modelin me Regresorë të Jashtëm:", 
                      choices = c("RegARIMA", "Hibrit")),
          actionButton("run_hybrid", "Ekzekuto Modelin"),
          plotOutput("hybridForecastPlot"),
          plotOutput("hybridResidualPlot"),
          tableOutput("modelAccuracy")  
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Lexojmë të dhënat e Skedarit Excel 
  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # UI për zgjedhjen e variablave
  output$variable_ui <- renderUI({
    req(data())
    selectInput("variable", "Zgjidh variablin për analizën e serisë kohore:",
                choices = names(data()), selected = names(data())[1])
  })
  # UI për zgjedhjen e regresorit të jashtëm
  output$external_regressor_ui <- renderUI({
    req(data())
    selectInput("external_regressor", "Zgjidh regresorin e jashtëm:",
                choices = names(data()), selected = names(data())[2])
  })
  # Vizualizimi i Serisë Kohore
  output$timeSeriesPlot <- renderPlotly({
    req(data(), input$variable)  
    
    # Krijojmë grafikën me ggplot
    p <- ggplot(data(), aes_string(x = "Viti", y = input$variable)) +
      geom_line() +  # Vizualizojmë serinë kohore
      labs(title = paste("Vizualizimi i Serisë Kohore për", input$variable), 
           x = "Viti", 
           y = input$variable)
    # Kthejmë grafikun ggplot në një grafik interaktiv me plotly
    ggplotly(p)
  })
  
  # Autokorrelacionet
  output$acfPlot <- renderPlot({
    req(data(), input$variable)
    ts_data <- ts(data()[[input$variable]], frequency = 1)
    Acf(ts_data, main = paste("Autokorrelacionet për", input$variable))
  })
  
  # Vizualizimi i regresit linear të shumefishtë
  output$multipleRegressionPlot <- renderPlot({
    req(data(), input$regression_y, input$regression_xs)
    
    # Krijojmë formulën për regresin linear të shumefishtë
    formula_str <- paste(input$regression_y, "~", paste(input$regression_xs, collapse = "+"))
    model <- lm(as.formula(formula_str), data = data())
    
    # Përdorim të dhënat për të krijuar regresin
    predictions <- predict(model, newdata = data(), interval = "confidence")
    
    # Bashkojmë predictions me të dhënat origjinale
    data_with_predictions <- cbind(data(), predictions)
    
    # Vizualizimi i regresit linear të shumefishtë
    ggplot(data(), aes_string(x = input$regression_xs[1], y = input$regression_y)) +
      geom_point() +  # Vizualizimi i pikave
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +  # Drejtëza e regresit
      labs(title = paste("Regresi Linear për", input$regression_y, "dhe", paste(input$regression_xs, collapse = " + ")),
           x = paste(input$regression_xs, collapse = " + "),
           y = input$regression_y) + 
      theme_minimal()
  })
  
  # Shfaqim rezultatin e regresit
  output$multipleRegressionSummary <- renderPrint({
    req(data(), input$regression_y, input$regression_xs)
    
    # Krijojmë formulën për regresin linear të shumëfishtë
    formula_str <- paste(input$regression_y, "~", paste(input$regression_xs, collapse = "+"))
    model <- lm(as.formula(formula_str), data = data())
    
    # Shfaqim përmbledhjen e modelit të regresit
    summary(model)
  })
  
  # UI për zgjedhjen e variablave të pavarur për regresin linear të shumefishtë
  output$regression_xs_ui <- renderUI({
    req(data())
    selectInput("regression_xs", "Zgjidhni variablat e pavarura për regresin:",
                choices = names(data()), selected = names(data())[2:3], multiple = TRUE)
  })
  
  # UI për zgjedhjen e variablit të varur për regresin linear të shumefishtë
  output$regression_y_ui <- renderUI({
    req(data())
    selectInput("regression_y", "Zgjidh variablin e varur për regresin:",
                choices = names(data()), selected = names(data())[1])
  })
  
  # Mesatarja e Lëvizshme
  # Vizualizimi i Mesatares së Lëvizshme dhe serisë origjinale
  output$movingAveragePlot <- renderPlot({
    req(data(), input$variable)
    
    # Krijojmë serinë kohore
    ts_data <- ts(data()[[input$variable]], frequency = 1)
    
    # Krijojmë mesataren e lëvizshme
    ma_model <- ma(ts_data, order = 3)
    
    # Vizualizimi i serisë origjinale dhe mesataren e lëvizshme
    ggplot() +
      geom_line(aes(x = time(ts_data), y = ts_data, color = "Seria Origjinale"), linetype = "solid") +  # Seria origjinale
      geom_line(aes(x = time(ma_model), y = ma_model, color = "Mesatarja e Lëvizshme"), linetype = "solid") +  # Mesatarja e lëvizshme
      labs(title = paste("Seria Kohore dhe Mesatarja e Lëvizshme për", input$variable),
           x = "Koha", y = input$variable) +
      scale_color_manual(values = c("Seria Origjinale" = "blue", "Mesatarja e Lëvizshme" = "red")) +  
      theme_minimal() +
      theme(legend.title = element_blank()) 
  })
  
  # UI për zgjedhjen e variablave
  output$variable_ui <- renderUI({
    req(data())
    selectInput("variable", "Zgjidh variablin për analizën e serisë kohore:",
                choices = names(data()), selected = names(data())[1])
  })
  
  # Vizualizimi i Sheshimit Eksponencial
  output$exponentialSmoothingPlot <- renderPlot({
    req(data(), input$variable)
    
    # Krijojmë serinë kohore
    ts_data <- ts(data()[[input$variable]], frequency = 1)
    
    # Sheshimi Eksponencial me metodën Holt-Winters
    es_model <- HoltWinters(ts_data, beta = FALSE, gamma = FALSE)  # Nuk përdorim beta dhe gamma
    
    # Vizualizimi i serisë kohore dhe modelit të sheshimit eksponencial
    plot(es_model, 
         main = paste("Sheshimi i thjeshtë Eksponencial për", input$variable), 
         ylab = input$variable)  # Shtojmë etiketën e boshtit Y si variabli i përzgjedhur
    
    # Shtojmë legjendën
    legend("topright", legend = c("Seria Kohore", "Sheshimi i thjeshtë Eksponencial"),
           col = c("black", "red"), lty = c(1, 1), bty = "n")
  })
  
  # Modelet Parashikuese
  output$arimaPlot <- renderPlot({
    req(data(), input$variable)
    ts_data <- ts(data()[[input$variable]], frequency = 1)
    arima_model <- auto.arima(ts_data)
    plot(forecast(arima_model), main = "Parashikimi me ARIMA")
  })
  # Parashikim
  output$forecastPlot <- renderPlot({
    req(data(), input$variable, input$model)
    
    # Përcaktojmë vitin e fillimit nga dataset-i
    start_year <- if ("Year" %in% colnames(data())) min(data()$Year) else as.numeric(row.names(data())[1])
    
    # Krijimi i serisë kohore për të dhënat e trajnimit dhe të testimit
    ts_data <- ts(data()[[input$variable]], start = start_year, frequency = 1)
    
    # Ndarja e të dhënave në 80% trajnim dhe 20% testim
    total_length <- length(ts_data)
    train_size <- floor(0.8 * total_length)
    test_size <- total_length - train_size
    
    train_data <- window(ts_data, end = c(start_year + train_size - 1))
    test_data <- window(ts_data, start = c(start_year + train_size))
    
    # Ndërtimi i modelit bazuar në zgjedhjen e përdoruesit
    # Parashikimi për modelet e ndryshme
    forecast_result <- NULL
    if (input$model == "ARIMA") {
      model <- auto.arima(train_data)
      forecast_result <- forecast(model, h = test_size)
    } else if (input$model == "Drift") {
      model <- rwf(train_data, h = test_size, drift = TRUE)
      forecast_result <- model
    } else if (input$model == "ETS") {
      model <- ets(train_data)
      forecast_result <- forecast(model, h = test_size)
    } else if (input$model == "TBATS") {
      model <- tbats(train_data)
      forecast_result <- forecast(model, h = test_size)
    }
    req(forecast_result)
    
    # Llogaritja e saktësisë
    accuracy_result <- accuracy(forecast_result, test_data)
    
    # Shfaqja e tabelës së saktësisë së modelit
    output$accuracy_table <- renderTable({
      accuracy_df <- as.data.frame(accuracy_result)  
      accuracy_df$Metric <- rownames(accuracy_df)  
      accuracy_df  
    })
    
    # Vizualizimi i modelit
    y_limits <- range(c(ts_data, forecast_result$mean, forecast_result$lower, forecast_result$upper))
    
    par(mfrow = c(2, 2))
    
    # Grafik 1: Parashikimi dhe të dhënat reale
    plot(forecast_result, main = paste("Parashikimi me Modelin", input$model),
         ylab = input$variable, xlab = "Viti", ylim = y_limits)
    lines(test_data, col = "red", lwd = 2)
    
    # Grafik 2: Kontrolli i mbetjeve
    residuals <- residuals(model)
    plot(residuals, main = "Mbetjet e Modelit", ylab = "Mbetjet", xlab = "Viti", type = "o", col = "blue")
    abline(h = 0, col = "red", lty = 2)
    
    # Grafik 3: Histogrami i mbetjeve
    hist(residuals, main = "Shpërndarja e Mbetjeve", xlab = "Mbetjet", col = "lightblue", breaks = 10)
    
    # Grafik 4: Shpërndarja e mbetjeve
    qqnorm(residuals, main = "Grafiku Q-Q për Mbetjet")
    qqline(residuals, col = "red", lty = 2)
  })
  # Përditësojmë listën e regresorëve të jashtëm
  observe({
    req(data())
    updateSelectInput(session, "external_regressor", choices = c("Zgjedhni Regresorin" = "", names(data())))
  })
  
  # Njoftim për përdoruesin !!! 
  # Kujdes kur zgjidhni regresorin e jashtëm duhet të jetë i ndryshëm nga variabli i varur përndryshe app ju mbyllet
  
  # Modelet me regresorë të jashtëm
  observeEvent(input$run_hybrid, {
    req(data(), input$variable, input$external_regressor)
    
    # Nëse përdoruesi nuk ka zgjedhur një regresor ose model, përdorim të gjitha mundësitë
    if(input$external_regressor == "") {
      external_regressor <- names(data())[1]  # Përdor regresorin e parë si parazgjedhje
    } else {
      external_regressor <- input$external_regressor
    }
    
    if(input$hybrid_model == "") {
      hybrid_model <- "RegARIMA"  # Parazgjedhje për modelin me regresorë të jashtëm
    } else {
      hybrid_model <- input$hybrid_model
    }
    
    # Përgatitja e serive kohore dhe ndarja në trajnim/test
    ts_data <- ts(data()[[input$variable]], frequency = 1)
    external_data <- data()[[external_regressor]]
    
    start_year <- if ("Year" %in% colnames(data())) min(data()$Year) else 1
    total_length <- length(ts_data)
    train_size <- floor(0.8 * total_length)
    
    train_data <- ts(ts_data[1:train_size], start = start_year, frequency = 1)
    test_data <- ts(ts_data[(train_size + 1):total_length], start = start_year + train_size, frequency = 1)
    
    train_regressor <- external_data[1:train_size]
    test_regressor <- external_data[(train_size + 1):total_length]
    
    # RegARIMA
    if (hybrid_model == "RegARIMA") {
      arima_model <- auto.arima(train_data, xreg = train_regressor)
      forecast_arima <- forecast(arima_model, xreg = test_regressor, h = length(test_data))
      
      # Grafiku i parashikimit
      output$hybridForecastPlot <- renderPlot({
        autoplot(forecast_arima) +
          autolayer(test_data, series = "Test Data", PI = FALSE, color = "red") +
          ggtitle("Parashikimi RegARIMA") +
          scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10),
            labels = scales::label_number()
          ) +
          theme_minimal() +
          xlab("Viti") + ylab("Vlera")
      })
      
      # Grafiku i mbetjeve
      output$hybridResidualPlot <- renderPlot({
        checkresiduals(arima_model)
      })
      
      # Tabela e saktësisë për modelin RegARIMA
      accuracy_arima <- accuracy(forecast_arima, test_data)
      output$modelAccuracy <- renderTable({
        as.data.frame(accuracy_arima)  
      })
    }
    
    # Modeli Hibrit 
    if (hybrid_model == "Hibrit") {
      ets_model <- ets(train_data)
      forecast_ets <- forecast(ets_model, h = length(test_data))
      
      # Grafiku i parashikimit
      output$hybridForecastPlot <- renderPlot({
        autoplot(forecast_ets) +
          autolayer(test_data, series = "Test Data", PI = FALSE, color = "red") +
          ggtitle("Hibrit") +
          scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10),
            labels = scales::label_number()
          ) +
          theme_minimal() +
          xlab("Viti") + ylab("Vlera")
      })
      
      # Grafiku i mbetjeve
      output$hybridResidualPlot <- renderPlot({
        checkresiduals(ets_model)
      })
      
      # Tabela e saktësisë për modelin Hibrit
      accuracy_ets <- accuracy(forecast_ets, test_data)
      output$modelAccuracy <- renderTable({
        as.data.frame(accuracy_ets) 
      })
    }
  })
}    
# Ekzekutojmë aplikacionin
shinyApp(ui = ui, server = server)

#Faleminderit !!! 😃😃