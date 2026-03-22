library(shiny)
library(bslib)
source("arma.R")


ui <- page_sidebar(
  
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#2C3E50"),

  title = "Time Series Model Behaviour Explorer",

  
  sidebar = sidebar(
    
  
    h4("Data Generating Process"),
    p("These inputs control the time series that gets simulated."),
    
    textInput(inputId = "p_val",
              label = "AR coffecients (\\(\\phi_i\\))",
              value = "0.60,-0.30"),
    
    textInput(inputId = "q_val",
              label = "MA coffecients (\\(\\theta_i\\))",
              value = "0.45,-0.20"),
    
    
    numericInput(inputId = "n_val",
                 label = "Sample size (n)",
                 value = 20),
    
    numericInput(inputId = "sigma",
                 label = "Innovation standard deviation (\\(\\sigma\\))",
                 value = 1.5),
    
    numericInput(inputId = "b1_val",
                 label = "Trend slope (\\(b_1\\))",
                 value = 0.04),
    
    numericInput(inputId = "b0_val",
                 label = "Trend intercept (\\(b_0\\))",
                 value = 2),
    
    
    numericInput(
      inputId = "seed",
      label = "Random seed",
      value = 0,
      min = 0,
      step = 1
    ),
    
    hr(),
    
    h4("Fitted Model"),
    p("These inputs control the ARMA model fitted to the generated data."),
    
    numericInput(inputId = "fit_p_order",
                 label = "Fitted AR order",
                 value = 1,
                 min = 0),
    
    numericInput(inputId = "fit_q_order",
                 label = "Fitted MA order",
                 value = 2,
                 min = 0),
    
    checkboxInput("show_fit",
                  "Show fitted overlay",
                  value = TRUE),
    
    actionButton("match_fit_order",
                 "Use generated order"),
    
    helpText("Try matching the generated order first, then try a simpler or more complex model."),
    
    hr(),
    
    h4("Display Options"),
    
    selectInput(
      inputId = "series_type",
      label = "Series to display",
      choices = c("y (data)" = "y", "e (errors)" = "e"),
      selected = "y"
    ),
    
    helpText("Choose y to view the simulated data or e to inspect the ARMA error process."),
    
  ),
  
  p("This app simulates a time series with a linear trend and ARMA errors."),
  p("Use the controls on the left to choose the data-generating process, then fit a possibly different ARMA model and inspect the plots below."),
  
  
  uiOutput("equation_panel")
)

to_numeric <- function(x) {
  temp <- x[[1]]
  result <- numeric(length(temp))
  
  if (length(temp) == 0) {
    return(NULL)
  }
  
  for (k in 1:length(temp)) {
    if(is.na(as.numeric(temp[k]))){
      stop(
        paste0(
          "Invalid coefficient input: '", temp[k],
          "'. Please enter numbers separated by commas."
        )
      )
    }
    result[k] <- as.numeric(temp[k])
  }
  
  return(result)
}

get_coefficients <- function(prefix, order, input) {
  if (order == 0) {
    return(numeric(0))
  }
  
  values <- numeric(order)
  
  for (i in 1:order) {
    values[i] <- input[[paste0(prefix, i)]]
  }
  
  return(values)
}

ar_equation <- function(coefs) {
  if (length(coefs) == 0 || is.null(coefs)) return("")
  
  terms <- character(length(coefs))
  
  for (i in 1:length(coefs)) {
    coef <- coefs[i]
    
    if (i == 1) {
      sign <- if (coef < 0) "-" else ""
    } else {
      sign <- if (coef < 0) " - " else " + "
    }
    
    terms[i] <- paste0(sign, abs(coef), "e_{t-", i, "}")
  }
  
  return(paste0(terms, collapse = ""))
}

ma_equation <- function(coefs) {
  if (length(coefs) == 0 || is.null(coefs)) return("")
  
  terms <- character(length(coefs))
  
  for (i in 1:length(coefs)) {
    
    coef <- coefs[i]
    
    sign <- if (coef < 0) " - " else " + "
    
    terms[i] <- paste0(sign, abs(coef), "z_{t-", i, "}")
  }
  
  return(paste0(terms, collapse = ""))
}


server <- function(input, output, session) {
  
  
  observeEvent(input$match_fit_order, {
    updateNumericInput(session,
                       "fit_p_order",
                       value = length(to_numeric(strsplit(input$p_val, ","))))
    
    updateNumericInput(session,
                       "fit_q_order",
                       value = length(to_numeric(strsplit(input$q_val, ","))))
  })
  
  arma_data <- reactive({
    if (trimws(input$p_val) == "") {
      p <- numeric(0)
    } else {
      p <- to_numeric(strsplit(input$p_val, ","))
    }
    
    if (trimws(input$q_val) == "") {
      q <- numeric(0)
    } else {
      q <- to_numeric(strsplit(input$q_val, ","))
    }
    
    
    generate_ARMA_dataset(
      n = input$n_val,
      p = p,
      q = q,
      sigma = input$sigma,
      b = c(input$b1_val, input$b0_val),
      seed = input$seed
    )
  })
  
  
  arma_fit <- reactive({
    d <- arma_data()
    fit_ARMA(
      d,
      p_order = input$fit_p_order,
      q_order = input$fit_q_order
    )
  })
  
  output$model_summary <- renderUI({
    tagList(
      p(strong("Generated process:"), paste0(" ARMA(", length(to_numeric(strsplit(input$p_val, ","))), ", ", length(to_numeric(strsplit(input$q_val, ","))), ")")),
      p(strong("Fitted model:"), paste0(" ARMA(", input$fit_p_order, ", ", input$fit_q_order, ")")),
      p(strong("Series shown:"), if (input$series_type == "y") " observed data (y)" else " error process (e)")
    )
  })
  
  output$main_plot <- renderPlot({
    current_fit <- NULL
    
    if (input$show_fit) {
      current_fit <- arma_fit()
    }
    
    if(input$series_type == "e"){
      plot_ARMA_series(arma_data(), series = "e", fit = current_fit)
    }else{
      plot_ARMA_series(arma_data(), series = "y", fit = current_fit)
    }
    
  })
  
  output$residual_plot <- renderPlot({
    d <- arma_data()
    fit <- arma_fit()
    
    plot(d$t,
         residuals(fit),
         type = "l",
         col = "steelblue",
         lwd = 2,
         xlab = "Time",
         ylab = "Residual",
         main = "Residual Time Plot")
    
    abline(h = 0, lty = 2, col = "gray40")
  })
  
  output$equation_panel <- renderUI(
    fluidRow(
      column(
        width = 7,
        plotOutput("main_plot", height = "420px"),
        plotOutput("residual_plot", height = "260px")
      ),
      column(
        width = 5,
        withMathJax(
          wellPanel(
            h4("Model Equations"),
            p("The app simulates data using a linear trend plus ARMA errors."),
            HTML(paste0("$$y_t = ", input$b1_val, "t +", input$b0_val, "+ e_t$$")),
            HTML(paste0("$$e_t = ", ar_equation(to_numeric(strsplit(input$p_val, ",")))," + z_t", ma_equation(to_numeric(strsplit(input$q_val, ","))),"$$")),
            p("\\(y_t\\): observed time series"),
            p("\\(e_t\\): ARMA error process"),
            p("\\(z_t\\): white-noise innovation"),
            p("\\(b_1\\): trend slope"),
            p("\\(b_0\\): trend intercept"),
            p("\\(\\phi_i\\): autoregressive (AR) coefficients"),
            p("\\(\\theta_i\\): moving-average (MA) coefficients"),
          

          )
        ),
        
        wellPanel(
          h4("Current Setup"),
          uiOutput("model_summary"),
          hr(),
          h4("What to Look For"),
          p("If the fitted model is doing a good job, the residual plot should not show a strong pattern over time."),
          p("If the residuals still show visible structure, the fitted model may be missing part of the time-series behaviour.")
        )
    )
  ))
    
}


  
shinyApp(ui = ui, server = server)
