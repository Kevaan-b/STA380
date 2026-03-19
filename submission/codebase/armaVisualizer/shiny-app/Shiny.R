library(shiny)
library(bslib)
source("arma.R")

ui <- page_sidebar(
  
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#2C3E50"),

  title = "Time Series Model Behaviour Explorer",

  
  sidebar = sidebar(
    width = 330,
  
    h4("Data Generating Process"),
    p("These inputs control the time series that gets simulated."),
    
    numericInput(inputId = "dgp_p_order",
                 label = "Generated AR order",
                 value = 1,
                 min = 0,
                 max = 5),
    
    uiOutput("p_inputs"),
    
    numericInput(inputId = "dgp_q_order",
                 label = "Generated MA order",
                 value = 2,
                 min = 0,
                 max = 5),
    
    uiOutput("q_inputs"),
    
    numericInput(inputId = "n_val",
                 label = "Sample size",
                 value = 300),
    
    numericInput(inputId = "sigma",
                 label = "Innovation standard deviation",
                 value = 1.5),
    
    numericInput(inputId = "b1_val",
                 label = "Trend slope (b1)",
                 value = 0.04),
    
    numericInput(inputId = "b0_val",
                 label = "Trend intercept (b0)",
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
  
  fluidRow(
    column(
      width = 8,
      plotOutput("main_plot", height = "420px"),
      plotOutput("residual_plot", height = "260px")
    ),
    column(
      width = 4,
      withMathJax(
        wellPanel(
          h4("Model Equations"),
          p("The app simulates data using a linear trend plus ARMA errors."),
          HTML("$$y_t = b_1 t + b_0 + e_t$$"),
          HTML("$$e_t = \\phi_1 e_{t-1} + \\cdots + \\phi_p e_{t-p} + z_t + \\theta_1 z_{t-1} + \\cdots + \\theta_q z_{t-q}$$"),
          p("\\(y_t\\) is the observed series, \\(e_t\\) is the error process, and \\(z_t\\) is white noise.")
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
  )
)

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

server <- function(input, output, session) {
  
  output$p_inputs <- renderUI({
    if (input$dgp_p_order == 0) {
      return(helpText("No AR coefficients needed."))
    }
    
    p_inputs <- list()
    
    for (i in 1:input$dgp_p_order) {
      default_value <- 0
      
      if (i == 1) {
        default_value <- 0.4
      }
      
      p_inputs[[i]] <- numericInput(
        inputId = paste0("p_", i),
        label = paste("AR coefficient", i),
        value = default_value,
        step = 0.1
      )
    }
    
    return(tagList(p_inputs))
  })
  
  output$q_inputs <- renderUI({
    if (input$dgp_q_order == 0) {
      return(helpText("No MA coefficients needed."))
    }
    
    q_inputs <- list()
    
    for (i in 1:input$dgp_q_order) {
      default_value <- 0
      
      if (i == 1) {
        default_value <- 0.25
      }
      
      if (i == 2) {
        default_value <- -0.1
      }
      
      q_inputs[[i]] <- numericInput(
        inputId = paste0("q_", i),
        label = paste("MA coefficient", i),
        value = default_value,
        step = 0.1
      )
    }
    
    return(tagList(q_inputs))
  })
  
  observeEvent(input$match_fit_order, {
    updateNumericInput(session,
                       "fit_p_order",
                       value = input$dgp_p_order)
    
    updateNumericInput(session,
                       "fit_q_order",
                       value = input$dgp_q_order)
  })
  
  arma_data <- reactive({
    p_values <- get_coefficients("p_", input$dgp_p_order, input)
    q_values <- get_coefficients("q_", input$dgp_q_order, input)
    
    generate_ARMA_dataset(
      n = input$n_val,
      p = p_values,
      q = q_values,
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
      p(strong("Generated process:"), paste0(" ARMA(", input$dgp_p_order, ", ", input$dgp_q_order, ")")),
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

    
}
  
shinyApp(ui = ui, server = server)
