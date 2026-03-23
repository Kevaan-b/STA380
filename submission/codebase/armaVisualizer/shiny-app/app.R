library(shiny)
library(bslib)
source("arma.R")

# Helper function for the info icons

tip_label <- function(label, what, how) {
  tagList(
    span(
      HTML(paste0(label, " ")),
      tags$span(
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "right",
        `data-bs-html` = "true",
        `data-bs-custom-class` = "tooltip-left",
        title = paste0(
          "<b></b> ", what,
          "<br><br>",
          "<b>How to use:</b> ", how
        ),
        style = "display:inline-flex; align-items:center; justify-content:center;
                 width:16px; height:16px; border-radius:50%; background:#2C3E50;
                 color:white; font-size:10px; cursor:pointer; margin-left:4px;",
        "i"
      )
    )
  )
}


ui <- page_navbar(
  
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#2C3E50"),
  
  header = tagList(
    withMathJax(),
    tags$head(tags$style(HTML(".navbar-nav { margin-left: auto !important; }.tooltip-left .tooltip-inner { text-align: left !important; }"))),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        var tooltips = document.querySelectorAll('[data-bs-toggle=\"tooltip\"]');
        tooltips.forEach(function(el) { new bootstrap.Tooltip(el); });
      });
    "))
  ),
    

  title = "Time Series Model Behaviour Explorer",

  

  # Main Explorer Page
  nav_panel(
    title = "Explorer",
    layout_sidebar(
      
      sidebar = sidebar(
        
        
        h4("Data Generating Process"),
        p("These inputs control the time series that gets simulated."),
        
        textInput(inputId = "p_val",
                  label = tip_label(
                    "AR coffecients (\\(\\phi_i\\))",
                    "The autoregressive coefficients that control how past values influence the current value.",
                    "Enter numbers separated by commas, e.g. 0.60,-0.30 for an AR(2) process."
                  ),
                  value = "0.60,-0.30"),
        
        textInput(inputId = "q_val",
                  label = tip_label(
                    "MA coefficients (\\(\\theta_i\\))",
                    "The moving-average coefficients that control how past error influence the current value.",
                    "Enter numbers separated by commas, e.g. 0.45,-0.20 for an MA(2) process."
                  ),
                  value = "0.45,-0.20"),
        
        
        numericInput(inputId = "n_val",
                     label = tip_label(
                       "Sample size (n)",
                       "The number of time points to simulate.",
                       "Enter a positive integer. Larger values give smoother, more stable estimates."
                     ),
                     value = 20),
        
        numericInput(inputId = "sigma",
                     label = tip_label(
                       "Innovation standard deviation (\\(\\sigma\\))",
                       "The spread of the white noise innovations driving the ARMA process.",
                       "Enter a positive number. Larger values mean noisier, more volatile series."
                     ),
                     value = 1.5),
        
        numericInput(inputId = "b1_val",
                     label = tip_label(
                       "Trend slope (\\(b_1\\))",
                       "Controls how steeply the linear trend rises or falls over time.",
                       "Enter any number. Use 0 for no trend, positive for upward, negative for downward."
                     ),
                     value = 0.04),
        
        numericInput(inputId = "b0_val",
                     label = tip_label(
                       "Trend intercept (\\(b_0\\))",
                       "The starting value of the series at time t = 0.",
                       "Enter any number to shift the entire series up or down."
                     ),
                     value = 2),
        
        
        numericInput(
          inputId = "seed",
          label = tip_label(
            "Random seed",
            "Sets the random number generator so results are reproducible.",
            "Enter any integer. Change it to get a different random draw with the same settings."
          ),
          value = 0,
          min = 0,
          step = 1
        ),
        
        hr(),
        
        h4("Fitted Model"),
        p("These inputs control the ARMA model fitted to the generated data."),
        
        numericInput(inputId = "fit_p_order",
                     label = tip_label(
                       "Fitted AR order",
                       "The number of AR lags in the model you are fitting to the data.",
                       "Enter a non-negative integer. Try matching the true order first, then experiment."
                     ),
                     value = 1,
                     min = 0),
        
        numericInput(inputId = "fit_q_order",
                     label = tip_label(
                       "Fitted MA order",
                       "The number of MA lags in the model you are fitting to the data.",
                       "Enter a non-negative integer. Try matching the true order first, then experiment."
                     ),
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

  ),
  
  # About page
  nav_panel(
    title = "About",
    
    fluidRow(
      column(
        width = 8, offset = 2,
        
        br(),
        h2("About This App"),
        hr(),
        
      )
      
    )
    
  )
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
