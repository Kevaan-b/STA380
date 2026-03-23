library(shiny)
library(bslib)
source("arma.R")

# Helper function for the info icons

tip_label <- function(label, what, how) {
  tagList(
    tags$div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$span(
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "right",
        `data-bs-html` = "true",
        `data-bs-custom-class` = "tooltip-left",
        title = paste0(
          "<b>What:</b> ", what,
          "<br><br>",
          "<b>How to use:</b> ", how
        ),
        style = "display:inline-flex; align-items:center; justify-content:center;
                 width:16px; height:16px; border-radius:50%; background:#2C3E50;
                 color:white; font-size:10px; cursor:pointer; flex-shrink:0;",
        "i"
      ),
      span(HTML(label))
    )
  )
}


ui <- page_navbar(
  
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#2C3E50"),
  
  header = tagList(
    withMathJax(),
    tags$head(tags$style(HTML(".tooltip-left .tooltip-inner { text-align: left !important; }.navbar-brand { color: #00000 !important; font-weight: 700 !important; font-size: 22px !important; }"))),    
    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {

      var collapse = document.querySelector('.navbar-collapse');
      if (collapse) {
        collapse.style.marginLeft = 'auto';
        collapse.style.flexGrow = '0';
      }

      var tooltips = document.querySelectorAll('[data-bs-toggle=\"tooltip\"]');
      tooltips.forEach(function(el) { new bootstrap.Tooltip(el); });
    });
  "))
  ),
    

  title = "Time Series Model Behaviour Explorer",

  

  # Main Explorer Page
  nav_panel(
    title = "Explorer",
    
    # Main content area
    uiOutput("equation_panel"),
    
    # Bottom control bar
    tags$div(
      style = "border-top: 1px solid #dee2e6; background: #f8f9fa; padding: 16px 24px;",
      fluidRow(
        column(2,
               h6("Fitted Model", style = "font-weight:600; margin-bottom:10px;"),
               numericInput("fit_p_order", tip_label("Fitted AR order", "The number of AR lags in the model you are fitting to the data.","Enter a non-negative integer. Try matching the true order first, then experiment."),value = 1, min = 0),
               numericInput("fit_q_order", tip_label("Fitted MA order", "The number of MA lags in the model you are fitting to the data.","Enter a non-negative integer. Try matching the true order first, then experiment."),value = 2, min = 0)
        ),
        
        column(2,
               h6("Options", style = "font-weight:600; margin-bottom:10px;"),
               selectInput("series_type", "Series to display",
                           choices = c("y (data)" = "y", "e (errors)" = "e"), selected = "y"),
               checkboxInput("show_fit", "Show fitted overlay", value = TRUE),
               actionButton("match_fit_order", "Use generated order",
                            class = "btn-sm btn-outline-secondary w-100"),
        )
      )
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
        
        wellPanel(
          h4("Purpose"),
          p("This app simulates a time series with a linear trend and ARMA errors.
          To get started, go to the \"Explorer\" tab and use the controls on the left to choose the data-generating process,
          fit a possibly different ARMA model, and inspect how well it captures
          the underlying structure through the plots and residuals.")
        ),
        
        wellPanel(
          h4("What to Look For"),
          p("If the fitted model is doing a good job, the residual plot should not show a strong pattern over time."),
          p("If the residuals still show visible structure, the fitted model may be missing part of the time-series behaviour.")
        ),
        
        wellPanel(
          h4("How to Use"),
          tags$ul(
            tags$li("Set the fitted model order to match the true process — residuals should look like white noise."),
            tags$li("Overfit by using a larger order than the true process — watch what changes."),
            tags$li("Underfit by using a smaller order — see if residual structure remains."),
            tags$li("Increase sample size to see how estimation improves with more data.")
          )
        ),
        
        wellPanel(
          h4("Key Terms"),
          tags$ul(
            tags$li(strong("AR (AutoRegressive):"), " current value depends on its own past values."),
            tags$li(strong("MA (Moving Average):"), " current value depends on past error terms."),
            tags$li(strong("ARMA(p, q):"), " p AR lags and q MA lags combined."),
            tags$li(strong("Residuals:"), " what's left after the model is subtracted from the data. A good fit leaves no visible pattern.")
          )
        ),
        
        br()
      )
    )
  ),
  
  nav_panel(
    title = "How to use",
    fluidRow(
      column(
        width = 8, offset = 2,
        
        br(),
        h2("In development"),
        hr(),
        
        wellPanel(
          h4("Purpose"),
          p("This app simulates a time series with a linear trend and ARMA errors.
          To get started, go to the \"Explorer\" tab and use the controls on the left to choose the data-generating process,
          fit a possibly different ARMA model, and inspect how well it captures
          the underlying structure through the plots and residuals.")
        ),
        
        wellPanel(
          h4("What to Look For"),
          p("If the fitted model is doing a good job, the residual plot should not show a strong pattern over time."),
          p("If the residuals still show visible structure, the fitted model may be missing part of the time-series behaviour.")
        ),
        
        wellPanel(
          h4("How to Use"),
          tags$ul(
            tags$li("Set the fitted model order to match the true process — residuals should look like white noise."),
            tags$li("Overfit by using a larger order than the true process — watch what changes."),
            tags$li("Underfit by using a smaller order — see if residual structure remains."),
            tags$li("Increase sample size to see how estimation improves with more data.")
          )
        ),
        
        wellPanel(
          h4("Key Terms"),
          tags$ul(
            tags$li(strong("AR (AutoRegressive):"), " current value depends on its own past values."),
            tags$li(strong("MA (Moving Average):"), " current value depends on past error terms."),
            tags$li(strong("ARMA(p, q):"), " p AR lags and q MA lags combined."),
            tags$li(strong("Residuals:"), " what's left after the model is subtracted from the data. A good fit leaves no visible pattern.")
          )
        ),
        
        br()
      )
    )
  )
)
  

to_numeric <- function(x) {
  temp <- if (is.list(x)) x[[1]] else x
  result <- numeric(length(temp))
  
  if (length(temp) == 0 || identical(temp, "")) return(numeric(0))
  
  result <- numeric(length(temp))
  for (k in seq_along(temp)) {
    val <- suppressWarnings(as.numeric(temp[k]))
    if (is.na(val)) {
      stop(
        paste0(
          "Invalid coefficient input: '", temp[k],
          "'. Please enter numbers separated by commas."
        )
      )
    }
    result[k] <- val
  }
  
  return(result)
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
  
  safe_coefs <- function(val) {
    if (is.null(val) || trimws(val) == "") return(numeric(0))
    to_numeric(strsplit(val, ","))
  }
  
  # Empty-check guard
  observeEvent(input$match_fit_order, {
    req(input$p_val, input$q_val)
    p_len <- length(safe_coefs(input$p_val))
    q_len <- length(safe_coefs(input$q_val))
    
    updateNumericInput(session, "fit_p_order", value = p_len)
    updateNumericInput(session, "fit_q_order", value = q_len)
  })
  
  arma_data <- reactive({
    req(input$p_val, input$q_val, input$n_val, input$sigma, input$b1_val, input$b0_val, input$seed)
    generate_ARMA_dataset(
      n    = input$n_val,
      p    = safe_coefs(input$p_val),
      q    = safe_coefs(input$q_val),
      sigma = input$sigma,
      b    = c(input$b1_val, input$b0_val),
      seed = input$seed
    )
  })
  
  
  arma_fit <- reactive({
    req(input$fit_p_order, input$fit_q_order)
    fit_ARMA(arma_data(), p_order = input$fit_p_order, q_order = input$fit_q_order)
  })
  
  output$model_summary <- renderUI({
    req(input$p_val, input$q_val)
    p_coefs <- safe_coefs(input$p_val)
    q_coefs <- safe_coefs(input$q_val)
    
    tagList(
      p(strong("Generated process:"), paste0(" ARMA(", length(to_numeric(strsplit(input$p_val, ","))), ", ", length(to_numeric(strsplit(input$q_val, ","))), ")")),
      p(strong("Fitted model:"), paste0(" ARMA(", input$fit_p_order, ", ", input$fit_q_order, ")")),
      p(strong("Series shown:"), if (input$series_type == "y") " observed data (y)" else " error process (e)")
    )
  })
  
  output$main_plot <- renderPlot({
    current_fit <- if (input$show_fit) arma_fit() else NULL
    
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
  
  output$equation_panel <- renderUI({
    p_coefs <- safe_coefs(input$p_val)
    q_coefs <- safe_coefs(input$q_val)
    b1 <- if (is.null(input$b1_val)) "" else input$b1_val
    b0 <- if (is.null(input$b0_val)) "" else input$b0_val
    
      fluidRow(
        column(
          width = 8,
          plotOutput("main_plot", height = "380px"),
          plotOutput("residual_plot", height = "240px")
        ),
        column(
          width = 4,
          style = "overflow-y: auto; max-height: 620px;",
          
          wellPanel(
            h6("True Data-Generating Process", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
            HTML(paste0("$$y_t = ", input$b1_val, "t +", input$b0_val, "+ e_t$$")),
            HTML(paste0("$$e_t = ", ar_equation(p_coefs), " + z_t", ma_equation(q_coefs), "$$"))          
            ),
        
          wellPanel(
            h6("Data Generating Process Inputs", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
            textInput("p_val", tip_label("AR coefficients (\u03C6\u1D62)",
                                         "The autoregressive coefficients that control how past values influence the current value.",
                                         "Enter numbers separated by commas, e.g. 0.60,-0.30 for an AR(2) process."),
                      value = "0.60,-0.30"),
            textInput("q_val", tip_label("MA coefficients (\u03B8\u1D62)",
                                         "The moving-average coefficients that control how past error influence the current value.",
                                         "Enter numbers separated by commas, e.g. 0.45,-0.20 for an MA(2) process."),
                      value = "0.45,-0.20"),
            fluidRow(
              column(6, numericInput("n_val", tip_label("Sample size (n)",
                                                        "The number of time points to simulate.",
                                                        "Enter a positive integer. Larger values give smoother, more stable estimates."),
                                     value = 20)),
              column(6, numericInput("sigma", tip_label("Innovation SD (\u03C3)",
                                                        "The spread of the white noise innovations driving the ARMA process.",
                                                        "Enter a positive number. Larger values mean noisier, more volatile series."),
                                     value = 1.5))
            ),
            fluidRow(
              column(6, numericInput("b1_val", tip_label("Trend slope (b\u2081)",
                                                         "Controls how steeply the linear trend rises or falls over time.",
                                                         "Enter any number. Use 0 for no trend, positive for upward, negative for downward."),
                                     value = 0.04)),
              column(6, numericInput("b0_val", tip_label("Trend intercept (b\u2080)",
                                                         "The starting value of the series at time t = 0.",
                                                         "Enter any number to shift the entire series up or down."),
                                     value = 2))
            ),
            numericInput("seed", tip_label("Random seed",
                                           "Sets the random number generator so results are reproducible.",
                                           "Enter any integer. Change it to get a different random draw with the same settings."),
                         value = 0, min = 0, step = 1)
          ),
          wellPanel(
            h6("Current Setup", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
            uiOutput("model_summary")
          )
        )
      )
    }
  )
}


  
shinyApp(ui = ui, server = server)
