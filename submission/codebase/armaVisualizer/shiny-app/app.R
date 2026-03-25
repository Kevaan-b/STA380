library(shiny)
library(bslib)
source("arma.R")


# Helper function for the info icons

tip_label <- function(label, what, how) {
  tags$div(
    style = "display: flex; align-items: center; gap: 6px;",
    tags$span(
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "right",
      `data-bs-html` = "true",
      `data-bs-custom-class` = "tooltip-left",
      `data-bs-title` = paste0(
        "<b>What:</b> ", what,
        "<br><br>",
        "<b>How to use:</b> ", how
      ),
      style = "display:inline-flex; align-items:center; justify-content:center;
               width:16px; height:16px; border-radius:50%; background:#2C3E50;
               color:white; font-size:10px; cursor:pointer; flex-shrink:0;",
      "i"
    ),
    span(label)
  )
}

#Helper function for calculating the characteristic polynomial
check_stationarity <- function(ar_coefs) {
  if (length(ar_coefs) == 0) return(list(stationary = TRUE, roots = numeric(0)))
  
  roots <- Mod(polyroot(c(1, -ar_coefs)))
  
  stationary <- all(roots > 1)
  return(list(stationary = stationary, roots = roots))
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
    
        new bootstrap.Tooltip(document.body, {
          selector: '[data-bs-toggle=\"tooltip\"]',
          html: true,
          trigger: 'hover focus'
        });
      });
    ")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('renderMathJax', function(message) {
      var el = document.getElementById('mathjax-equations');
      if (el && window.MathJax) {
        el.innerHTML = message.html;
        MathJax.Hub.Queue(['Typeset', MathJax.Hub, el]);
      }
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
        column(3,
               wellPanel(
                 h6("Fitted Model", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
                 numericInput("fit_p_order", tip_label("Fitted AR order", "The number of AR lags in the model you are fitting to the data.","Enter a non-negative integer. Try matching the true order first, then experiment."),value = 0, min = 0),
                 numericInput("fit_q_order", tip_label("Fitted MA order", "The number of MA lags in the model you are fitting to the data.","Enter a non-negative integer. Try matching the true order first, then experiment."),value = 0, min = 0)
               )
        ),
        
        column(3,
               wellPanel(
                 h6("Options", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
                 selectInput("series_type", "Series to display",
                             choices = c("y (data)" = "y", "e (errors)" = "e"), selected = "y"),
                 checkboxInput("show_fit", "Show fitted overlay", value = TRUE),
                 actionButton("match_fit_order", "Use generated order",
                              class = "btn-sm btn-outline-secondary w-100")
               )
        ),
        
        column(3,
               wellPanel(
                 h6("Current Setup", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
                 uiOutput("model_summary")
               )
        ),
        
        column(3, uiOutput("estimated_coef_summary"))
        
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
        h1(strong("by 4kasters: Kevaan Chirag Buch, Kevin Joseph, Eshan Hussain, Will Wang, and Amin Amir"), style = "font-size: 18px; color: #555; margin-bottom: 24px;"),
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
          p("If the fitted model is doing a good job, the residual plot should not show a strong pattern over time. If the residuals still show visible structure, the fitted model may be missing part of the time-series behaviour.")
        ),
        
        wellPanel(
          h4("How to Use"),
          tags$ul(
            tags$li("Set the fitted model order to match the true process. Residuals should look like white noise."),
            tags$li("Overfit by using a larger order than the true process. Watch what changes."),
            tags$li("Underfit by using a smaller order. See if residual structure remains."),
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
    
    if(abs(coef) == 0){
      terms[i] <- paste0("")
    }else{
      sign <- if (coef < 0) " - " else " + "
      
      if(coef == 1 || coef == -1){
        terms[i] <- paste0(sign, "z_{t-", i, "}")
      }else{
        terms[i] <- paste0(sign, abs(coef), "z_{t-", i, "}")
      }
    }
  }
  
  return(paste0(terms, collapse = ""))
}

ma_equation <- function(coefs) {
  if (length(coefs) == 0 || is.null(coefs)) return("")
  
  terms <- character(length(coefs))
  
  for (i in 1:length(coefs)) {
    
    coef <- coefs[i]
    
    if(abs(coef) == 0){
      terms[i] <- paste0("")
    }else{
      sign <- if (coef < 0) " - " else " + "
      
      if(coef == 1 || coef == -1){
        terms[i] <- paste0(sign, "z_{t-", i, "}")
      }else{
        terms[i] <- paste0(sign, abs(coef), "z_{t-", i, "}")
      }
    }
    
  }
  
  return(paste0(terms, collapse = ""))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {


  safe_coefs <- function(val) {
    if (is.null(val) || trimws(val) == "") return(numeric(0))
    tryCatch(
      to_numeric(strsplit(val, ",")),
      error = function(e) numeric(0)  # return empty on invalid input, don't crash
    )
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
    req(input$n_val, input$sigma, input$b1_val, input$b0_val, input$seed)
    generate_ARMA_dataset(
      n     = input$n_val,
      p     = safe_coefs(input$p_val),
      q     = safe_coefs(input$q_val),
      sigma = input$sigma,
      b     = c(input$b1_val, input$b0_val),
      seed  = input$seed
    )
  })
  
  
  
  arma_fit <- reactive({
    req(input$fit_p_order, input$fit_q_order)
    fit_ARMA(arma_data(), p_order = input$fit_p_order, q_order = input$fit_q_order)
  })
  
  
  output$dgp_equations <- renderUI({
    
    p_coefs <- safe_coefs(input$p_val)
    q_coefs <- safe_coefs(input$q_val)
    b1 <- if (is.null(input$b1_val)) "" else input$b1_val
    b0 <- if (is.null(input$b0_val)) "" else input$b0_val
    
    stationarity <- check_stationarity(p_coefs)
    if (length(p_coefs) == 0) {
      stationary_badge <- tags$span(
        style = "background:#17a2b8; color:white; padding:3px 10px; border-radius:12px; font-size:12px;",
        "No AR terms — stationary"
      )
    } else if (stationarity$stationary) {
      root_str <- paste(round(stationarity$roots, 3), collapse = ", ")
      stationary_badge <- tags$span(
        style = "background:#28a745; color:white; padding:3px 10px; border-radius:12px; font-size:12px;",
        paste0("✓ Stationary — roots: ", root_str)
      )
    } else {
      root_str <- paste(round(stationarity$roots, 3), collapse = ", ")
      stationary_badge <- tags$span(
        style = "background:#dc3545; color:white; padding:3px 10px; border-radius:12px; font-size:12px;",
        paste0("✗ Non-stationary — roots: ", root_str)
      )
    }
    
    local({
      html_content <- paste0(
        "<p>\\(y_t = ", b1, "t + ", b0, " + e_t\\)</p>",
        "<p>\\(e_t = ", ar_equation(p_coefs), " + z_t", ma_equation(q_coefs), "\\)</p>"
      )
      session$onFlushed(function() {
        session$sendCustomMessage("renderMathJax", list(html = html_content))
      }, once = TRUE)
    })
    
    wellPanel(
      h6("True Data-Generating Process", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
      tags$div(
        style = "overflow-x: auto; overflow-y: hidden; width: 100%;",
        tags$div(
          id = "mathjax-equations",
          style = "min-width: max-content; white-space: nowrap;",
          HTML(paste0("$$y_t = ", b1, "t + ", b0, " + e_t$$")),
          HTML(paste0("$$e_t = ", ar_equation(p_coefs), " + z_t", ma_equation(q_coefs), "$$"))
        )
      ),
      tags$div(style = "margin-top: 8px;", stationary_badge)
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
    res <- residuals(fit)
    keep <- is.finite(res)
    
    plot(d$t[keep],
         res[keep],
         type = "l",
         col = "steelblue",
         lwd = 2,
         xlab = "Time",
         ylab = "Residual",
         main = "Residual Time Plot")
    
    abline(h = 0, lty = 2, col = "gray40")
  })
  
  output$acf_plot <- renderPlot({
    fit <- arma_fit()
    res <- residuals(fit)
    acf(res[is.finite(res)], main = "ACF of Residuals")
  })

  output$pacf_plot <- renderPlot({
    fit <- arma_fit()
    res <- residuals(fit)
    pacf(res[is.finite(res)], main = "PACF of Residuals")
  })


  output$equation_panel <- renderUI({
      fluidRow(
        column(
          width = 8,
          plotOutput("main_plot", height = "380px"),
          tabsetPanel(
          tabPanel("Residuals",    plotOutput("residual_plot", height = "220px")),
          tabPanel("ACF",          plotOutput("acf_plot",      height = "220px")),
          tabPanel("PACF",         plotOutput("pacf_plot",     height = "220px")),
        )
        ),
        column(
          width = 4,
          uiOutput("dgp_equations"),  
          
          wellPanel(
            h6("Data Generating Process Inputs", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
            textInput("p_val", tip_label("AR coefficients (\u03C6\u1D62)",
                                         "The autoregressive coefficients that control how past values influence the current value.",
                                         "Enter numbers separated by commas, e.g. 0.60,0.30 for an AR(2) process."),
                      value = isolate(input$p_val) %||% "0.60,0.30"),
            textInput("q_val", tip_label("MA coefficients (\u03B8\u1D62)",
                                         "The moving-average coefficients that control how past error influence the current value.",
                                         "Enter numbers separated by commas, e.g. 0.45,0.20 for an MA(2) process."),
                      value = isolate(input$q_val) %||% "0.45,0.20"),
            fluidRow(
              column(6, numericInput("n_val", tip_label("Sample size (n)",
                                                        "The number of time points to simulate.",
                                                        "Enter a positive integer. Larger values give smoother, more stable estimates."),
                                     value = isolate(input$n_val) %||% 100, min = 10, step = 1)),
              column(6, numericInput("sigma", tip_label("Innovation SD (\u03C3)",
                                                        "The spread of the white noise innovations driving the ARMA process.",
                                                        "Enter a positive number. Larger values mean noisier, more volatile series."),
                                     value = isolate(input$sigma) %||% 1.5, min = 0, step = 0.01), )
            ),
            fluidRow(
              column(6, numericInput("b1_val", tip_label("Trend slope (b\u2081)",
                                                         "Controls how steeply the linear trend rises or falls over time.",
                                                         "Enter any number. Use 0 for no trend, positive for upward, negative for downward."),
                                     value = isolate(input$b1_val) %||% 0.2)),
              column(6, numericInput("b0_val", tip_label("Trend intercept (b\u2080)",
                                                         "The starting value of the series at time t = 0.",
                                                         "Enter any number to shift the entire series up or down."),
                                     value = isolate(input$b0_val) %||% 2))
            ),
            numericInput("seed", tip_label("Random seed",
                                           "Sets the random number generator so results are reproducible.",
                                           "Enter any integer. Change it to get a different random draw with the same settings."),
                         value = isolate(input$seed) %||% 0, min = 0, step = 1)
          )
          
        )
      )
    }
  )
  output$model_summary <- renderUI({
    req(input$p_val, input$q_val)
    p_coefs <- safe_coefs(input$p_val)
    q_coefs <- safe_coefs(input$q_val)
    
    tagList(
      p(strong("Generated process:"), paste0(" ARMA(", length(p_coefs), ", ", length(q_coefs), ")")),
      p(strong("Fitted model:"), paste0(" ARMA(", input$fit_p_order, ", ", input$fit_q_order, ")")),
      p(strong("Series shown:"), if (input$series_type == "y") " observed data (y)" else " error process (e)")
    )
  })

  output$estimated_coef_summary <- renderUI({
    fit <- arma_fit()
    coefs <- coef(fit)
    coef_names <- names(coefs)
    coef_fmt <- function(x) sprintf("%.4f", as.numeric(x))
    to_subscript <- function(i) {
      digit_map <- c("0" = "₀", "1" = "₁", "2" = "₂", "3" = "₃", "4" = "₄",
                     "5" = "₅", "6" = "₆", "7" = "₇", "8" = "₈", "9" = "₉")
      digits <- strsplit(as.character(i), "")[[1]]
      paste(digit_map[digits], collapse = "")
    }
    format_lag_terms <- function(values, symbol) {
      if (length(values) == 0) return("none")
      pieces <- character(length(values))
      for (k in seq_along(values)) {
        pieces[k] <- paste0(symbol, to_subscript(k), " = ", coef_fmt(values[k]))
      }
      paste(pieces, collapse = ", ")
    }

    ar_idx <- grepl("^ar", coef_names)
    ma_idx <- grepl("^ma", coef_names)
    trend_idx <- coef_names %in% c("intercept", "t")

    ar_text <- format_lag_terms(coefs[ar_idx], "φ")
    ma_text <- format_lag_terms(coefs[ma_idx], "θ")

    trend_text <- if (any(trend_idx)) {
      trend_terms <- character(0)
      if ("intercept" %in% coef_names) trend_terms <- c(trend_terms, paste0("b₀ = ", coef_fmt(coefs["intercept"])))
      if ("t" %in% coef_names) trend_terms <- c(trend_terms, paste0("b₁ = ", coef_fmt(coefs["t"])))
      paste(trend_terms, collapse = ", ")
    } else {
      "none"
    }

    sd_est <- suppressWarnings(sqrt(as.numeric(fit$sigma2)))
    sd_text <- if (is.finite(sd_est)) {
      paste0("σ̂ = ", coef_fmt(sd_est))
    } else {
      "σ̂ = NA"
    }

    scroll_field <- function(label, value_text) {
      tagList(
        tags$div(style = "font-weight:600; margin-bottom:6px;", label),
        tags$div(
          class = "form-control",
          style = "height:auto; min-height:40px; white-space:nowrap; overflow-x:auto; overflow-y:hidden;",
          value_text
        )
      )
    }

    wellPanel(
      h6("Estimated Coefficients", style = "text-transform: uppercase; font-size: 11px; color: #888; font-weight: 600;"),
      tags$div(style = "display:flex; flex-direction:column; gap:10px;",
               scroll_field("AR coefficients (φᵢ)", ar_text),
               scroll_field("MA coefficients (θᵢ)", ma_text),
               scroll_field("Trend coefficients (b₀, b₁)", trend_text),
               scroll_field("Innovation SD estimate (σ̂)", sd_text))
    )
  })
}


  
shinyApp(ui = ui, server = server)
