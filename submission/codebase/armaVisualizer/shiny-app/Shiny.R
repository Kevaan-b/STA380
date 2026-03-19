library(shiny)
library(bslib)
source("arma.R")

ui <- page_sidebar(
  
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#2C3E50"),

  title = "Time Series Model Behaviour Explorer",

  
  sidebar = sidebar(
  
    h4("Model Inputs"),
    
    textInput(inputId = "p_val",
                 label = "AR coefficients (p)",
                 value = "0.4"),
    
    textInput(inputId = "q_val",
                 label = "MA coefficients (q)",
                 value = "0.25, -0.1"),
    
    numericInput(inputId = "n_val",
                 label = "Sample size",
                 value = 300),
    
    numericInput(inputId = "sigma",
                 label = "Innovation standard deviation",
                 value = 1.5),
    
    textInput(inputId = "b_val",
              label = "Trend coefficients (b1, b0)",
              value = "0.04, 2"),
    
    
    sliderInput(
      inputId = "seed",
      label = "Random seed",
      min = 0,
      max = 1000,
      value = 0
    ),
    
    selectInput(
      inputId = "series_type",
      label = "Choose series:",
      choices = c("y (data)" = "y", "e (errors)" = "e"),
      selected = "y"
    ),
    
    conditionalPanel(
      condition = "input.series_type == 'e'",
      checkboxInput("show_fit", "Show fit", value = TRUE)
    ),
    
    helpText("Tip: choose y to view the generated data, or e to inspect the ARMA error process."),
    
  ),
  
  plotOutput("main_plot")
)

to_numeric <- function(x) {
  return(as.numeric(trimws(x[[1]])))
}

server <- function(input, output, session) {
  
  arma_data <- reactive({
    generate_ARMA_dataset(
      n = input$n_val,
      p = to_numeric(strsplit(input$p_val, ",")),
      q = to_numeric(strsplit(input$q_val, ",")),
      sigma = input$sigma,
      b = to_numeric(strsplit(input$b_val, ",")),
      seed = input$seed
    )
  })
  
  
  arma_fit <- reactive({
    d <- arma_data()
    fit_ARMA(
      d,
      p_order = length(to_numeric(strsplit(input$p_val, ","))),
      q_order = length(to_numeric(strsplit(input$q_val, ",")))
    )
  })
  
  output$main_plot <- renderPlot({
    if(input$series_type == "e"){
      if(input$show_fit){
        plot_ARMA_series(arma_data(), series = "e", fit = arma_fit())
      }else{
        plot_ARMA_series(arma_data(), series = "e", fit = NULL)
      }
    }else{
      plot_ARMA_series(arma_data(), series = "y")
    }
    
  })

    
}
  
shinyApp(ui = ui, server = server)
