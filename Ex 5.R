library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DTOutput('tbl'),sliderInput(inputId = "bins",
                                             label = "Number of bins:",
                                             min = 0,
                                             max = 99,
                                             value = 0)),
  server = function(input, output) {
    X <- RV(0:100, 1/3)
    Outcomes = outcomes(X)
    Probabilities =probs(X)
    df <-data.frame(Outcomes,Probabilities)
    observeEvent(input$bins,{df <- tail(df,100-input$bins)
    output$tbl = renderDT(
      df, options = list(pageLength=10)
    )
    
    
    })
    
  }
)