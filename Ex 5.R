library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(sliderInput(inputId = "begin",
                                             label = "Where to display from:",
                                             min = 0,
                                             max = 99,
                                             value = 0),DTOutput('tbl')),
  server = function(input, output) {
    X <- RV(0:100, 1/3)
    Outcomes = outcomes(X)
    Probabilities = probs(X)
    df <-data.frame(Outcomes,Probabilities)
    observeEvent(input$begin,{df <- tail(df,100-input$begin)
    output$tbl = renderDT(
      df, options = list(pageLength=10)
    )
    
    
    })
    
  }
)