library(shiny)
library(discreteRV)
library(DT)
shinyApp(
  ui = fluidPage(textInput(inputId = "functionInput", "Function"),actionButton(inputId = "evaluate", "Evaluate"),sliderInput(inputId = "begin",
                                                                                                                                     label = "Where to display from:",
                                                                                                                                     min = 0,
                                                                                                                                     max = 99,
                                                                                                                                    value = 0),DTOutput('tbl')),
  
  
  server = function(input, output) {
    X <- RV(0:100, 1/3)
    Outcomes = outcomes(X)
    Probabilities = probs(X)
    df <-data.frame(Outcomes,Probabilities)
    observeEvent(input$begin,
                 {
                   output$tbl = renderDT(
                     df, options = list(pageLength=10)
                   )
                   df <- tail(df,101-input$begin)
                   observeEvent(input$evaluate,{
                     f = function(x) {eval(parse(text = input$functionInput))}
                     Y=f(X)
                     Outcomes = outcomes(Y)
                     Probabilities = probs(Y)
                     df <-data.frame(Outcomes,Probabilities)
                     output$tbl = renderDT(
                       tail(df,101-input$begin), options = list(pageLength=10)
                     )
                   })
                 })
  }
)