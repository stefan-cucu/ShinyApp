library(shiny)
shinyApp(
  ui = fluidPage(textInput(inputId = "functionInput", "Function"),actionButton(inputId = "evaluate", "Evaluate"),textOutput(outputId = "medie"),textOutput(outputId = "dispersie")),
  
  
  server = function(input, output,a,b) {
    
    observeEvent(input$evaluate,{
      f = function(x) {eval(parse(text = input$functionInput))}
      me=f(1)+f(100)/2
      dis=sqrt((f(100)-f(1))/12)
      output$medie <- renderText(me)
      output$dispersie <- renderText(dis)
    })
  }
)