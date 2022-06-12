library(shiny)
library(discreteRV)

# See above for the definitions of ui and server
ui <- fluidPage(
  titlePanel("Ex2"),
  
  fluidRow(
    column(4,
           numericInput("nrVal2", "Introduceti numarul valorilor", 1, min = 1),
           uiOutput("table2"),
           actionButton("do2", "Introducere"),
           verbatimTextOutput("proprietati2")),
    
    column(4,
           h3("Functia de masa"),
           plotOutput("fctmasa2")
           ),
    column(4,
           h3("Functia de repartitie"),
           plotOutput("fctrepart2"))
  )
)

server <- function(input, output) {
  
  output$table2 <- renderUI({
    
    vals <- lapply(1:input$nrVal2, function(val){
      div(style="display: inline-block; width: 60px;", 
          numericInput(paste("value", val),"",0, min = 0) )
      })
    
    pr <- lapply(1:input$nrVal2, function(val){
      div(style="display: inline-block; width: 60px;", 
          numericInput(paste("prob", val),"", 0, min = 0) )
    })
    
   test <- lapply(1:input$nrVal2, function(val){
     tags$div(
       div(style="display: inline-block; width: 100px; margin-bottom:0; margin-top:0;"),
       tagList(vals)
     )
   })
   
   print(test)
    
    tags$div(
      div(style="margin:0;"),
      tags$div(
        tagList(test)
      )
    )
  })
  
  output$fctmasa2 <- renderPlot({
    
    if (input$do2 == 0)
      return()
    
    values <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("value", val)]])
    })
    
    probs <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("prob", val)]])
    })
    
    X <- RV(values, probs)
    plot(X, col="pink")
  })
  
  output$fctrepart2 <- renderPlot({
    
    if (input$do2 == 0)
      return()
    
    values <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("value", val)]])
    })
    
    probs <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("prob", val)]])
    })
    
    X <- RV(values, probs)
    domeniu <- seq(min(values)-1, max(values)+1, 0.01)
    codomeniu <- sapply(domeniu, function(nr) {
      return(P(X<=nr))
    })
    plot(domeniu, codomeniu, pch=19, col="pink")
  })
  
  output$proprietati2 <- renderText({
    if (input$do2 == 0)
      return()
    
    values <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("value", val)]])
    })
    
    probs <- sapply(1:input$nrVal2, function(val){
      return(input[[paste("prob", val)]])
    })
    
    X <- RV(values,probs)
    
    EX <- E(X)
    VAR <- V(X)
    paste("Media repartitiei este:", EX,
          "\nVarianta repartitiei este:", VAR)
  })

}

shinyApp(ui, server)