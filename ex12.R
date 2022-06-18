library(shiny)
library(discreteRV)

# See above for the definitions of ui and server
ui <- fluidPage(
  titlePanel("Ex12"),
  tags$head(
    tags$style(HTML("
        table {
          font-family: arial, sans-serif;
          border-collapse: collapse;
          width: 100%;
          margin: 0px 0px 30px 0px;
        }
        
        td, th {
          border: 1px solid #dddddd;
          text-align: left;
          padding: 8px;
          text-align:center;
        }
        
        "))),
  
  fluidRow(
    column(6,
           numericInput("ex_12_nrValX", "Introduceti numarul valorilor lui X", 1, min = 1),
           uiOutput("ex_12_tabelX")),
    
    column(6,numericInput("ex_12_nrValY", "Introduceti numarul valorilor lui Y", 1, min = 1),
           uiOutput("ex_12_tabelY"))
  ),
  fluidRow(
    column(12,
           radioButtons("ex_12_tip", "Operatia dintre X si Y:",
                        c("Suma" = "suma",
                          "Diferenta" = "dif",
                          "Produs" = "prod",
                          "Raport" = "rap")),
           actionButton("ex_12_btn", "Calculeaza"),
           uiOutput("ex_12_rezultat")
           )
  )
)

server <- function(input, output) {
  output$ex_12_tabelX <- renderUI({
    values <- lapply(1:input$ex_12_nrValX, function(val){
      div(style="display: inline-block; width: 70px;", 
          numericInput(paste("ex_9_Xvalue", val, sep=""),"",0, min = 0) )
    })
    
    probs <- lapply(1:input$ex_12_nrValX, function(val){
      div(style="display: inline-block; width: 70px;", 
          numericInput(paste("ex_9_Xprob", val, sep=""),"",0, min = 0) )
    })
    
    tags$div(
      tags$div(
        div(style="display: inline-block; width: 100px;","Valori:"),
        tagList(values)
        ),
      tags$div(
        div(style="display: inline-block; width: 100px;","Probabilitati:"),
        tagList(probs)))
  })
  
  output$ex_12_tabelY <- renderUI({
    values <- lapply(1:input$ex_12_nrValY, function(val){
      div(style="display: inline-block; width: 70px;", 
          numericInput(paste("ex_9_Yvalue", val, sep=""),"",0, min = 0) )
    })
    
    probs <- lapply(1:input$ex_12_nrValY, function(val){
      div(style="display: inline-block; width: 70px;", 
          numericInput(paste("ex_9_Yprob", val, sep=""),"",0, min = 0) )
    })
    
    tags$div(
      tags$div(
        div(style="display: inline-block; width: 100px;","Valori:"),
        tagList(values)
      ),
      tags$div(
        div(style="display: inline-block; width: 100px;","Probabilitati:"),
        tagList(probs)))
  })
  
  observeEvent(input$ex_12_btn, {
    
    xvalues <- sapply(1:input$ex_12_nrValX, function(ind) {
      input[[paste("ex_9_Xvalue", ind, sep="")]]
    })
    
    xprobs <- sapply(1:input$ex_12_nrValX, function(ind) {
      input[[paste("ex_9_Xprob", ind, sep="")]]
    })
    
    yvalues <- sapply(1:input$ex_12_nrValY, function(ind) {
      input[[paste("ex_9_Yvalue", ind, sep="")]]
    })
    
    yprobs <- sapply(1:input$ex_12_nrValY, function(ind) {
      input[[paste("ex_9_Yprob", ind, sep="")]]
    })
    
    
    text <- NULL
    allOutcomes <- NULL
    
    if(input$ex_12_tip == "suma") {
      text <- "X + Y"
      allOutcomes <- as.vector(sapply(xvalues, function(val){ val + yvalues}))
      
    } else if(input$ex_12_tip == "dif") {
      text <- "X - Y"
      allOutcomes <- as.vector(sapply(xvalues, function(val){ val - yvalues }))
      
    } else if(input$ex_12_tip == "prod") {
      text <- "X * Y"
      allOutcomes <- as.vector(sapply(xvalues, function(val){ val * yvalues }))
      
    } else if(input$ex_12_tip == "rap") {
      text <- "X / Y"
      allOutcomes <- as.vector(sapply(xvalues, function(val){ val / yvalues}))
    }
    
    allProbs <- as.vector(sapply(xprobs, function(pr){ pr * yprobs }))
    
    outcomes <- sort(unique(allOutcomes))
    probs <- sapply(outcomes, function(out){
      poz <- which(out == allOutcomes)
      sum(allProbs[poz])
    })
    
    firstRow <- lapply(outcomes, function(out) {tags$th(out)})
    secondRow <- lapply(probs, function(prob) {tags$th(prob)})
    
    output$ex_12_rezultat <- renderUI({
      
      tags$div(
        tags$table(
          tags$tr(
            tags$th(text),
            firstRow
          ),
          tags$tr(
            tags$td(""),
            secondRow
          )
        )
      )
    })
  })
}

shinyApp(ui, server)