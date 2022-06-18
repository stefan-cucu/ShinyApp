library(shiny)

ui <- fluidPage(
  titlePanel("Ex11"),
  
  fluidRow(
    column(6,fileInput("ex_11_fisier",
                       label="Adaugati fisierului:",
                       multiple = FALSE,accept = ".csv"),
              actionButton("ex_11_btn_fisier", "Calculeaza folosind fisierul")),
    column(6,numericInput("ex_11_nrVal","Introduceti numarul valorilor:", 1, min = 1),
              uiOutput("ex_11_table"),
              actionButton("ex_11_btn_val", "Calculeaza folosind datele"))
  ),
  
  fluidRow(
    column(12, textOutput("ex_11_med"),
           textOutput("ex_11_q1"),
           textOutput("ex_11_q2"),
           textOutput("ex_11_q3")
           )
  ),
  
  fluidRow(
    column(6, plotOutput("ex_11_hist")),
    column(6, plotOutput("ex_11_box"))
  )
)

server <- function(input, output) {
  output$ex_11_table <- renderUI({
    values <- lapply(1:input$ex_11_nrVal, function(val){
      div(style="display: inline-block; width: 70px;", 
          numericInput(paste("ex_11_val", val, sep=""),"",NULL) )
    })
    
    tags$div(
      div(style="display: inline-block; width: 100px;","Valori:"),
      tagList(values)
    )
  })
  
  observeEvent(input$ex_11_btn_val, {
    values <- sapply(1:input$ex_11_nrVal, function(ind) { input[[paste("ex_11_val", ind, sep="")]]})
    
    mediana <- median(values)
    q1 <- quantile(values, 1/4)
    q2 <- quantile(values, 2/4)
    q3 <- quantile(values, 3/4)
    
    output$ex_11_med <- renderText({paste("Mediana este:", mediana)})
    output$ex_11_q1 <- renderText({paste("Prima quartila este:", q1)})
    output$ex_11_q2 <- renderText({paste("A doua quartila este:", q2)})
    output$ex_11_q3 <- renderText({paste("A treia quartila este:", q3)})
    
    output$ex_11_hist <- renderPlot({hist(values)})
    output$ex_11_box <- renderPlot({boxplot(values)})
  })
  
  observeEvent(input$ex_11_btn_fisier, {
    inFile <- input$ex_11_fisier
    dataFile <- read_excel(inFile$datapath,sheet=1)
    values <- dataFile[[1]]
    
    mediana <- median(values)
    q1 <- quantile(values, 1/4)
    q2 <- quantile(values, 2/4)
    q3 <- quantile(values, 3/4)
    
    output$ex_11_med <- renderText({paste("Mediana este:", mediana)})
    output$ex_11_q1 <- renderText({paste("Prima quartila este:", q1)})
    output$ex_11_q2 <- renderText({paste("A doua quartila este:", q2)})
    output$ex_11_q3 <- renderText({paste("A treia quartila este:", q3)})
    
    output$ex_11_hist <- renderPlot({hist(values)})
    output$ex_11_box <- renderPlot({boxplot(values)})
  })
}

shinyApp(ui, server)