library(shiny)
library(discreteRV)

# See above for the definitions of ui and server
ui <- fluidPage(
  titlePanel("Ex3"),
  fluidRow(
    sidebarLayout(sidebarPanel(
      radioButtons("ex_3_tip", "A si B sunt:",
                   c("Independente" = "indep",
                     "Incompatibile" = "incomp",
                     "Nu se stie nimic despre ele" = "idk")),
      actionButton("ex_3_btn", "Calculeaza")
    ), 
    mainPanel(numericInput("ex_3_PA", "P(A)", NULL),
                 numericInput("ex_3_PB", "P(B)", NULL),
                 numericInput("ex_3_PAorB", "P(A∪B)", NULL),
                 numericInput("ex_3_PAandB", "P(A∩B)", NULL),
                 numericInput("ex_3_PArB", "P(A\\B)", NULL),
                 numericInput("ex_3_PBrA", "P(B\\A)", NULL)
    )
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)