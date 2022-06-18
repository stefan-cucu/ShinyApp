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

    mainPanel( textOutput("ex_3_eroare"),
                 numericInput("ex_3_PA", "P(A)", NULL),
                 numericInput("ex_3_PB", "P(B)", NULL),
                 numericInput("ex_3_PAorB", "P(A∪B)", NULL),
                 numericInput("ex_3_PAandB", "P(A∩B)", NULL),
                 numericInput("ex_3_PArB", "P(A\\B)", NULL),
                 numericInput("ex_3_PBrA", "P(B\\A)", NULL)
    )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$ex_3_btn, {
    pa <- input$ex_3_PA
    pb <- input$ex_3_PB
    reun <- input$ex_3_PAorB
    inter <- input$ex_3_PAandB
    AcondB <- input$ex_3_PArB
    BcondA <- input$ex_3_PBrA
    ok <- TRUE
    
    if(input$ex_3_tip == "incomp") {
        inter <- 0
        AcondB <- 0
        BcondA <- 0
        
        if(!is.na(pa) && !is.na(pb)) {
            reun <- pa + pb
          } else if(!is.na(pa) && !is.na(reun)){
            pb <- reun - pa
          } else if(!is.na(pb) && !is.na(reun)){
            pa <- reun - pb
          } else {ok <- FALSE}
      
      } else if(input$ex_3_tip == "indep") {
          if(!is.na(pa)) { AcondB <- pa } else if(!is.na(AcondB)) { pa <- AcondB }
          if(!is.na(pb)) { BcondA <- pb } else if(!is.na(BcondA)) { pb <- BcondA }

          if(!is.na(pa) && !is.na(pb)) {
            inter <- pa * pb
            reun <- pa + pb - inter
          } else if(!is.na(pa) && !is.na(inter)) {
            pb <- inter / pa
            BcondA <- pb
            reun <- pa + pb - inter
          } else if(!is.na(pb) && !is.na(inter)) {
            pa <- inter / pb
            AcondB <- pa
            reun <- pa + pb - inter
          } else { ok <- FALSE}
        
      } else {
        if(!is.na(pa) && !is.na(pb) && !is.na(reun)) {
          inter <- reun - pa - pb
          AcondB <- inter / pb
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(pb) && !is.na(inter)) {
          reun <- pa + pb - inter
          AcondB <- inter / pb
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(pb) && !is.na(AcondB)) {
          inter <- AcondB * pa
          reun <- pa + pb - inter
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(pb) && !is.na(BcondA)) {
          inter <- BcondA * pb
          reun <- pa + pb - inter
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(reun) && !is.na(inter)) {
          pb <- reun - pa + inter
          AcondB <- inter / pb
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(reun) && !is.na(AcondB)) {
          pb <- (reun - pa) / (1-AcondB)
          inter <- pa + pb - reun
          BcondA <- inter / pa
          
        } else if(!is.na(pa) && !is.na(reun) && !is.na(BcondA)) {
          inter <- BcondA * pa
          pb <- reun - pa + inter
          AcondB <- inter / pb
          
        } else if(!is.na(pa) && !is.na(inter) && !is.na(AcondB)) {
          pb <- inter / AcondB 
          reun <- pa + pb - inter
          BcondA <- inter / pb
          
        } else if(!is.na(pa) && !is.na(AcondB) && !is.na(BcondA)) {
          inter <-  pa * BcondA
          pb <- inter / AcondB
          reun <-  pa + pb - inter
          
        } else if(!is.na(pb) && !is.na(reun) && !is.na(inter)) {
          pa <- reun - pb + inter
          AcondB <- inter / pb
          BcondA <- inter / pa
          
        } else if(!is.na(pb) && !is.na(reun) && !is.na(AcondB)) {
          inter <- AcondB * pb
          pa <- reun - pb + inter
          BcondA <- inter / pa
          
        } else if(!is.na(pb) && !is.na(reun) && !is.na(BcondA)) {
          pa <- (reun - pb) / (1-BcondA)
          inter <- pa + pb - reun
          AcondB <- inter / pb
          
        } else if(!is.na(pb) && !is.na(inter) && !is.na(BcondA)) {
          pa <- inter / BcondA 
          reun <- pa + pb - inter
          AcondB <- inter / pa
          
        } else if(!is.na(pb) && !is.na(AcondB) && !is.na(BcondA)) {
          inter <-  pb * AcondB
          pb <- inter / AcondB
          reun <-  pa + pb - inter
          
        } else if(!is.na(reun) && !is.na(inter) && !is.na(AcondB)) {
          pb <- inter / AcondB
          pa <- reun - pb + inter
          BcondA <- inter / pa
          
        } else if(!is.na(reun) && !is.na(inter) && !is.na(BcondA)) {
          pa <- inter / BcondA
          pb <- reun - pa + inter
          BcondA <- inter / pa
          
        } else if(!is.na(reun) && !is.na(AcondB) && !is.na(BcondA)) {
          inter <- reun / (1/BcondA + 1/AcondB - 1)
          pa <- inter / BcondA
          pb <- inter / AcondB
          
        } else if(!is.na(inter) && !is.na(AcondB) && !is.na(BcondA)) {
          pa <- inter / BcondA
          pb <- inter / AcondB
          reun <- pa + pb - inter
          
        } else { ok <- FALSE }
      }
    
    if(ok) {
      output$ex_3_eroare <- renderText("")
      updateNumericInput(session, "ex_3_PA", value = pa)
      updateNumericInput(session, "ex_3_PB", value = pb)
      updateNumericInput(session, "ex_3_PAorB", value = reun)
      updateNumericInput(session, "ex_3_PAandB", value = inter)
      updateNumericInput(session, "ex_3_PArB", value = AcondB)
      updateNumericInput(session, "ex_3_PBrA", value = BcondA)
    } else {
      output$ex_3_eroare <- renderText("Tabelul nu se poate completa")
      updateNumericInput(session, "ex_3_PA", value = NULL)
      updateNumericInput(session, "ex_3_PB", value = NULL)
      updateNumericInput(session, "ex_3_PAorB", value = NULL)
      updateNumericInput(session, "ex_3_PAandB", value = NULL)
      updateNumericInput(session, "ex_3_PArB", value = NULL)
      updateNumericInput(session, "ex_3_PBrA", value = NULL)
    }
  })

}

shinyApp(ui, server)