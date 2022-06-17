library(shiny)
library(discreteRV)

ui <- fluidPage(
  titlePanel("Ex9"),
  
  fluidRow(
    tags$head(
      tags$style(HTML("
        table {
          font-family: arial, sans-serif;
          border-collapse: collapse;
          width: 40%;
          margin: 0px 0px 30px 0px;
        }
        
        td, th {
          border: 1px solid #dddddd;
          text-align: left;
          padding: 8px;
          text-align:center;
        }
        
        "))),
    column(6,
           numericInput("nrValX9", "Introduceti numarul valorilor lui X", 1, min = 1),
           numericInput("nrValY9", "Introduceti numarul valorilor lui Y", 1, min = 1),
           uiOutput("tabel9"),
           actionButton("do9", "Completeaza")
          ),
    column(6,
           uiOutput("proprietati9"),
           textOutput("mesajEroare9"),
           textOutput("medieX9"),
           textOutput("medieY9"),
           textOutput("dispersieX9"),
           textOutput("dispersieY9"),
           textOutput("covarianta9"),
           textOutput("coef9")
    )
  )
)


server <- function(input, output, session) {
  
  output$tabel9 <- renderUI({
    tabel <- lapply(1:(input$nrValX9+2), function(val){
      
        line <- lapply(1:(input$nrValY9+2), function(val2){
          if(val == 1 && val2 == 1) {
            div(style="display: inline-block; width: 70px; text-align:center;", 
                p("X\\Y")
                )
          } else if(val == 1 && val2 == (input$nrValY9+2)) {
          div(style="display: inline-block; width: 70px; text-align:center;", 
              p("Pi")
              ) 
          } else if(val == (input$nrValX9+2) && val2 == 1) {
            div(style="display: inline-block; width: 70px; text-align:center;", 
                p("Qj")
            ) 
          } else{
            div(style="display: inline-block; width: 70px;", 
                numericInput(paste("tabRepCom", val, val2, sep=""),"",NULL, min = 0) )
          }
        })
        
        tags$div(
          div(style="display: inline-block; width: 100px; margin-bottom:0; margin-top:0;"),
          tagList(line)
        )
    })
  })
  
  observeEvent(input$do9, {
    mat <-  NULL

    for(i in 2:(input$nrValX9+2)) {
      mat <- rbind(mat, sapply(2:(input$nrValY9+2), function(j){
        input[[paste("tabRepCom", i, j, sep="")]]}))
    }
    
    # dif e o variabila de tip boolean
    # care retine 1 in cazul in care la o pargurgere a while-ului s-a modificat matricea
    # sau 0 in caz contrar
    # daca la o parcurgere nu s-a modificat matricea, atunci aceasta nu se poate completa
    
    nrlin <- nrow(mat)
    nrcol <- ncol(mat)
    mat[nrlin, nrcol] = 1
    
    dif <- TRUE
    while(dif){
      dif <- FALSE
      # se parcurg toate liniile si toate coloanele din matrice
      # daca pe linie / coloana exista o singura valoare necompletata, atunci se poate completa

      #pentru fiecare linie
      for(i in 1:nrlin) {
        poz <- which(is.na(mat[i,])) # indicii coloanelor de pe linia i pe care se afla NA
        
        if(length(poz) == 1) {# daca e un singur indice, atunci se poate completa
          dif <- TRUE
          
          mat[i,poz] <- ifelse(poz == nrcol,
                               sum(mat[i,], na.rm = TRUE),
                               mat[i,nrcol] - sum(mat[i,1:(nrcol-1)], na.rm = TRUE))
        }
      }
    
      #analog pentru fiecare coloana
      for(j in 1:nrcol) {
        poz <- which(is.na(mat[,j])) 
        
        if(length(poz) == 1) {
          dif <- TRUE
        
          mat[poz, j] <- ifelse(poz == nrcol,
                                sum(mat[,j], na.rm = TRUE),
                                mat[nrlin,j] - sum(mat[1:(nrlin-1),j], na.rm = TRUE))
        }
      }
      }
    
    # eventual de facut o verificare a faptului ca s a completat bine
    # sau ca valorile introduse au fost corecte
    # de ex, probabilitatile sunt >= 0
    # valorile pentru X si Y trebuie introduse obligatoriu
    # suma prob marg trebuie sa fie 1
      
    if(any(is.na(mat))){
      output$mesajEroare9 <- renderText({"Tabelul nu se poate completa!"})
    } else {
      
    
    ### urmatoarele chestii ar trebui sa se execute doar daca nu sunt erori
    
    # se actualizeaza tabelul
    for(i in 1:nrlin) {
      for(j in 1:nrcol) {
        updateNumericInput(session, paste("tabRepCom",i+1, j+1, sep=""), value = mat[i,j])
      }
    }
    
    
    #crearea variabilelor X si Y
    valX <- sapply(2:(input$nrValX9+1), function(ind) {
      input[[paste("tabRepCom", ind, 1, sep="")]]
    })
    
    valY <- sapply(2:(input$nrValY9+1), function(ind) {
      input[[paste("tabRepCom", 1, ind, sep="")]]
    })
    
    probX <- mat[1:(nrlin-1),nrcol]
    probY <- mat[nrlin,1:(nrcol-1)]
  
    #repart marginale
    X <- RV(valX, probX)
    Y <- RV(valY, probY)
    
    # mediile
    EX <- E(X)
    EY <- E(Y)
    
    # dispersiile
    VX <- V(X)
    VY <- V(Y)
    
    #covarianta
    prod <- function(out_rep_X, out_rep_Y, prob_rep) {
      allOutcomes <- as.vector(sapply(out_rep_X, function(x){x*out_rep_Y}))
      outcomes <- unique(allOutcomes)
      
      probs <- sapply(outcomes, function(out){
        indices <- which(allOutcomes==out)
        return(sum(prob_rep[indices]))
      })
      return(RV(outcomes, probs))
    }
    
    XtimesY <- prod(valX, valY, as.vector(t(mat[1:(nrlin-1), 1:(nrcol-1)])))
    cov <- E(XtimesY) - E(X)*E(Y)
    
    #coeficientul de corelatie
    coef <- cov/sqrt(V(X)*V(Y))
    
    
    ### Afisarea rezultatelor
    firstRowX <- lapply(valX, function(val) {tags$th(val)})
    
    secondRowX <- lapply(probX, function(val) {tags$td(val)})
    
    firstRowY <- lapply(valY, function(val) {tags$th(val)})
    
    secondRowY <- lapply(probY, function(val) {tags$td(val)})
    
    output$proprietati9 <- renderUI({
      
      tags$div(
        h3("Proprietati"),
        p("Repartitiile marginale"),
        tags$table(
          tags$tr(
            tags$th("X:"),
            firstRowX
          ),
          tags$tr(
            tags$td(""),
            secondRowX
          )
        ),
        tags$table(
          tags$tr(
            tags$th("Y:"),
            firstRowY
          ),
          tags$tr(
            tags$td(""),
            secondRowY
          )
        )
        
      )
    })
    
    output$medieX9 <- renderText({
      paste("Media lui X este ", EX)
    })
    
    output$medieY9 <- renderText({
      paste("Media lui Y este ", EY)
    })
    
    output$dispersieX9 <- renderText({
      paste("Dispersia lui X este ", VX)
    })
    
    output$dispersieY9 <- renderText({
      paste("Dispersia lui Y este ", VY)
    })
    
    output$covarianta9 <- renderText({
      paste("Covarianta este egala cu ", round(cov, digits = 2))
    })
    
    output$coef9 <- renderText({
      paste("Coeficientul de corelatie este egal cu ", round(coef, digits = 2))
    })
    }
  })
}


shinyApp(ui, server)

