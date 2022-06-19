library(shiny)
library(shinydashboard)
library(discreteRV)
library(DT)
library(dplyr)
library(rlist)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(uiOutput("va_select")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ex. 1 - Galerie", tabName="ex1", icon = icon('th')),
      menuItem("Ex. 2", tabName="ex2", icon = icon('pencil')),
      menuItem("Ex. 3", tabName="ex3", icon = icon('calculator')),
      menuItem("Ex. 5", tabName="ex5", icon = icon('eye')),
      menuItem("Ex. 6", tabName="ex6", icon = icon('less-than-equal')),
      menuItem("Ex. 7", tabName="ex7", icon = icon('filter')),
      menuItem("Ex. 8", tabName="ex8", icon = icon('equals')),
      menuItem("Ex. 9", tabName="ex9", icon = icon('table')),
      menuItem("Ex. 11", tabName="ex11", icon = icon('chart-bar')),
      menuItem("Ex. 12", tabName="ex12", icon = icon('plus'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "ex1",
        fluidPage(
          titlePanel("Ex. 1 - Galerie"),
          box(
            title="Bernoulli", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_bern_p", "p:", 0, min=0, max=1, step=0.1),
            "Functia de masa",
            plotOutput("ex_1_bern_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_bern_plot2", height = "290px"),
            textOutput("ex_1_bern_txt")
          ),
          box(
            title="Binomiala", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_binom_n", "n:", 0, min=0),
            numericInput("ex_1_binom_p", "p:", 0, min=0, max=1, step=0.1),
            "Functia de masa",
            plotOutput("ex_1_binom_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_binom_plot2", height = "290px"),
            textOutput("ex_1_binom_txt")
          ),
          box(
            title="Geometrica", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_geom_p", "p:", 0.01, min=0.01, max=1, step=0.1),
            "Functia de masa",
            plotOutput("ex_1_geom_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_geom_plot2", height = "290px"),
            textOutput("ex_1_geom_txt")
          ),
          box(
            title="Hipergeometrica", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_hgeom_m", "m:", 0, min=0),
            numericInput("ex_1_hgeom_n", "n:", 0, min=0),
            numericInput("ex_1_hgeom_k", "k:", 0, min=0),
            "Functia de masa",
            plotOutput("ex_1_hgeom_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_hgeom_plot2", height = "290px"),
            textOutput("ex_1_hgeom_txt")
          ),
          box(
            title="Poisson", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_pois_l", "lambda:", 0, min=0),
            "Functia de masa",
            plotOutput("ex_1_pois_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_pois_plot2", height = "290px"),
            textOutput("ex_1_pois_txt")
          ),
          box(
            title="Uniforma", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_unif_a", "a:", 0),
            numericInput("ex_1_unif_b", "b:", 1),
            "Functia de densitate",
            plotOutput("ex_1_unif_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_unif_plot2", height = "290px"),
            textOutput("ex_1_unif_txt")
          ),
          box(
            title="Exponentiala", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_exp_l", "lambda:", 0, min=0),
            "Functia de densitate",
            plotOutput("ex_1_exp_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_exp_plot2", height = "290px"),
            textOutput("ex_1_exp_txt")
          ),
          box(
            title="Normala", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_norm_m", "Media:", 1),
            numericInput("ex_1_norm_d", "Deviatia standard:", 1, min=0.001),
            "Functia de densitate",
            plotOutput("ex_1_norm_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_norm_plot2", height = "290px"),
            textOutput("ex_1_norm_txt")
          ),
          uiOutput("ex2discrete"),
          uiOutput("ex2continue")
        )
      ),
      tabItem(
        tabName = "ex3",
        fluidPage(
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
      ),
      tabItem(
        tabName = "ex2",
        fluidPage(
          titlePanel("Ex. 2"),
          tabsetPanel(
            tabPanel(
              title="V.A. Discrete",
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
            ),
            tabPanel(
              title="V.A. Continue",
              fluidRow(
                column(4, 
                       textInput("ex_2_f", "f(x)=", width = "70%"),
                       splitLayout(cellWidths = c("35%", "35%"), 
                                   numericInput("ex_2_cstart", "Start interval", value=0),
                                   numericInput("ex_2_cend", "Sfarsit interval", value=0)),
                       actionButton("ex_2_btnc", "Introducere"),
                       verbatimTextOutput("proprietatic2")
                ),
                column(4,
                       h3("Functia de densitate"),
                       plotOutput("ex_2_cdens")
                ),
                column(4,
                       h3("Functia de repartitie"),
                       plotOutput("ex_2_crep")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "ex5",
        fluidPage(
          titlePanel("Ex. 5"),
          uiOutput("ex_5_shownum"),
          sliderInput(inputId = "ex_5_begin",
                              label = "Where to display from:",
                              min = 0,
                              max = 99,
                              value = 0),
          actionButton('ex_5_btn', "Afiseaza"),
          textOutput("ex_5_txt"),
          DTOutput('ex_5_tbl')
        )
      ),
      tabItem(
        tabName = "ex6",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              titlePanel("Ex. 6"),
              textInput("ex_6_in", "Probabilitatea cautata: "),
              actionButton("ex_6_btn", "Calculeaza")
            ),
            mainPanel(
              textOutput("ex_6_out")
            )
          )
        )
      ),
      tabItem(
        tabName = "ex7",
        fluidPage(
          titlePanel("Ex. 7"),
          uiOutput("ex_7_shownum"),
          textInput(inputId = "functionInput", "Function"),
          actionButton(inputId = "ex_7_btn", "Evaluate"),
          sliderInput(inputId = "ex_7_begin",
                      label = "Where to display from:",
                      min = 0,
                      max = 99,
                      value = 0),
          textOutput("ex_7_txt"),
          DTOutput('ex_7_tbl')
        ),
      ),
      tabItem(
        tabName = "ex8",
        fluidPage(
          titlePanel("Ex. 8"),
          textInput(inputId = "ex8_functionInput", "Function"),
          uiOutput("ex_8_shownum"),
          actionButton('ex_8_eval', "Evaluate"),
          textOutput("ex_8_med"),
          textOutput("ex_8_disp")
        )
      ),
      tabItem(
        tabName = "ex9",
        fluidPage(
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
      ),
      tabItem(
        tabName = "ex11",
        fluidPage(
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
      ),
      tabItem(
        tabName = "ex12",
        fluidPage(
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
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$va_select <- renderUI({
    predef <- c("Bernoulli"="bern",
                "Binomiala"="binom",
                "Geometrica"="geom",
                "Hipergeometrica"="hgeom",
                "Poisson"="pois",
                "Uniforma"="unif",
                "Exponentiala"="exp",
                "Normala"="norm")
    if(ex2_rvs$cnt > 0){
      print(ex2_rvs$cnt)
      print(1:ex2_rvs$cnt)
      customd <- sapply(1:ex2_rvs$cnt, function(a){
        return(sprintf("d%d", a))
      })
      names(customd) = sapply(1:ex2_rvs$cnt, function(a){
        return(sprintf("Custom v.a. discreta %d", a))
      })
      predef <- c(predef, customd)
    }
    if(ex2_fcts$cnt > 0){
      customc <- sapply(1:ex2_fcts$cnt, function(a){
        return(sprintf("c%d", a))
      })
      names(customc) = sapply(1:ex2_fcts$cnt, function(a){
        return(sprintf("Custom v.a. continua %d", a))
      })
      predef <- c(predef, customc)
    }
    selectInput("va_selected", label="VA", predef)
  })
  
    #########
    # Ex. 1 #
    #########
  
    # Bern
    output$ex_1_bern_plot1 <- renderPlot({
        p <- input$ex_1_bern_p
        X <- RV(c(0,1), c(1-p, p))
        x <- seq(-5, 5)
        y <- sapply(x, function(nr) {
          if(nr %in% X) return(P(X == nr))
          else return(0)
        })
        plot(x, y)
    })
    
    output$ex_1_bern_plot2 <- renderPlot({
      p <- input$ex_1_bern_p
      X <- RV(c(0,1), c(1-p, p))
      x <- seq(-5, 5, 0.001)
      y <- sapply(x, function(nr) {
        return(P(X <= nr))
      })
      plot(x, y)
    })
    
    output$ex_1_bern_txt <- renderText({
      p <- input$ex_1_bern_p
      paste("Media:", p, "\nDispersia:", p * (1 - p))
    })
    
    # Binom
    output$ex_1_binom_plot1 <- renderPlot({
      p <- input$ex_1_binom_p
      n <- input$ex_1_binom_n
      
      x <- seq(0, n+2)
      plot(x, dbinom(x,n,p))
    })
    
    output$ex_1_binom_plot2 <- renderPlot({
      p <- input$ex_1_binom_p
      n <- input$ex_1_binom_n
      
      x <- seq(0, n+2,0.001)
      plot(x,pbinom(x,n,p))
    })
    
    output$ex_1_binom_txt <- renderText({
      p <- input$ex_1_binom_p
      n <- input$ex_1_binom_n
      paste("Media:", p*n, "\nDispersia:", n * p * (1 - p))
    })
    
    # Geom
    output$ex_1_geom_plot1 <- renderPlot({
      p <- input$ex_1_geom_p
      x <- seq(0, 6)
      plot(x, dgeom(x,p))
    })
    
    output$ex_1_geom_plot2 <- renderPlot({
      p <- input$ex_1_geom_p
      x <- seq(0, 6,0.001)
      plot(x,pgeom(x,p))
    })
    
    output$ex_1_geom_txt <- renderText({
      p <- input$ex_1_geom_p
      paste("Media:", (1 - p) / p, "\nDispersia:", (1 - p) / (p ^ 2))
    })
    
    # Hgeom
    output$ex_1_hgeom_plot1 <- renderPlot({
      n <- input$ex_1_hgeom_n
      m <- input$ex_1_hgeom_m
      k <- input$ex_1_hgeom_k
      
      x <- seq(0, k+1)
      plot(x, dhyper(x,m,n,k))
    })
    
    output$ex_1_hgeom_plot2 <- renderPlot({
      n <- input$ex_1_hgeom_n
      m <- input$ex_1_hgeom_m
      k <- input$ex_1_hgeom_k
      x <- seq(0, k+1, 0.001)
      plot(x,phyper(x,m,n,k))
    })
    
    output$ex_1_hgeom_txt <- renderText({
      n <- input$ex_1_hgeom_n
      m <- input$ex_1_hgeom_m
      k <- input$ex_1_hgeom_k
      p <- m / (m + n)
      paste("Media:", k * p, "\nDispersia:", k * p * (1 - p) * (m + n - k) / (m + n - 1))
    })
    
    # Pois
    output$ex_1_pois_plot1 <- renderPlot({
      l <- input$ex_1_pois_l
      x <- seq(0, 6)
      plot(x, dpois(x,l))
    })
    
    output$ex_1_pois_plot2 <- renderPlot({
      l <- input$ex_1_pois_l
      x <- seq(0, 6,0.001)
      plot(x,ppois(x,l))
    })
    
    output$ex_1_pois_txt <- renderText({
      l <- input$ex_1_pois_l
      paste("Media:", l, "\nDispersia:", l)
    })
    
    # Unif
    output$ex_1_unif_plot1 <- renderPlot({
      a <- input$ex_1_unif_a
      b <- input$ex_1_unif_b
      
      x <- seq(a-1, b+1, 0.1)
      plot(x, dunif(x,a,b), type='l')
      lines(x, dunif(x,a,b))
    })
    
    output$ex_1_unif_plot2 <- renderPlot({
      a <- input$ex_1_unif_a
      b <- input$ex_1_unif_b
      
      x <- seq(a-1, b+1,0.001)
      plot(x,punif(x,a,b), type='l')
    })
    
    output$ex_1_unif_txt <- renderText({
      a <- input$ex_1_unif_a
      b <- input$ex_1_unif_b
      paste("Media:", 0.5 * (a + b), "\nDispersia:", ((b - a) ^ 2) / 12)
    })
    
    # Exp
    output$ex_1_exp_plot1 <- renderPlot({
      l <- input$ex_1_exp_l
      
      x <- seq(0, 6, 0.1)
      plot(x, dexp(x,l), type='l')
    })
    
    output$ex_1_exp_plot2 <- renderPlot({
      l <- input$ex_1_exp_l
      
      x <- seq(0, 6, 0.1)
      plot(x,pexp(x,l), type='l')
    })
    
    output$ex_1_exp_txt <- renderText({
      l <- input$ex_1_exp_l
      paste("Media:", 1 / l, "\nDispersia:", 1 / (l ^ 2))
    })
    
    # Norm
    output$ex_1_norm_plot1 <- renderPlot({
      n <- input$ex_1_norm_m
      d <- input$ex_1_norm_d
      
      x <- seq(n-d/2, n+d/2, 0.1)
      plot(x, dnorm(x,n,d), type='l')
    })
    
    output$ex_1_norm_plot2 <- renderPlot({
      n <- input$ex_1_norm_m
      d <- input$ex_1_norm_d
      
      x <- seq(n-d/2, n+d/2, 0.1)
      plot(x, pnorm(x,n,d), type='l')
    })
    
    output$ex_1_norm_txt <- renderText({
      n <- input$ex_1_norm_m
      d <- input$ex_1_norm_d
      paste("Media:", n, "\nDispersia:", d ^ 2)
    })
    
    output$ex2discrete <- renderUI({
      if(ex2_rvs$cnt == 0)
        return()
      
      lapply(1:ex2_rvs$cnt, function(a) {
        output[[sprintf("ex_1_#%d_plot1", a)]] <- renderPlot({
          plot(ex2_rvs$arr[[a]])
        })
        output[[sprintf("ex_1_#%d_plot2", a)]] <- renderPlot({
          X <- ex2_rvs$arr[[a]]
          domeniu <- seq(min(X)-1, max(X)+1, 0.01)
          codomeniu <- sapply(domeniu, function(nr) {
            return(P(X<=nr))
          })
          plot(domeniu, codomeniu, pch=19)
        })
        output[[sprintf("ex_1_#%d_text", a)]] <- renderText({
          X <- ex2_rvs$arr[[a]]
          paste("Media:", E(X), "\nDispersia:", V(X))
        })
        box(
          title=sprintf("V.A. Discreta #%d", a), status="primary", solidHeader=TRUE, collapsible=TRUE,
          "Functia de masa",
          plotOutput(sprintf("ex_1_#%d_plot1", a), height = "290px"),
          "Functia de repartitie",
          plotOutput(sprintf("ex_1_#%d_plot2", a), height = "290px"),
          textOutput(sprintf("ex_1_#%d_text", a))
        )
      })
    })
    
    output$ex2continue <- renderUI({
      if(ex2_fcts$cnt == 0)
        return()
      
      lapply(1:ex2_fcts$cnt, function(a) {
        func_string <- ex2_fcts$arr[[a]][[1]]
        cstart <- ex2_fcts$arr[[a]][[2]]
        cend <- ex2_fcts$arr[[a]][[3]]
        
        f <-  function(x) {
          check <- mapply(function(val){
            if(val < cstart)
              return(FALSE)
            else if(val > cend){
              return(FALSE)
            }
            else return(TRUE)
          }, x)
          x <- eval(parse(text=func_string))
          x[!check] <- 0
          return(x)
        }
        
        Fx <- function(x) {
          return(
            mapply(function(x){
              return(integrate(f, -Inf, x)$value)
            }, x)
          )
        }
        
        output[[sprintf("ex_2_#%d_plot1", a)]] <- renderPlot({
          domeniu <- seq(cstart, cend, 0.1)
          valori <- f(domeniu)
          plot(domeniu, valori, type='l')
        })
        
        output[[sprintf("ex_2_#%d_plot2", a)]] <- renderPlot({
          domeniu <- seq(cstart - 1, cend + 1, 0.1)
          valori <- Fx(domeniu)
          plot(domeniu, valori, type='l')
        })
        
        media <- integrate(function(x){
          x * f(x)
        }, -Inf, Inf)$value
        
        media2 <- integrate(function(x){
          x * x * f(x)
        }, -Inf, Inf)$value
        
        box(
          title=sprintf("V.A. Continua #%d", a), status="warning", solidHeader=TRUE, collapsible=TRUE,
          "Functia de densitate",
          plotOutput(sprintf("ex_2_#%d_plot1", a), height = "290px"),
          "Functia de repartitie",
          plotOutput(sprintf("ex_2_#%d_plot2", a), height = "290px"),
          paste("Media: ", media),
          paste("Dispersia: ", media * media - media2)
        )
      })
    })
    
    #########
    # Ex. 2 #
    #########
    
    ex2_rvs <- reactiveValues(arr = list(), cnt = 0)
    ex2_fcts <- reactiveValues(arr = list(), cnt = 0)
    
    output$table2 <- renderUI({
      
      vals <- lapply(1:input$nrVal2, function(val){
        div(style="display: inline-block; width: 60px;", 
            numericInput(paste("value", val),"",0, min = 0) )
      })
      
      pr <- lapply(1:input$nrVal2, function(val){
        div(style="display: inline-block; width: 60px;", 
            numericInput(paste("prob", val),"", 0, min = 0) )
      })
      
      tags$div(
        tags$div(
          div(style="display: inline-block; width: 100px;","Valori:"),
          tagList(vals)),
        tags$div(
          div(style="display: inline-block; width: 100px;","Probabilitati:"),
          tagList(pr)))
      
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
    
    
    observeEvent(input$do2, {
      values <- sapply(1:input$nrVal2, function(val){
        return(input[[paste("value", val)]])
      })
      
      probs <- sapply(1:input$nrVal2, function(val){
        return(input[[paste("prob", val)]])
      })
      X <- RV(values,probs)
      
      ex2_rvs$cnt <- ex2_rvs$cnt + 1
      ex2_rvs$arr[[ex2_rvs$cnt]] <-  X
      
      # output$va_select <- renderUI({
      #   predef <- c("Bernoulli"="bern",
      #               "Binomiala"="binom",
      #               "Geometrica"="geom",
      #               "Hipergeometrica"="hgeom",
      #               "Poisson"="pois",
      #               "Uniforma"="unif",
      #               "Exponentiala"="exp",
      #               "Normala"="norm")
      #   customd <- sapply(1:ex2_rvs$cnt, function(a){
      #     return(sprintf("c%d", a))
      #   })
      #   names(customd) = sapply(1:ex2_rvs$cnt, function(a){
      #     return(sprintf("Custom v.a. discreta %d", a))
      #   })
      #   customc <- sapply(1:ex2_fcts$cnt, function(a){
      #     return(sprintf("c%d", a))
      #   })
      #   names(customc) = sapply(1:ex2_fcts$cnt, function(a){
      #     return(sprintf("Custom v.a. continua %d", a))
      #   })
      #   predef <- c(predef, customd, customc)
      #   selectInput("va_selected", label="VA", predef)
      # })
    })
    
    observeEvent(input$ex_2_btnc, {
      if(input$ex_2_cstart > input$ex_2_cend){
        output$proprietatic2 <- renderText({
          paste("Interval gresit!")
        })
        return()
      }
      
      f <-  function(x) {
        check <- mapply(function(val){
          if(val < input$ex_2_cstart)
            return(FALSE)
          else if(val > input$ex_2_cend){
            return(FALSE)
          }
          else return(TRUE)
        }, x)
        x <- eval(parse(text=input$ex_2_f))
        x[!check] <- 0
        return(x)
      }
      
      if(between(integrate(f, input$ex_2_cstart-1, input$ex_2_cend)$value, 0.95, 1.05) == FALSE){
        output$proprietatic2 <- renderText({
          paste("Functie gresita!")
        })
        return()
      }
      
      Fx <- function(x) {
        return(
          mapply(function(x){
            return(integrate(f, -Inf, x)$value)
          }, x)
        )
      }
      
      output$ex_2_cdens <- renderPlot({
        domeniu <- seq(input$ex_2_cstart, input$ex_2_cend, 0.1)
        valori <- f(domeniu)
        plot(domeniu, valori, type='l')
      })
      
      output$ex_2_crep <- renderPlot({
        domeniu <- seq(input$ex_2_cstart - 1, input$ex_2_cend + 1, 0.1)
        valori <- Fx(domeniu)
        plot(domeniu, valori, type='l')
      })
      
      output$proprietatic2 <- renderText({
        media <- integrate(function(x){
          x * f(x)
        }, -Inf, Inf)$value
        dispersia <- integrate(function(x){
          x * x * f(x)
        }, -Inf, Inf)$value
        paste("Media repartitiei este: ", media,
              "\nDispersia repartitiei este: ", dispersia)
      })
      
      ex2_fcts$cnt <- ex2_fcts$cnt + 1
      ex2_fcts$arr[[ex2_fcts$cnt]] <- list(input$ex_2_f, input$ex_2_cstart, input$ex_2_cend)
    })
    #########
    # Ex. 3 #
    #########
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
    #########
    # Ex. 5 #
    #########
    
    ex_5_max <- reactiveValues(val = 0)
    
    observeEvent(input$va_selected, {
      tip <- input$va_selected
      if(tip == "unif" || tip == "norm" || tip == "exp" || startsWith(tip, "c")){
        output$ex_5_txt <- renderText({
          paste("VA aleasa nu este discreta!")
        })
        return()
      }
      if(tip == "bern"){
        if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 1){
          ex_5_max$val <- 1
          updateSliderInput(session, "ex_5_begin", min=0, max=0, value=0)
        }
        else{
          ex_5_max$val <- 2
          updateSliderInput(session, "ex_5_begin", min=0, max=1, value=0)
        }
      }
      if(tip == "binom"){
        ex_5_max$val <- input$ex_1_binom_n + 1
        updateSliderInput(session, "ex_5_begin", min=0, max=input$ex_1_binom_n, value=0)
      }
      if(tip == "hgeom"){
        ex_5_max$val <- input$ex_1_hgeom_k + 1
        updateSliderInput(session, "ex_5_begin", min=0, max=input$ex_1_hgeom_k, value=0)
      }
      if(startsWith(tip, "d")){
        nr <- strtoi(substring(tip, 2))
        ex_5_max$val <- length(probs(ex2_rvs$arr[[nr]]))
        updateSliderInput(session, "ex_5_begin", min=0, max=ex_5_max$val, value=0)
      }
      if(tip == "geom" || tip == "pois"){
        output$ex_5_shownum <- renderUI({
          numericInput("ex_5_dim", "N max:", 0)
        })
      }
      else {
        output$ex_5_shownum <- renderUI({
        })
      }
    })  
      
      observeEvent(input$ex_5_btn, {
        tip <- input$va_selected
        if(tip == "unif" || tip == "norm" || tip == "exp" || startsWith(tip, "c")){
          output$ex_5_txt <- renderText({
            paste("VA aleasa nu este discreta!")
          })
          return()
        }
        else {
          output$ex_5_txt <- renderText({
            paste("")
          })
        }
        
        if(tip == "bern"){
          p <- input$ex_1_bern_p
          X <- RV(c(0,1), c(1-p, p))
          if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 1){
            ex_5_max$val <- 1
            updateSliderInput(session, "ex_5_begin", min=0, max=0, value=0)
          }
          else{
            ex_5_max$val <- 2
            updateSliderInput(session, "ex_5_begin", min=0, max=1, value=0)
          }
        }
        
        if(tip == "binom"){
          X <- RV(0:input$ex_1_binom_n, dbinom(0:input$ex_1_binom_n,input$ex_1_binom_n,input$ex_1_binom_p))
          ex_5_max$val <- input$ex_1_binom_n + 1
          updateSliderInput(session, "ex_5_begin", min=0, max=input$ex_1_binom_n, value=0)
        }
        
        if(tip == "geom"){
          X <- RV(0:input$ex_5_dim, dgeom(0:input$ex_5_dim, input$ex_1_geom_p))
          observeEvent(input$ex_5_dim, {
            if(input$ex_5_dim>length(outcomes(X))){ex_5_max$val <- length(outcomes(X))+1}else{ex_5_max$val <- input$ex_5_dim+1}
            updateSliderInput(session, "ex_5_begin", min=0, max=ex_5_max$val-1, value=0)
          })
        }
        
        if(tip == "hgeom"){
          X <- RV(0:input$ex_1_hgeom_k, dhyper(0:input$ex_1_hgeom_k,input$ex_1_hgeom_m,input$ex_1_hgeom_n,input$ex_1_hgeom_k))
          if(input$ex_1_hgeom_k>length(outcomes(X))){ex_5_max$val <- length(outcomes(X))+1}else{ex_5_max$val <- input$ex_1_hgeom_k+1}
          updateSliderInput(session, "ex_5_begin", min=0, max=ex_5_max$val-1, value=0)
        }
        
        if(tip == "pois"){
          X <- RV(0:input$ex_5_dim, dpois(0:input$ex_5_dim, input$ex_1_pois_l))
          observeEvent(input$ex_5_dim, {
            if(input$ex_5_dim>length(outcomes(X))){ex_5_max$val <- length(outcomes(X))+1}else{ex_5_max$val <- input$ex_5_dim+1}
            updateSliderInput(session, "ex_5_begin", min=0, max=ex_5_max$val-1, value=0)
          })
        }
        
        if(startsWith(tip, "d")){
          nr <- strtoi(substring(tip, 2))
          X <- ex2_rvs$arr[[nr]]
        }
        
        Outcomes <-  outcomes(X)
        Probabilities <-  probs(X)
        df <- data.frame(Outcomes,Probabilities)
        observeEvent(input$ex_5_begin,{
          df <- tail(df,ex_5_max$val-input$ex_5_begin)
          output$ex_5_tbl = renderDT(
             datatable(df, options=list(pageLength= 10),rownames= FALSE)
          )
        })
      })
    
    
    
    #########
    # Ex. 6 #
    #########
    
    observeEvent(input$ex_6_btn, {
      tip <- input$va_selected
      if(tip == "unif" || tip == "norm" || tip == "exp" || startsWith(tip, "c")){
        instr <- input$ex_6_in
        if(tip == "unif"){
          a <- input$ex_1_unif_a
          b <- input$ex_1_unif_b
          p <- 0
          if(substring(instr, 2, 3) == "<="){
            val <- as.numeric(substring(instr, 4))
            p <- punif(val, a, b)
          }
          else if(substring(instr, 2, 2) == "<"){
            val <- as.numeric(substring(instr, 3))
            p <- punif(val, a, b)
          }
          else if(substring(instr, 2, 3) == ">="){
            val <- as.numeric(substring(instr, 4))
            p <- 1 - punif(val, a, b)
          }
          else if(substring(instr, 2, 2) == ">"){
            val <- as.numeric(substring(instr, 3))
            p <- 1 -punif(val, a, b)
          }
          else if(substring(instr, 2, 3) == "=="){
            p <- 0
          }
          else {
            output$ex_6_out <- renderText({
              "Operatie invalida!"
            })
            return()
          }
          output$ex_6_out <- renderText({
            paste("P(", instr, ") = ", p, sep="")
          })
        }
        if(tip == "norm"){
          n <- input$ex_1_norm_m
          d <- input$ex_1_norm_d
          p <- 0
          if(substring(instr, 2, 3) == "<="){
            val <- as.numeric(substring(instr, 4))
            p <- pnorm(val, n, d)
          }
          else if(substring(instr, 2, 2) == "<"){
            val <- as.numeric(substring(instr, 3))
            p <- punif(val, n, d)
          }
          else if(substring(instr, 2, 3) == ">="){
            val <- as.numeric(substring(instr, 4))
            p <- 1 - punif(val, n, d)
          }
          else if(substring(instr, 2, 2) == ">"){
            val <- as.numeric(substring(instr, 3))
            p <- 1 -punif(val, n, d)
          }
          else if(substring(instr, 2, 3) == "=="){
            p <- 0
          }
          else {
            output$ex_6_out <- renderText({
              "Operatie invalida!"
            })
            return()
          }
          output$ex_6_out <- renderText({
            paste("P(", instr, ") = ", p, sep="")
          })
        }
        if(tip == "exp"){
          l <- input$ex_1_exp_l
          p <- 0
          if(substring(instr, 2, 3) == "<="){
            val <- as.numeric(substring(instr, 4))
            p <- pexp(val, l)
          }
          else if(substring(instr, 2, 2) == "<"){
            val <- as.numeric(substring(instr, 3))
            p <- pexp(val, l)
          }
          else if(substring(instr, 2, 3) == ">="){
            val <- as.numeric(substring(instr, 4))
            p <- 1 - pexp(val, l)
          }
          else if(substring(instr, 2, 2) == ">"){
            val <- as.numeric(substring(instr, 3))
            p <- 1 -pexp(val, l)
          }
          else if(substring(instr, 2, 3) == "=="){
            p <- 0
          }
          else {
            output$ex_6_out <- renderText({
              "Operatie invalida!"
            })
            return()
          }
          output$ex_6_out <- renderText({
            paste("P(", instr, ") = ", p, sep="")
          })
        }
        if(startsWith(tip, "c")){
          tip <- input$va_selected
          a <- strtoi(substring(tip, 2))
          func_string <- ex2_fcts$arr[[a]][[1]]
          cstart <- ex2_fcts$arr[[a]][[2]]
          cend <- ex2_fcts$arr[[a]][[3]]
          
          f <-  function(x) {
            check <- mapply(function(val){
              if(val < cstart)
                return(FALSE)
              else if(val > cend){
                return(FALSE)
              }
              else return(TRUE)
            }, x)
            x <- eval(parse(text=func_string))
            x[!check] <- 0
            return(x)
          }
          
          Fx <- function(x) {
            return(
              mapply(function(x){
                return(integrate(f, -Inf, x)$value)
              }, x)
            )
          }
          p <- 0
          if(substring(instr, 2, 3) == "<="){
            val <- as.numeric(substring(instr, 4))
            p <- Fx(val)
          }
          else if(substring(instr, 2, 2) == "<"){
            val <- as.numeric(substring(instr, 3))
            p <- Fx(val)
          }
          else if(substring(instr, 2, 3) == ">="){
            val <- as.numeric(substring(instr, 4))
            p <- 1 - Fx(val)
          }
          else if(substring(instr, 2, 2) == ">"){
            val <- as.numeric(substring(instr, 3))
            p <- 1 - Fx(val)
          }
          else if(substring(instr, 2, 3) == "=="){
            p <- 0
          }
          else {
            output$ex_6_out <- renderText({
              "Operatie invalida!"
            })
            return()
          }
          output$ex_6_out <- renderText({
            paste("P(", instr, ") = ", p, sep="")
          })
        }
      }
      else {
        if(tip == "bern"){
          p <- input$ex_1_bern_p
          X <- RV(c(0,1), c(1-p, p))
        }
        
        if(tip == "binom"){
          X <- RV(0:input$ex_1_binom_n, dbinom(0:input$ex_1_binom_n,input$ex_1_binom_n,input$ex_1_binom_p)) 
        }
        
        if(tip == "geom"){
          X <- RV(0:input$ex_5_dim, dgeom(0:input$ex_5_dim, input$ex_1_geom_p))
        }
        
        if(tip == "hgeom"){
          X <- RV(0:input$ex_1_hgeom_k, dhyper(0:input$ex_1_hgeom_k,input$ex_1_hgeom_m,input$ex_1_hgeom_n,input$ex_1_hgeom_k)) 
        }
        
        if(tip == "pois"){
          X <- RV(0:input$ex_5_dim, dpois(0:input$ex_5_dim, input$ex_1_pois_l))
        }
        
        if(startsWith(tip, "d")){
          nr <- strtoi(substring(tip, 2))
          X <- ex2_rvs$arr[[nr]]
        }
        output$ex_6_out <- renderText({
          paste("P(", input$ex_6_in, ") = ", P(eval(parse(text = input$ex_6_in))), sep="")
        })
      }
    })
    #########
    # Ex. 7 #
    #########
    
    ex_7_max <- reactiveValues(val = 0)
    observeEvent(input$va_selected, {
      tip <- input$va_selected
      if(tip == "unif" || tip == "norm" || tip == "exp" || startsWith(tip, "c")){
        output$ex_7_txt <- renderText({
          paste("VA aleasa nu este discreta!")
        })
        return()
      }
      if(tip == "bern"){
        if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 1){
          ex_7_max$val <- 1
          updateSliderInput(session, "ex_7_begin", min=0, max=0, value=0)
        }
        else{
          ex_7_max$val <- 2
          updateSliderInput(session, "ex_7_begin", min=0, max=1, value=0)
        }
      }
      if(tip == "binom"){
        ex_7_max$val <- input$ex_1_binom_n + 1
        updateSliderInput(session, "ex_7_begin", min=0, max=input$ex_1_binom_n, value=0)
      }
      if(tip == "hgeom"){
        ex_7_max$val <- input$ex_1_hgeom_k + 1
        updateSliderInput(session, "ex_7_begin", min=0, max=input$ex_1_hgeom_k, value=0)
      }
      if(startsWith(tip, "d")){
        nr <- strtoi(substring(tip, 2))
        ex_7_max$val <- length(probs(ex2_rvs$arr[[nr]]))
        updateSliderInput(session, "ex_7_begin", min=0, max=ex_7_max$val, value=0)
      }
      if(tip == "geom" || tip == "pois"){
        output$ex_7_shownum <- renderUI({
          numericInput("ex_7_dim", "N max:", 0)
        })
      }
      else {
        output$ex_7_shownum <- renderUI({
        })
      }
    })  
      
      observeEvent(input$ex_7_btn, {
        tip <- input$va_selected
        if(tip == "unif" || tip == "norm" || tip == "exp" || startsWith(tip, "c")){
          output$ex_7_txt <- renderText({
            paste("VA aleasa nu este discreta!")
          })
          return()
        }
        else {
          output$ex_7_txt <- renderText({
            paste("")
          })
        }
        
        if(tip == "bern"){
          p <- input$ex_1_bern_p
          X <- RV(c(0,1), c(1-p, p))
          if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 1){
            ex_7_max$val <- 1
            updateSliderInput(session, "ex_7_begin", min=0, max=0, value=0)
          }
          else{
            ex_7_max$val <- 2
            updateSliderInput(session, "ex_7_begin", min=0, max=1, value=0)
          }
        }
        
        if(tip == "binom"){
          X <- RV(0:input$ex_1_binom_n, dbinom(0:input$ex_1_binom_n,input$ex_1_binom_n,input$ex_1_binom_p))
          ex_7_max$val <- input$ex_1_binom_n + 1
          updateSliderInput(session, "ex_7_begin", min=0, max=input$ex_1_binom_n, value=0)
        }
        
        if(tip == "geom"){
          X <- RV(0:input$ex_7_dim, dgeom(0:input$ex_7_dim, input$ex_1_geom_p))
          observeEvent(input$ex_7_dim, {
            if(input$ex_7_dim>length(outcomes(X))){ex_7_max$val <- length(outcomes(X))+1}else{ex_7_max$val <- input$ex_7_dim+1}
            updateSliderInput(session, "ex_7_begin", min=0, max=ex_7_max$val-1, value=0)
          })
        }
        
        if(tip == "hgeom"){
          X <- RV(0:input$ex_1_hgeom_k, dhyper(0:input$ex_1_hgeom_k,input$ex_1_hgeom_m,input$ex_1_hgeom_n,input$ex_1_hgeom_k)) 
          if(input$ex_1_hgeom_k>length(outcomes(X))){ex_7_max$val <- length(outcomes(X))+1}else{ex_7_max$val <- input$ex_1_hgeom_k+1}
          updateSliderInput(session, "ex_7_begin", min=0, max=ex_7_max$val-1, value=0)
        }
        
        if(tip == "pois"){
          X <- RV(0:input$ex_7_dim, dpois(0:input$ex_7_dim, input$ex_1_pois_l))
          observeEvent(input$ex_7_dim, {
            if(input$ex_7_dim>length(outcomes(X))){ex_7_max$val <- length(outcomes(X))+1}else{ex_7_max$val <- input$ex_7_dim+1}
            updateSliderInput(session, "ex_7_begin", min=0, max=ex_7_max$val-1, value=0)
          })
        }
        
        if(startsWith(tip, "d")){
          nr <- strtoi(substring(tip, 2))
          X <- ex2_rvs$arr[[nr]]
        }
        
        f <-  function(x) {eval(parse(text = input$functionInput))}
        X <- f(X)
        
        Outcomes <-  outcomes(X)
        Probabilities <-  probs(X)
        df <- data.frame(Outcomes,Probabilities)
        observeEvent(input$ex_7_begin,{
          df <- tail(df,ex_7_max$val-input$ex_7_begin)
          output$ex_7_tbl = renderDT(
             datatable(df, options=list(pageLength= 10),rownames= FALSE)
          )
        })
      })
    
    #########
    # Ex. 8 #
    #########
    observeEvent(input$va_selected, {
      tip <- input$va_selected
      if (tip == "norm") {
        observeEvent(input$ex_8_eval, {
          f = function(x) {eval(parse(text = input$ex8_functionInput))}
          output$ex_8_med <- renderText(
            {paste("Media este: ",f(input$ex_1_norm_m))}
          )
          output$ex_8_disp <-
            renderText(
              {paste("Dispersia este: ",f(input$ex_1_norm_d))}
            )
        })
        
      }
    })
    #########
    # Ex. 9 #
    #########  
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
              
              mat[poz, j] <- ifelse(poz == nrlin,
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
          output$proprietati9 <- renderUI({p("")})
          output$medieX9 <- renderText("")
          output$medieY9 <- renderText("")
          output$dispersieX9 <- renderText("")
          output$dispersieY9 <- renderText("")
          output$covarianta9 <- renderText("")
          output$coef9 <- renderText("")
        } else {
          output$mesajEroare9 <- renderText("")
          
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
    #########
    # Ex.11 #
    #########
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
    #########
    # Ex.12 #
    ######### 
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

# Run the application 
shinyApp(ui = ui, server = server)