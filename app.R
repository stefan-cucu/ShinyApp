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
      menuItem("Ex. 2", tabName="ex2", icon = icon('th')),
      menuItem("Ex. 5", tabName="ex5"),
      menuItem("Ex. 6", tabName="ex6"),
      menuItem("Ex. 7", tabName="ex7"),
      menuItem("Ex. 8", tabName="ex8")
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
        if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 0){
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
          if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 0){
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
            df, options = list(pageLength=10)
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
        if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 0){
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
          if(input$ex_1_bern_p == 0 || input$ex_1_bern_p == 0){
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
            df, options = list(pageLength=10)
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
}

# Run the application 
shinyApp(ui = ui, server = server)