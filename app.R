library(shiny)
library(shinydashboard)
library(discreteRV)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ex. 1 - Galerie", tabName="ex1", icon = icon('th')),
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
            "Media",
            "Dispersia"
          ),
          box(
            title="Binomiala", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_binom_n", "n:", 0, min=0),
            numericInput("ex_1_binom_p", "p:", 0, min=0, max=1, step=0.1),
            "Functia de masa",
            plotOutput("ex_1_binom_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_binom_plot2", height = "290px"),
            "Media",
            "Dispersia"
          ),
          box(
            title="Geometrica", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_geom_p", "p:", 0.01, min=0.01, max=1, step=0.1),
            "Functia de masa",
            plotOutput("ex_1_geom_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_geom_plot2", height = "290px"),
            "Media",
            "Dispersia"
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
            "Media",
            "Dispersia"
          ),
          box(
            title="Poisson", status="primary", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_pois_l", "lambda:", 0, min=0),
            "Functia de masa",
            plotOutput("ex_1_pois_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_pois_plot2", height = "290px"),
            "Media",
            "Dispersia"
          ),
          box(
            title="Uniforma", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_unif_a", "a:", 0),
            numericInput("ex_1_unif_b", "b:", 1),
            "Functia de densitate",
            plotOutput("ex_1_unif_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_unif_plot2", height = "290px"),
            "Media",
            "Dispersia"
          ),
          box(
            title="Exponentiala", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_exp_l", "lambda:", 0, min=0),
            "Functia de densitate",
            plotOutput("ex_1_exp_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_exp_plot2", height = "290px"),
            "Media",
            "Dispersia"
          ),
          box(
            title="Normala", status="warning", solidHeader=TRUE, collapsible=TRUE,
            numericInput("ex_1_norm_m", "Media:", 1),
            numericInput("ex_1_norm_d", "Dispersia:", 1, min=0.001),
            "Functia de densitate",
            plotOutput("ex_1_norm_plot1", height = "290px"),
            "Functia de repartitie",
            plotOutput("ex_1_norm_plot2", height = "290px"),
            "Media",
            "Dispersia"
          )
        )
      ),
      tabItem(
        tabName = "ex5",
        titlePanel("Placeholder")
      )
    )
  )
)

server <- function(input, output) {
  
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
        plot(x, y, type='l')
    })
    
    output$ex_1_bern_plot2 <- renderPlot({
      p <- input$ex_1_bern_p
      X <- RV(c(0,1), c(1-p, p))
      x <- seq(-5, 5, 0.001)
      y <- sapply(x, function(nr) {
        return(P(X <= nr))
      })
      plot(x, y, type='l')
    })
    
    # Binom
    output$ex_1_binom_plot1 <- renderPlot({
      p <- input$ex_1_binom_p
      n <- input$ex_1_binom_n
      
      x <- seq(0, n+2, 0.1)
      plot(x, dbinom(x,n,p), type='l')
    })
    
    output$ex_1_binom_plot2 <- renderPlot({
      p <- input$ex_1_binom_p
      n <- input$ex_1_binom_n
      
      x <- seq(0, n+2,0.001)
      plot(x,pbinom(x,n,p), type='l')
    })
    
    # Geom
    output$ex_1_geom_plot1 <- renderPlot({
      p <- input$ex_1_geom_p
      x <- seq(0, 6, 0.1)
      plot(x, dgeom(x,p), type='l')
    })
    
    output$ex_1_geom_plot2 <- renderPlot({
      p <- input$ex_1_geom_p
      x <- seq(0, 6,0.001)
      plot(x,pgeom(x,p), type='l')
    })
    
    # Hgeom
    output$ex_1_hgeom_plot1 <- renderPlot({
      ### TODO: CHECK IF CORRECT
      n <- input$ex_1_hgeom_n
      m <- input$ex_1_hgeom_m
      k <- input$ex_1_hgeom_k
      
      x <- seq(0, k+1, 1)
      plot(x, dhyper(x,m,n,k), type='l')
    })
    
    output$ex_1_hgeom_plot2 <- renderPlot({
      n <- input$ex_1_hgeom_n
      m <- input$ex_1_hgeom_m
      k <- input$ex_1_hgeom_k
      x <- seq(0, k+1,0.001)
      plot(x,phyper(x,m,n,k), type='l')
    })
    
    # Pois
    output$ex_1_pois_plot1 <- renderPlot({
      l <- input$ex_1_pois_l
      x <- seq(0, 6, 0.1)
      plot(x, dpois(x,l), type='l')
    })
    
    output$ex_1_pois_plot2 <- renderPlot({
      l <- input$ex_1_pois_l
      x <- seq(0, 6,0.001)
      plot(x,ppois(x,l), type='l')
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
}

# Run the application 
shinyApp(ui = ui, server = server)

