library(shiny)

ui <- fluidPage(
  titlePanel("Estimation de Pi avec Monte Carlo"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_sim", "Nombre de simulations :", 
                  min = 100, max = 10000, value = 1000),
      actionButton("run_sim", "Lancer la simulation")
    ),
    
    mainPanel(
      plotOutput("pi_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run_sim, {
    X <- rep(0, input$n_sim)
    x <- runif(input$n_sim)
    y <- runif(input$n_sim)
    
    for (i in 1:input$n_sim) {
      if (x[i]^2 + y[i]^2 <= 1) {
        X[i] <- 1
      }
    }
    
    pi_MC <- 4 * mean(X)
    IC_pi <- c(pi_MC - 1.96 * 4 * sd(X) / sqrt(input$n_sim), 
               pi_MC + 1.96 * 4 * sd(X) / sqrt(input$n_sim))
    
    output$pi_plot <- renderPlot({
      plot(x, y, xlim = c(0, 1), ylim = c(0, 1),lwd=2, col = ifelse(X == 1, "blue", "red"), 
           pch = 20, main = paste("Estimation de Pi :", round(pi_MC, 4)))
      points(x[which(X == 1)], y[which(X == 1)], col = "blue", pch = 20)
      points(x[which(X == 0)], y[which(X == 0)], col = "red", pch = 20)
      theta <- seq(0, 2 * pi, length.out = 100)
      lines(cos(theta), sin(theta), col = "black",lwd=2)
      
    })
  })
}

shinyApp(ui = ui, server = server)
