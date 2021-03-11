library(shiny)
library(tidyverse)

## Make vectors for initial demo
set.seed(23)
v1same <- rnorm(30, 50, 5)
v2same <- v1same
samedf <- data.frame(v = c(v1same, v2same), num = c(rep("1", length(v1same)), 
                                                    rep("2", length(v2same))))

set.seed(24)
v1diff <- rnorm(30, 60, 5)

set.seed(25)
v2diff <- rnorm(30, 40, 5)

diffdf <- data.frame(v = c(v1diff, v2diff), num = c(rep("1", length(v1diff)), 
                                                    rep("2", length(v2diff))))

## Import mixing functions
source("www/shuffle.R")

## Put shuffle function here

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  titlePanel(
    div(class = "title", "Permutation Tests")
  ),
  
  # Same group UI text
  fluidRow(
    column(5,
           p(
             "Consider two groups that are completely identical:"
           ),
           
          
           ),
  
    column(2,

           
           ),
  
    column(5,
           
           span(style = "font-size: 20px; text-align: center;", textOutput("morediff")),
           
           )
  ),
  
  # Same plot UI
  
  fluidRow(
    column(5,
           plotOutput("same")
           ),
    
    column(2,
           
           p(
             "What happens when we"
           ),
           
           div(class = "button", actionButton("shuffle1", "shuffle")),
           
           p(
             "them?"
           )
           ),
    
    column(5,
           plotOutput("sameShuffle")
           )
  ),
  

  hr(),
  
  # Different Group  text UI
  
  fluidRow(
    column(5,
           p(
             "Consider two groups that are different from populations with different means:"
           )
           
    ),
    
    column(2,
    ),
    
    column(5,
           
           span(style = "font-size: 20px; text-align: center;", textOutput("moresame")),
           
          
    )
  ),
  
  #Different Group plot UI
  
  fluidRow(
    
    column(5,
           plotOutput("diff")
           ),
    
    column(2,
           p(
             "What happens when we"
           ),
           
           div(class = "button", actionButton("shuffle2", "shuffle")),
           
           p(
             "them?"
           )
           ),
    
    column(5,
           plotOutput("diffShuffle")
           )
  )
  
)

server <- function(input, output){
  
  # Create shuffle same output
  
  # Become different text
  observeEvent(input$shuffle1,{output$morediff <-  
                                   renderText({"They become more different!"})
                                   })
  
  # Same non-shuffled plot
  output$same <- renderPlot({
    ggplot(samedf, aes(x = num, y = v)) +
      geom_boxplot()
  })
  
  # Same shuffle plot
  
  observeEvent(input$shuffle1, {
    output$sameShuffle <- renderPlot({
    
    # Shuffle data and create boxplots
    sameshuffledf <- mix(samedf)
    ggplot(sameshuffledf, aes(x = num, y = v)) +
      geom_boxplot()
  })
  })
  
  # Create shuffle different output
  
  # Become same text
  observeEvent(input$shuffle2,{output$moresame <-  
    renderText({"They become more similar!"})
  }) 
  
  # Different Shuffle Plot
  output$diff <- renderPlot({
    ggplot(diffdf, aes(x = num, y = v)) +
      geom_boxplot()
  })
  
  # Shuffle data and create boxplots
  observeEvent(input$shuffle2, {
    output$diffShuffle <- renderPlot({
      
      # Shuffle data and create boxplots
      diffshuffledf <- mix(diffdf)
      ggplot(diffshuffledf, aes(x = num, y = v)) +
        geom_boxplot()
    })
  })
}

shinyApp(ui = ui, server = server)
  
