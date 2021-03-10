library(shiny)
library(tidyverse)

## Make vectors for initial demo
set.seed(23)
v1same <- rnorm(50, 5)
v2same <- v1same

set.seed(24)
v1diff <- rnorm(60, 5)

set.seed(25)
v2diff <- rnorm(40, 5)

## Put shuffle function here

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  titlePanel(
    div(class = "title", "Permutation Tests")
  ),
  
  fluidRow(
    column(5,
           p(
             "Consider two groups that are completely identical:"
           ),
           
           plotOutput("same"),
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
           
           p("The become more different!"),
           
           plotOutput("sameShuffle")
           )
  )
  
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)
  
