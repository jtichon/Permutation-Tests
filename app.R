library(shiny)
library(tidyverse)

## Make vectors for initial demo
set.seed(23)
v1same <- rnorm(30, 50, 5)
v2same <- v1same
samedf <-
  data.frame(v = c(v1same, v2same), num = c(rep("1", length(v1same)),
                                            rep("2", length(v2same))))

set.seed(24)
v1diff <- rnorm(30, 60, 5)

set.seed(25)
v2diff <- rnorm(30, 40, 5)

diffdf <-
  data.frame(v = c(v1diff, v2diff), num = c(rep("1", length(v1diff)),
                                            rep("2", length(v2diff))))

## Import mixing functions
source("www/shuffle.R")

## Bike Data
bike <- read.csv("www/biketime.txt")

## Children Data
children <- read.csv("www/childrenandlifespan.txt")

## Put shuffle function here

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  titlePanel(div(class = "title", "Permutation Tests")),
  
  # Create Lesson Tabs
  
  navbarPage(
    title = "Lesson",
    
    tabPanel(
      "Intro",
      
      # Same group UI text
      fluidRow(column(
        5,
        p("Consider two groups that are completely identical:"),
        
        
      ),
      
      column(2,),
      
      column(
        5,
        
        span(style = "font-size: 20px; text-align: center;", textOutput("morediff")),
        
      )),
      
      # Same plot UI
      
      fluidRow(
        column(5,
               plotOutput("same")),
        
        column(
          2,
          
          p("What happens when we"),
          
          div(class = "button", actionButton("shuffle1", "shuffle")),
          
          p("them?")
        ),
        
        column(5,
               plotOutput("sameShuffle"))
      ),
      
      
      hr(),
      
      # Different Group  text UI
      
      fluidRow(column(
        5,
        p(
          "Consider two groups that are different from populations with different means:"
        )
        
      ),
      
      column(2,),
      
      column(
        5,
        
        span(style = "font-size: 20px; text-align: center;", textOutput("moresame")),
        
        
      )),
      
      #Different Group plot UI
      
      fluidRow(
        column(5,
               plotOutput("diff")),
        
        column(
          2,
          p("What happens when we"),
          
          div(class = "button", actionButton("shuffle2", "shuffle")),
          
          p("them?")
        ),
        
        column(5,
               plotOutput("diffShuffle"))
      )
      
      #End Intro Tab
    ),
    
    tabPanel("Testing",
             
             # End Tab Testing Panel
             ),
             
             tabPanel(
               "Bikes",
               
               #Bike Story Panel
               div(class = "story",
                   
                   p("Text about story of bike data")),
               
               # Row for text of Bike Shuffle
               
               fluidRow(column(5,
                               p(
                                 "Here is a boxplot of the data:"
                               )),
                        
                        column(2,),
                        
                        column(
                          5,
                          span(style = "font-size: 20px; text-align: center;",
                               textOutput("bikeText"))
                        )),
               
               # Row for plot UI of bike shuffle
               fluidRow(
                 column(5,
                        plotOutput("bike")),
                 
                 column(
                   2,
                   p("Try to"),
                   
                   div(class = "button", actionButton("shuffleBike", "shuffle")),
                   
                   p("a few times.")
                 ),
                 
                 column(5,
                        plotOutput("bikeShuffle"))
               ),
             
            div(class = "reflect", 
               p(
                span(class = "question", "QUESTION:"), "Does it appear to us that commute times are the same for both the carbon and the steel frame?"
               ), 
               
               # Reflective Answers for Bike 
               fluidRow(
                 
                 column(2,
                        div(class = "button", actionButton("reflectBike", "Reflect"))
                        ),
                 
                 column(10,
                          htmlOutput("answerBike")
                        )
                 
               )
               
              ),
            
            p(class = "preamble", "Now let's use simulation to see how many times, in a random shuffling of the groups, would be get means further apart than 107.82-108.34 = -0.52 in the negative direction OR further apart than 0.52 in the positive direction."),
            
            # Simulation for bike data
            fluidRow(
              p("Let's", actionButton("simulateBike", "simulate"), "5,000 shuffles and make a histogram of all the group differences we observe. (", span(class = "aside", "Note this may take a few seconds to load"),")")
            ),
            
            fluidRow(
              
              #Histogram of simulation
              column(5,
                     plotOutput("bikeSimulate")
                     ),
              
              # Explanation of simulation
              column(5, 
                     #textOutput("anchor")
                     div(style = "font-size: large; font-weight: bold", textOutput("bikeQuestion"))
                     )
            ),
            
            # div(class = "reflect", 
            #     p(
            #       span(class = "question", "QUESTION:"), "How does this proportion related to hypothesis testing?"
            #     ) , 
                
                # Reflective Answers for Bike 
            #     fluidRow(
            #       
            #       column(2,
            #              div(class = "button", actionButton("reflectBikePvalue", "Reflect"))
            #       ),
            #       
            #       column(10,
            #              htmlOutput("answerBikePvalue")
            #       )
            #       
            #     )
            #     
            # )
               
               
               #End Tab Bikes Panel
             ),
             
             tabPanel("Children",
                      
                      # End Tab Children Panel
                      )
                      
            #End Nav Bar
             )
    )

    
    server <- function(input, output) {
      # Create shuffle same output
      
      # Become different text
      observeEvent(input$shuffle1, {
        output$morediff <-
          renderText({
            "They become more different!"
          })
      })
      
      # Same non-shuffled plot
      output$same <- renderPlot({
        ggplot(samedf, aes(x = num, y = v)) +
          geom_boxplot() +
          xlab("Group") +
          ylab(NULL) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      })
      
      # Same shuffle plot
      
      observeEvent(input$shuffle1, {
        output$sameShuffle <- renderPlot({
          # Shuffle data and create boxplots
          sameshuffledf <- mix(samedf)
          ggplot(sameshuffledf, aes(x = group, y = y)) +
            geom_boxplot() +
            xlab("Group") +
            ylab(NULL) +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 20, face = "bold"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
            )
        })
      })
      
      # Create shuffle different output
      
      # Become same text
      observeEvent(input$shuffle2, {
        output$moresame <-
          renderText({
            "They become more similar!"
          })
      })
      
      # Different Shuffle Plot
      output$diff <- renderPlot({
        ggplot(diffdf, aes(x = num, y = v)) +
          geom_boxplot() +
          xlab("Group") +
          ylab(NULL) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      })
      
      # Shuffle data and create boxplots
      observeEvent(input$shuffle2, {
        output$diffShuffle <- renderPlot({
          # Shuffle data and create boxplots
          diffshuffledf <- mix(diffdf)
          ggplot(diffshuffledf, aes(x = group, y = y)) +
            geom_boxplot() +
            xlab("Group") +
            ylab(NULL) +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 20, face = "bold"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
            )
        })
      })
      
      # Text for shuffled bikes
      
      observeEvent(input$shuffleBike, {
        output$bikeText <-
          renderText({
            "Here are what the shufflings look like:"
          })
      })
      
      # Plot for unshuffled bikes
      
      output$bike <- renderPlot({
        ggplot(bike, aes(x = Frame, y = Time)) +
          geom_boxplot() +
          xlab("Frame") +
          ylab("Time") +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      })
      
      # Plot for Shuffled Bikes
      observeEvent(input$shuffleBike, {
        output$bikeShuffle <- renderPlot({
          # Shuffle data and create boxplots
          bikeshuffledf <- mix(bike)
          ggplot(bikeshuffledf, aes(x = group, y = y)) +
            geom_boxplot() +
            xlab("Frame") +
            ylab("Time") +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 20, face = "bold"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
            )
        })
      })
      
      # Reflective Answers for Bike
      
     observeEvent(input$reflectBike,{

        output$answerBike <- renderUI({
          tags$ol(
            tags$li("We notice that the center lines in both boxplots are fairly close together. As the boxplots are relatively symmetric, the means should be approximately equal to the medians. This is a good indication the means are not significantly different."),
            tags$li("When we shuffle the boxplots, we notice that the medians (and hence means), tend to get further apart nearly every time. Samples that become more different when shuffled is a good indication that the means are similar."),
            tags$li(paste("We can calculate that the mean of the steel frames is actually 107.81 and the mean of the carbon frames is actually 108.34 which are very close considering the standard deviation of all bike times is 5.52. This is another good indication that a statistical test would conclude the means are equal." ))
             )
        })
      })
     
     # Simulation histogram for bikes
     observeEvent(input$simulateBike, {
       
       bikedf <- reactive({
         diffMix(bike)
       })
       
       output$bikeSimulate <- renderPlot({
         
         ggplot(bikedf(), aes(x = differences)) +
           geom_histogram(bins = 20, 
                          color = "black", 
                          fill = "lightgreen") +
           geom_segment(x = -0.52, y = 1200, xend = -0.52, yend = 0, 
                        colour = "red", arrow = arrow()) +
           geom_segment(x = 0.52, y = 1200, xend = 0.52, yend = 0, 
                        colour = "red", arrow = arrow()) +
           geom_text(x = -1, y = 650, label = "-0.52", 
                     colour = "red", size = 7, hjust = 1) +
           geom_text(x = 1, y = 650, label = "0.52", 
                     hjust = 0, colour = "red", size = 7) +
           theme_light() +
           labs(
                x = "Differences",
                y = "Frequency") +
           theme(
             legend.position = "none",
             plot.title = element_text(size = 20, face = "bold"),
             axis.title.x = element_text(size = 16),
             axis.title.y = element_text(size = 16),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12)
           )
       })
       
       propBike <<- 
         mean(bikedf()$differences < -0.52)+ mean(bikedf()$differences > 0.52)
       
       
       output$bikeQuestion <- renderText({"How many times did we observe a difference in sample means less than -0.52 or greater than 0.52?"})
       
       # On click of simulate, remake answer button and clear text
       removeUI( selector = "#answerPropBike")
       removeUI( selector = "#bikeAnswer")
       removeUI( selector = "#bikeProportion")

       insertUI( selector = "#bikeQuestion",
                 #selector = "#anchor",
                 where = "afterEnd",
                 ui = actionButton("answerPropBike", "answer")
                 )

    })
     
     
     # Bike Conclusion
     
     # Create conclusion text on click of answer
     observeEvent(input$answerPropBike, {
       output$bikeProportion <- renderPrint({
         propBike
       })
       
       
       
       insertUI( selector = "#answerPropBike",
                 #selector = "#anchor",
                 where = "afterEnd",
                 ui = verbatimTextOutput("bikeProportion")
       )
       
       output$bikeAnswer <-
         renderText({
             " is the proportion of differences greater than 0.52 or less than -0.52. i.e. This is the number of times we observe a sample difference in the shufflings as or more extreme than in the real life observed sample data. More often than not, shuffling results in groups whose means become more different, which is consistent with the difference in means being statistically insignificant."
         })
       
       insertUI(selector = "#bikeProportion",
                where = "afterEnd",
                ui = div(style = "font-size: medium; font-weight: normal", textOutput("bikeAnswer"))
     )
     
     })
     
     # Bike Pvalue Reflection
     
     observeEvent(input$reflectBikePvalue,{
       
       output$answerBikePValue <- renderUI({
         tags$ol(
           tags$li("Making a point here"),
           tags$li("Making another point here")
         )
       })
     })

    }
    
    shinyApp(ui = ui, server = server)
    