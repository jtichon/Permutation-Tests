library(shiny)
library(tidyverse)

#### OUTSIDE OF APP ####

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

## Bike t - test
bikeSummary <- t.test(Time ~ Frame, data = bike)

## Children Data
children <- read.csv("www/childrenandlifespan.txt")

## Children t-test
childSummary <- t.test(Age ~ Children, data = children)


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  titlePanel(div(class = "title", "Permutation Tests")),
  
  # Create Lesson Tabs
  
  navbarPage(
    title = "Lesson",
    
#### START INTRO UI ####    
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

#### START OF TESTING UI ####
    
    tabPanel("Testing",
             
             # Statistical Question
             fluidRow( style = "margin-bottom: 50px;",
               column(6,
                        p(class = "sideHeader", "Statistical Question"),
                        p(class = "sideTalk",
                          "Let's remind ourselves what we are doing in hypothesis testing. There is some default/`null' idea about the shape of a population. e.g. `The bottles are filled following a normal distribution with an average of 710ml'. We, as statisticians, have an alternate idea of what is going on `The bottles are filled with less than 710ml on average.' Here we are trying to decide if two populations have equal means."
                        ),
                        p("Math symbols showing H0: mu1=mu2"),
                        p(class = "sideTalk", "This is the same thing as saying the difference in the means is 0."),
                        p("Math symbols showing H0: mu1-mu2=0"),
                        p(class = "sideTalk",
                          "So we are testing whether the true difference in means is 0 or if it's different somehow."
                        )
                      ),
               
               
               column(6,
                      img(src="idea.png")
                      )
               
             ),
             
             # Hypothesis Test
             fluidRow( style = "margin-bottom: 50px;",
               
               column(6,
                      p(class = "sideHeader", "Collect Data"),
                      p(class = "sideTalk",
                        "After we have made our hypothesis, we go out and collect data that is representative sample from each population. Our observations should be independent and ideally come from random samples though that is not always practical.")
                      ),
               
               column(6,
                      img(src="data.png")
                      )
  
             ),
             
             # Shufflings
             fluidRow( 
               
               column(6, 
                      p(class = "sideHeader", "Shufflings"),
                      p(class = "sideTalk", 
                        "Next we will take our data, see what happened in our sample, and see how unusual that result would be against the null model we are trying to 'counter'. If our real life data falls in the center of the model of the population under H0, we will fail to reject. If our real life data is out in the tails of the model and would rarely happen under the null model (usually in the out 5% of the tails), then we conclude that this is an unusual event. As what we saw in real life would rarely happen when H0 is true, that likely means we have good evidence it is not and we should reject it."),
                      p(class = "sideTalk",
                        "Here, our shuffling represents a model under H0. When we shuffle, we are saying that the groupings don't matter because both groups come from populations with the same mean. If we shuffle many times and keep track of all the differences in means, this will provide us with a picture of population of all differences under H0, the world where shuffling is of no consequence.")
                      ),
               
               column(6,
                      img(src="model.png")
                      )
             
             )
             
             # End Tab Testing Panel
             ),
 
#### START OF BIKE UI ####            
             tabPanel(
               "Bikes",
               
               #Bike Story Panel
               div(class = "story",
                   
                   p("Jerry Groves, a medical research, did a study using data on himself, published in the", em("British Medical Journal"), "(Groves, Jerry. (2010) ``Bike weight and commuting time: randomised trial''.", em("British Medical Journal"), "Vol. 341). He regularly cycled 27 miles to work and wanted to test if the choice of material for his bike frame would affect the commute times. He had two different frames for his bike: a lighter 20.90lb carbon frame and a heavier 29.75lb steel frame. Each day, for 56 days, he would flip a coin before leaving for work and use one frame if it was heads and the other if it was tails.")
                   ),
               
               # Row for data show buttons
               
               fluidRow(
                 column(4, offset = 4,
                   
                   actionButton("showBike", "Show the data"),
                   actionButton("hideBike", "Hide the data")
                   
                 )
                 
               ),
               
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
                 
               ),
               
              ),
            
            p(class = "preamble", "Now let's use simulation to see how many times, in a random shuffling of the groups, would be get means further apart than 107.82-108.34 = -0.52 in the negative direction OR further apart than 0.52 in the positive direction."),
            
            # Simulation for bike data
            fluidRow(
              p("Let's", actionButton("simulateBike", "simulate"), "5,000 shuffles and make a histogram of all the group differences we observe. (", span(class = "aside", "Note this may take half a minute to load"),")")
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
            
             div(class = "reflect",
                 p(
                   span(class = "question", "QUESTION:"), "How does this proportion related to hypothesis testing?"
                 ) ,

                # Reflective Answers for Bike P-value
                 fluidRow(
            
                   column(2,
                          div(class = "button", 
                              actionButton("reflectBikePvalue", "Reflect"))
                   ),
            
                   column(10,
                          htmlOutput("answerBikePvalue")
                  )
            
                 ),
                
                # T-test output for bike
                fluidRow(
                  
                  column(2,
                  ),
                  
                  column(10,
                         verbatimTextOutput("bikeTest")
                  )
                  
                )
            
             )
               
               
               #End Tab Bikes Panel
             ),
 
#### START OF CHILDREN UI ####

             tabPanel("Children",
                      #Child Story Panel
                      div(class = "story",
                          
                          p("There was a group of students at Cal Poly university that wanted to know if men with kids tended to live longer than men without kids. They took a random sample of men from the obituaries page in the", em("San Luis Obispo Tribune"), "webpage from June and November 2012. They noted the age of death of every male obituary and whether they were listed as having had kids or not.")
                      ),
                      
                      # Row for data show buttons
                      
                      fluidRow(
                        column(4, offset = 4,
                               
                               actionButton("showChild", "Show the data"),
                               actionButton("hideChild", "Hide the data")
                               
                        )
                        
                      ),
                      
                      # Row for text of Child Shuffle
                      
                      fluidRow(column(5,
                                      p(
                                        "Here is a boxplot of the data:"
                                      )),
                               
                               column(2,),
                               
                               column(
                                 5,
                                 span(style = "font-size: 20px; text-align: center;",
                                      textOutput("childText"))
                               )),
                      
                      # Row for plot UI of child shuffle
                      fluidRow(
                        column(5,
                               plotOutput("child")),
                        
                        column(
                          2,
                          p("Try to"),
                          
                          div(class = "button", actionButton("shuffleChild", "shuffle")),
                          
                          p("a few times.")
                        ),
                        
                        column(5,
                               plotOutput("childShuffle"))
                      ),
                      
                      div(class = "reflect", 
                          p(
                            span(class = "question", "QUESTION:"), "Does it appear to us that the age of death is the same for men with and without children?"
                          ), 
                          
                          # Reflective Answers for Child
                          fluidRow(
                            
                            column(2,
                                   div(class = "button", actionButton("reflectChild", "Reflect"))
                            ),
                            
                            column(10,
                                   htmlOutput("answerChild")
                            )
                            
                          ),
                          
                      ),
                      
                      p(class = "preamble", "Now let's use simulation to see how many times, in a random shuffling of the groups, would we get means further apart than 78.43 - 63.90  = 14.53 in the positive directiohn OR further apart than -14.53 in the negative direction."),
                      
                      # Simulation for child data
                      fluidRow(
                        p("Let's", actionButton("simulateChild", "simulate"), "5,000 shuffles and make a histogram of all the group differences we observe. (", span(class = "aside", "Note this may take half a minute to load"),")")
                      ),
                      
                      fluidRow(
                        
                        #Histogram of simulation
                        column(5,
                               plotOutput("childSimulate")
                        ),
                        
                        # Explanation of simulation
                        column(5, 
                               div(style = "font-size: large; font-weight: bold", textOutput("childQuestion"))
                        )
                      ),
                      
                      div(class = "reflect",
                          p(
                            span(class = "question", "QUESTION:"), "How does this proportion related to hypothesis testing?"
                          ) ,
                          
                          # Reflective Answers for Child P-value
                          fluidRow(
                            
                            column(2,
                                   div(class = "button", 
                                       actionButton("reflectChildPvalue", "Reflect"))
                            ),
                            
                            column(10,
                                   htmlOutput("answerChildPvalue")
                            )
                            
                          ),
                          
                          # T-test output for bike
                          fluidRow(
                            
                            column(2,
                            ),
                            
                            column(10,
                                   verbatimTextOutput("childTest")
                            )
                            
                          ),
                          
                      #),
                          
                    

                      # Reflect on natural answer for children
                      #div(
                        #class = "reflect",
                      fluidRow(  
                      p(style = "padding-top: 1cm;",
                          span(class = "question", "QUESTION:"),
                          "Without trying to justify socio-economic reasoning, what common sense solution would tell you this conclusion on the difference in means must be true?", em("Hint: Look at the boxplots of the sample")),
                      ),
                        
                        
                        # Reflective Answers for Child Answer
                        fluidRow(
                          column(2,
                                        div(
                                          class = "button",
                                          actionButton("reflectChildAnswer", "Reflect")
                                        )
                                        ),
                                 
                                 column(10,
                                        htmlOutput(
                                          "answerChildAnswer"
                                        )
                                        )
                                 ),
                        
                      )
                      
                    
                      
                      # End Tab Children Panel
                      )
                      
            #End Nav Bar
             )
    )

    
    server <- function(input, output) {

#### BEGINNING OF INTRO SERVER ####
      
      # Create shuffle same output
      
      # Become different text
      observeEvent(input$shuffle1, {
        output$morediff <-
          renderText({
            "The means/medians become more different (further apart) than in the original sample!"
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
            "The means/medians become more similar (closer together) than in the original sample!"
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
#### END OF INTRO SERVER ####
      
#### BEGINNING OF TESTING SERVER ####
      
#### END OF TESTING SERVER ####

#### START BIKE SERVER ####   
      
      # Text for shuffled bikes
      
      observeEvent(input$shuffleBike, {
        output$bikeText <-
          renderText({
            "Here are what the shufflings look like:"
          })
      })
      
      # Data for bikes
      
      observeEvent(input$hideBike, {
        removeUI( selector = "#bikeCode")
        removeUI( selector = "#bikeTable")
      })
      
      observeEvent(input$showBike, {

        insertUI( selector = "#hideBike",
                  where = "afterEnd",
                  ui = htmlOutput("bikeCode")
                  )
        
        insertUI( selector = "#bikeCode",
                  where = "afterEnd",
                  ui = tableOutput("bikeTable")
                    )
        
      })

      output$bikeCode <- renderUI({
        code("read.csv(bike.txt)")
      })
      
      output$bikeTable <- renderTable({
        bike
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
            tags$li(paste("We can calculate that the mean of the steel frames is actually 107.81 and the mean of the carbon frames is actually 108.34 which are very close considering it is a difference of approximately half a minute over a 27 mile bike ride which likely included at least some traffic considering he is a medical researcher that likely works in a city." ))
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
       
       output$answerBikePvalue <- renderUI({
         tags$ol(
           tags$li("'Proportion of differences greater than 0.52 or less than -0.52' is really just a different wording for `proportion of observations as or more extreme than than we observed' which is our key wording for p-values."),
           tags$li("When we perform the shufflings, we are basically saying 'It doesn't matter what group we put these observations in because it's all the same.' This is like an assumption that the means of both groups are the same. Putting this together, we are seeing how often we get an observation as or more extreme than real life when the null hypothesis (both means are equal) is true."),
           tags$li("Look at the simulated graph of differences again. Notice that it is bell shaped and centered at 0. This should hint that the differences are (or are at least approximately) normal. We should be able to get an approximately equivalent result by running a t-test on the difference in sample means. Using a t-test because we have unknown sigmas."),
           tags$li("Looking at the R output for a t-test to see if the means of carbon and steel are equal (i.e. that the difference in means is 0), we see that the true p-value is similar to our simulated proportion. They will not match exactly but will be a reasonable approximation of one another. ", br(), code("t.test(Time ~ Frame, data = bike)")),
         )
         
       })
       
       # t-test for bike data
       
       output$bikeTest <- renderPrint({
         bikeSummary
       })
       

       
     })
###### END OF BIKE SERVER #########     

#### BEGINNING OF CHILDREN SERVER #####

     # Text for shuffled children
     
     observeEvent(input$shuffleChild, {
       output$childText <-
         renderText({
           "Here are what the shufflings look like:"
         })
     })
     
     # Data for children
     
     observeEvent(input$hideChild, {
       removeUI( selector = "#childCode")
       removeUI( selector = "#childTable")
     })
     
     observeEvent(input$showChild, {
       
       insertUI( selector = "#hideChild",
                 where = "afterEnd",
                 ui = htmlOutput("childCode")
       )
       
       insertUI( selector = "#childCode",
                 where = "afterEnd",
                 ui = tableOutput("childTable")
       )
       
     })
     
     output$childCode <- renderUI({
       code("read.csv(child.txt)")
     })
     
     output$childTable <- renderTable({
       children
     })
     
     # Plot for unshuffled children
     
     output$child <- renderPlot({
       ggplot(children, aes(x = Children, y = Age)) +
         geom_boxplot() +
         xlab("Children") +
         ylab("Age at Death") +
         theme(
           legend.position = "none",
           plot.title = element_text(size = 20, face = "bold"),
           axis.title.x = element_text(size = 16),
           axis.title.y = element_text(size = 16),
           axis.text.x = element_text(size = 12),
           axis.text.y = element_text(size = 12)
         )
     })
     
     # Plot for Shuffled Children
     observeEvent(input$shuffleChild, {
       output$childShuffle <- renderPlot({
         # Shuffle data and create boxplots
         childshuffledf <- mix(children)
         ggplot(childshuffledf, aes(x = group, y = y)) +
           geom_boxplot() +
           xlab("Children") +
           ylab("Age at Death") +
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
     
     # Reflective Answers for Children
     
     observeEvent(input$reflectChild,{
       
       output$answerChild <- renderUI({
         tags$ol(
           tags$li("We notice that the center lines in the boxplots are not close together. Though one of the boxplots is not symmetric, so the median and mean are not the same, the skewness would suggest that mean for men without children is even lower than the median. This suggests the means are even further apart than the medians."),
           tags$li("When we shuffle the boxplots, we notice the medians (and hence the means), tend to get closer together than in the original sample nearly every time. Samples whose means become more similar when shuffled, is a good indication that the means are in fact statistically different from one another."),
           tags$li("We can calculate that the mean age of death for men with children is 78.43 and the mean age of death for men without children is 63.9 which are far apart ." )
         )
       })
     })
     
     # Simulation histogram for bikes
     observeEvent(input$simulateChild, {
       
       childdf <- reactive({
         diffMix(children)
       })
       
       #### SIMULATION PLOT CHILDREN ####
       output$childSimulate <- renderPlot({
         
         ggplot(childdf(), aes(x = differences)) +
           geom_histogram(bins = 20, 
                          color = "black", 
                          fill = "lightgreen") +
           geom_segment(x = -14.53, y = 1200, xend = -14.53, yend = 0, 
                        colour = "red", arrow = arrow()) +
           geom_segment(x = 14.53, y = 1200, xend = 14.53, yend = 0, 
                        colour = "red", arrow = arrow()) +
           geom_text(x = -14, y = 650, label = "-14.53", 
                     colour = "red", size = 7, hjust = 0) +
           geom_text(x = 14, y = 650, label = "14.53", 
                     hjust = 1, colour = "red", size = 7) +
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
       
       propChild <<- 
         mean(childdf()$differences < -14.53)+ mean(childdf()$differences > 14.53)
       
       
       output$childQuestion <- renderText({"How many times did we observe a difference in sample means less than -14.53 or greater than 14.53?"})
       
       # On click of simulate, remake answer button and clear text
       removeUI( selector = "#answerPropChild")
       removeUI( selector = "#childAnswer")
       removeUI( selector = "#childProportion")
       
       insertUI( selector = "#childQuestion",
                 where = "afterEnd",
                 ui = actionButton("answerPropChild", "Answer")
       )
       
     })
     
     
     # Children Conclusion
     
     # Create conclusion text on click of answer
     observeEvent(input$answerPropChild, {
       output$childProportion <- renderPrint({
         propChild
       })
       
       
       
       insertUI( selector = "#answerPropChild",
                 where = "afterEnd",
                 ui = verbatimTextOutput("childProportion")
       )
       
       output$childAnswer <-
         renderText({
           " is the proportion of differences greater than 14.53 or less than -14.53. i.e. This is the number of times we observe a sample difference in the shufflings as or more extreme than in the real life observed sample data. Nearly always, shuffling results in groups whose means become more similar, which is consistent with the difference in means being statistically significant."
         })
       
       insertUI(selector = "#childProportion",
                where = "afterEnd",
                ui = div(style = "font-size: medium; font-weight: normal", textOutput("childAnswer"))
       )
       
     })
     
     # Children Pvalue Reflection
     
     observeEvent(input$reflectChildPvalue,{
       
       output$answerChildPvalue <- renderUI({
         tags$ol(
           tags$li("Look at the simulated graph of differences again. Notice that it is still bell shaped and centered at 0, like for the bikes. Again, shuffling is stating a belief that the groups don't matter (i.e. the differences are the same). This is a simulated distribution of differences if that fact (the null hypothesis) were true."),
           tags$li("As the differences are (or are at least approximately) normal, we should be able to get an approximately equivalent result by running a t-test on the difference in sample means. Using a t-test because we have unknown sigmas."),
           tags$li("Looking at the R output for a t-test to see if the mean age of death of men with children and without children are equal (i.e. that the difference in means is 0), we see that the true p-value is similar to our simulated proportion. They will not match exactly but will be a reasonable approximation of one another. ", br(), code("t.test(Age ~ Children, data = children)")),
         )
         
       })
       
       # t-test for child data
       
       output$childTest <- renderPrint({
         childSummary
       })
     })
       
       # Answer to reflection question about why children happens
       
       observeEvent(input$reflectChildAnswer,{
         output$answerChildAnswer <- renderUI({
           p(style = "font-size: large", "Notice the boxplots for the original dataset. The ages for men with children is relatively symmetric and the ages for men without children is right-skewed. This makes sense as, to have children, you need to not die young. Having a child means you have lived long enough to have one which will, by common sense, raise the average age at which men with children die.")
         })
       })
       
     
#### END OF CHILDREN SERVER ####

    }
    
    shinyApp(ui = ui, server = server)
    