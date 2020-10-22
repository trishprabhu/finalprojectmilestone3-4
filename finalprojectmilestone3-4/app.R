# Download relevant libraries, including the sentimentr library, so I can
# complete sentiment analysis!

library(shiny)
library(readr)
library(sentimentr)
library(tidyverse)
library(ggthemes)

trumptweets <- read_csv("Trump_tweets (1).csv")
summary(trumptweets)

hillarytweets <- read_csv("hillarytweets.csv")
summary(hillarytweets)

# Rather than calculate sentiment scores for all of the Tweets (thousands of
# observations, which would substantially slow things down, I took a subset
# of observations).

trump_sentiment_scores <- sentiment(trumptweets$text[1:100])
hillary_sentiment_scores <- sentiment(hillarytweets$text[1:100])

# UI definition:

ui <- navbarPage(
    "Trisha's Final Project Milestones",
    tabPanel("Tweet Sentiment Analysis",
             fluidPage(
                 titlePanel("Sentiment Analysis"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "dataset",
                                     label = "Choose a Twitter account:",
                                     choices = c("Hillary Clinton", 
                                                 "Donald Trump")),
                         numericInput(inputId = "obs",
                                      label = "Number of observations to view:",
                                      value = 10),
                         
# Originally, I just had a numericInput() box; at Dan's suggestion, I added a
# slider, so folks who visit my Shiny App can more easily look at the desired
# number of observations.
                         
                         sliderInput("obs", 
                         "Slide to the number of observations to view:",
                         min = 0, max = 300, value = 30
                             )),
                     mainPanel(
                         verbatimTextOutput("summary"),
                         tableOutput("view"),
                     )),

# This sidebar was a great spot to both 1) provide some context around the
# histogram, and align/style the page so that the graphs were aesthetically 
# appealing.

                    sidebarPanel(
                        p("Analysis: Here, I visualize the sentiment 
                        distributions (above) of Trump and Clinton's Tweets. 
                        On average, they are both relatively neutral on Twitter, 
                        but it's clear: Trump's Tweets see much more variation
                        in sentiment; by comparison, Clinton never reaches the 
                        most extreme sentiment scores (1 and -1).")
                    ),
                    mainPanel(
                        plotOutput(outputId = "hillPlot"),
                        
                        plotOutput(outputId = "donPlot")))),
    tabPanel("Discussion",
             titlePanel("About The Data"),
             p("09/23 Status Report: Building off of my work from last week,
               this week, I visualized (via 2 histograms) the distributions of 
               Trump and Clinton's Tweets' sentiment scores. I also
               created a mini-panel with some brief analysis. This was a good
               exercise in using ggplot() in Shiny; I feel much more 
               comfortable rendering plots now! With regards to my data, I'm 
               using the same data -- Donald Trump's Tweets, from 07/13/20 to 
               10/13/20, and Hillary Clinton's Tweets, from 08/03/16 to 
               11/03/16, both derived from The Trump Twitter Archive --
               but my goal is to pull data from Twitter's API (my developer 
               account was approved!) for the next milestone. Also on the
               data front, I found a new dataset -- Trump's approval rating
               over the course of his presidency -- which I plan to analyze
               in conjunction with how his sentiment on Twitter has evolved
               during the same period (this dataset is now in the Dropbox link 
               below). Looking ahead (as I discussed in Recitation this week!), 
               I'm planning to pivot, and further explore Donald Trump's 
               language off of Twitter -- by looking at the speech level 
               associated with his remarks, I hope to better  understand just 
               how often he stays on script during a speech."), 
             a("See the data currently in use by visiting this Dropbox link.",
               
# At Dan's suggestion, I uploaded my datasets (which were large, and making it
# impossible for me to commit my work to GitHub) to Dropbox. Also, Dan, 
# apologies -- the link below was too long to fit within the 80 character code 
# line limit!
               
               href = "https://www.dropbox.com/sh/5azksa5cvrsi9cs/AADvM-p9h8Sqf4oYzcgaMWXda?dl=0")
             ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! This is a project exploring Donald Trump's Tweets,
               and what they can tell us about his Presidency, politics,
               and the world at large."),
             h3("About Me"),
             p("My name is Trisha Prabhu and I study Government, on
             the Tech Science pathway. 
             You can reach me at trishaprabhu@college.harvard.edu."),
             a("Visit the GitHub repo for this project here.", 
    href = "https://github.com/trishprabhu/finalprojectmilestone3-4/tree/main")
    ))

# Define server logic:

server <- function(input, output) {
    
    datasetInput <- reactive({
        switch(input$dataset,
               
# As I learned, the values below correspond to the choices argument above -- 
# important to ensure that everything stays consistent, or your code will break
# (as mine did, until I figured this out)!
               
               "Hillary Clinton" = hillary_sentiment_scores,
               "Donald Trump" = trump_sentiment_scores)
    })
    
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
    hillsentimentmean <- mean(hillary_sentiment_scores$sentiment)
    
    output$hillPlot <- renderPlot({
        
        hillary_sentiment_scores %>%
            ggplot(aes(x = sentiment)) +
            geom_histogram(bins = 10, color = "white") +
            labs(x = "Sentiment",
                 y = "Count",
                 subtitle = "Overall, Hillary is very neutral in her Tweets",
                 title = "Sentiment Expressed In Hillary's Tweets",
                 caption = "Source: Trump Twitter Archive") +
            
# I thought that explicitly graphing the mean of both Trump and Clinton's 
# sentiment scores could help viewers better visualize the distribution overall
# (I also thought it was interesting that, on average, they are both very
# neutral -- likely a result of Trump's more positive Tweets "canceling out"
# his more negative Tweets).
            
            geom_vline(xintercept = hillsentimentmean, 
                       linetype = "dashed") +
            theme_bw()
        
    })
    
    donsentimentmean <- mean(trump_sentiment_scores$sentiment)
    
    output$donPlot <- renderPlot({
        
        trump_sentiment_scores %>%
            ggplot(aes(x = sentiment)) +
              geom_histogram(bins = 10, color = "white") +
              labs(x = "Sentiment",
                   y = "Count",
                   
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my graph. Apologies!
                   
                   subtitle = "On average, Trump is too, but with much more variation -- both positive and negative",
                   title = "Sentiment Expressed In Trump's Tweets",
                   caption = "Source: Trump Twitter Archive") +
            geom_vline(xintercept = donsentimentmean, 
                       linetype = "dashed",
                       label = "0.01336323") +
            theme_bw()
        
    })
    
}

# Run the application:

shinyApp(ui = ui, server = server)
