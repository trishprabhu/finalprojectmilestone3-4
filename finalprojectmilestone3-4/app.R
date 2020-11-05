# Download relevant libraries, including the sentimentr library, so I can
# complete sentiment analysis!

library(shiny)
library(readr)
library(sentimentr)
library(tidyverse)
library(ggthemes)

source('graph_code.R', local = TRUE)

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
    tabPanel("Tweet Analysis",
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

# The sidebars were great spots to both 1) provide some context around the
# graphics, and 2) align/style the page so that the graphs were aesthetically 
# appealing.

                    sidebarPanel(
                        p("Analysis: Here, I visualize the distributions
                        of Trump and Clinton's Tweets' sentiment scores (above). 
                        On average, they are both relatively neutral on Twitter, 
                        but it's clear: Trump's Tweets see much more variation
                        in sentiment; by comparison, Clinton rarely reaches the 
                        most extreme sentiment scores (1 and -1).")
                    ),
                    mainPanel(
                        plotOutput(outputId = "hillPlot"),
            
                        plotOutput(outputId = "donPlot")),
                   sidebarPanel(
                   p("Analysis: Here, I look at Donald Trump's daily approval
                     ratings and Twitter sentiment scores (the average sentiment
                     of his Tweets on a given day) over a 2 week period -- 
                     09/30/20 - 10/13/20. Interestingly, Trump's approval 
                     ratings and sentiment scores seem to be weakly negatively
                     correlated (as his approval rating increases, he becomes
                     increasingly negative on Twitter -- perhaps he feels
                     emboldened and vindicated?). One must be cautious in
                     drawing any conclusions, though -- not only is the
                     relationship relatively weak, this is also a relatively 
                     short period of time; a longer period -- with more 
                     datapoints -- would likely be more telling. (Accordingly,
                     that is something I'm aiming to work on for my next 
                     Milestone!)")
                   ),
                   mainPanel(
                       plotOutput(outputId = "approvalSentiment")))),
    tabPanel("Discussion",
             titlePanel("About The Data"),
             p("11/06 Status Report: Building off of my work from last week,
               this week, I began to really wrangle/work with my data; 
               specifically, I created a graph visualizing how, over a 2-week
               period, the sentiment in Donald Trump's Tweets changed relative
               to his approval rating (e.g. as he gets more popular, does he
               become more positive online?). This was a great exercise in
               learning how to troubleshoot/work with data that wasn't in
               the format I needed it to be -- I made extensive use of the
               lubridate, sentimentr, and dplyr libraries; I also brought in
               for loops! With regards to the data in use: I'm still using
               Donald Trump's Tweets, from 07/13/20 to 10/13/20 (source: the
               Trump Twitter Archive -- it's got everything I need, so I
               figured there was no need to use Twitter's API), as well as the
               dataset documenting Trump's approval ratings over the course
               his presidency (both csvs are still in the Dropbox link below!). 
               Looking ahead, for my next Milestone, I'm hoping to pivot a bit
               from Donald Trump's Twitter, and consider his language in other
               spaces (e.g. his speeches). By looking at the speech level 
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
             p("Hello! This is a project exploring Donald Trump's language,
               and what it can tell us about his Presidency, politics,
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
            labs(x = "Sentiment Score",
                 y = "Count",
                 subtitle = "Overall, Hillary is very neutral in her Tweets",
                 title = "Sentiment Expressed In Clinton's Tweets",
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
              labs(x = "Sentiment Score",
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
    
    output$approvalSentiment <- renderPlot({
        
# I didn't know this, but because "graph_code.R" is in the same folder as this 
# app, I can use objects/functions defined in that R script here! Super handy.
        
        finalgraphtib %>%
            ggplot(aes(x = approval_ratings, y = meanofmeans)) +
            geom_point() +
            
# I know that the lines below surpasses the 80 character limit, but cutting them
# off was not aesthetically appealing on my graph. Apologies!
            
            labs(title = "Trump's daily approval ratings and sentiment scores on Twitter, 09/30 - 10/13",
                 subtitle = "Trump's approval ratings and sentiment scores seem to be weakly negatively correlated",
                 x = "Approval Rating",
                 y = "Sentiment Score",
                 caption = "Source: Trump Twitter Archive") +
            scale_x_continuous(labels = scales::percent_format()) +
            theme_bw()
        
    })
    
}

# Run the application:

shinyApp(ui = ui, server = server)
