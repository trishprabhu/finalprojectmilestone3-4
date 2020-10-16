library(shiny)
library(readr)
library(sentimentr)

# Read in Donald Trump Twitter data:

trumptweets <- read_csv("Trump_tweets (1).csv")
summary(trumptweets)

# Read in Hillary Clinton Twitter data:

hillarytweets <- read_csv("hillarytweets.csv")
summary(hillarytweets)

# Sentiment values:

trump_sentiment_scores <- sentiment(trumptweets$text[1:100])
hillary_sentiment_scores <- sentiment(hillarytweets$text[1:100])

# Define UI:

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
                         sliderInput("obs", "Slide to the number of
                                     observations to view:",
                                         min = 0, max = 300, value = 30
                             )),
                     mainPanel(
                         verbatimTextOutput("summary"),
                         tableOutput("view"),
                     )))),
    tabPanel("Discussion",
             titlePanel("About The Data"),
             p("09/16 Status Report: The data used in this project currently 
             consists of two datasets -- Donald Trump's Tweets, from 07/13/20 to 
             10/13/20, and Hillary Clinton's Tweets, from 08/03/16 to 11/03/16 
             (both meant to represent either 3 months before their respective
             Presidential elections/3 months before the present date). I've
             currently run a sentiment analysis on a subset of observations;
             in the future, I plan to both 1) run additional analyses with
             regards to sentiment (e.g. how does sentiment vary with time
             (closer to the election)?) and 2) look at other elements of the
             Tweets, from their characteristics (does Donald Trump Retweet
             more, or write his own Tweets more?) to their correlation
             with other data of interest (such as Trump's approval rating).
             In doing this, I plan to focus in more exclusively on Donald
             Trump (as opposed to Hillary Clinton), and to pull/clean data
             from Twitter's API."), 
             a("See the data currently in use by visiting this Dropbox link.", 
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
    
    # Return the requested data-set ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Hillary Clinton" = hillary_sentiment_scores,
               "Donald Trump" = trump_sentiment_scores)
    })
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
