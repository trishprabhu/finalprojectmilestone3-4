# Download relevant libraries, including the sentimentr library, so I can
# complete sentiment analysis!

library(shiny)
library(readr)
library(sentimentr)

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
                sidebarPanel(
                    sliderInput(inputId = "binshillary",
                                label = "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 10),
                    sliderInput(inputId = "binstrump",
                                label = "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 10)),
                    mainPanel(
                        plotOutput(outputId = "hillPlot"),
                        plotOutput(outputId = "donPlot")))),
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
    
    output$hillPlot <- renderPlot({
        
        x <- hillary_sentiment_scores$sentiment
        binshillary <- seq(min(x), 
                           max(x), 
                           length.out = input$binshillary + 1)
        hist(x, breaks = binshillary, col = "#75AADB", border = "white",
             xlab = "Sentiment",
             main = "Histogram of Sentiment Expressed In Clinton's Tweets")
    })
    
    output$donPlot <- renderPlot({
        
        y <- trump_sentiment_scores$sentiment
        binstrump <- seq(min(y), 
                         max(y), 
                         length.out = input$binstrump + 1)
        hist(y, breaks = binstrump, col = "#75AADB", border = "white",
             xlab = "Sentiment",
             main = "Histogram of Sentiment Expressed In Trump's Tweets")
        
    })
    
}

# Run the application:

shinyApp(ui = ui, server = server)
