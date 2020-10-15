library(shiny)
library(readr)
library(sentimentr)

# Read in Trump Twitter data:

trumptweets <- read_csv("finalprojectmilestone3-4/data/Trump_tweets (1).csv")
summary(trumptweets)

# Read in Hillary Twitter data:

hillarytweets <- read_csv("finalprojectmilestone3-4/data/hillarytweets.csv")
summary(hillarytweets)

# Sentiment values:

trump_sentiment_scores <- sentiment(trumptweets$text[1:100])
hillary_sentiment_scores <- sentiment(hillarytweets$text[1:100])

# Define UI:

ui <- navbarPage(
    "Trisha's Final Project Milestone #4",
    tabPanel("Sentiment Analysis",
             fluidPage(
                 titlePanel("Sentiment Analysis"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "dataset",
                                     label = "Choose a dataset:",
                                     choices = c("Hillary", "Trump")),
                         numericInput(inputId = "obs",
                                      label = "Number of observations to view:",
                                      value = 10),
                         sliderInput("obs", "Number of observations  to view:",
                                         min = 0, max = 500, value = 50
                             )),
                     mainPanel(
                         verbatimTextOutput("summary"),
                         tableOutput("view"),
                         
                     )))),
    tabPanel("Discussion",
             titlePanel("About The Data"),
             p("Once my data is finalized, I'll complete this section.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! This is a project exploring Donald Trump's tweets,
               and what they can tell us about his Presidency, politics,
               and the world at large."),
             h3("About Me"),
             p("My name is Trisha Prabhu and I study Government, on
             the Tech Science pathway. 
             You can reach me at trishaprabhu@college.harvard.edu."),
             a("Visit the GitHub repo for this project here.", 
               href = "https://github.com/trishprabhu/finalprojectmilestones")
    ))

# Define server logic:

server <- function(input, output) {
    
    # Return the requested data-set ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Hillary" = hillary_sentiment_scores,
               "Trump" = trump_sentiment_scores)
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
    
    output$distPlot <- renderPlot({
        hist(rnorm(input$obs))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
