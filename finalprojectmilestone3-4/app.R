
# Things To Do:

# Include interactions in Model (partially complete; need to understand what
# it means -- interpret?).
# Include confidence intervals (done)!
# Create readability scores for each of the Tweets; do Tweet analysis (done!) 
# and add to model (done).
# Change first page to include Tweet and sentiment score; try to make it look
# cool (done)!
# Figure out how to get the desired # of digits in the gt table (done!).
# Figure out how to read in the other R scripts (email Dan).
# Figure out to make the variable names professional throughout the project
# (still need to do).
# Save subset of data/commit to GitHub (write.csv(objectname, "pathway.csv"))
# (still need to do; low priority).
# Solidify interpretation of histogram on "Tweet Analysis" page (still need
# to do).
# Create separate "Readability" page (unsure if going to do).
# Create a Word Cloud (done)!

# Download relevant libraries, including the sentimentr library, so I can
# complete sentiment analysis!

library(shiny)
library(readr)
library(sentimentr)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(colourpicker)
library(wordcloud2)
library(tm)

trumptweets <- read_csv("Trump_tweets (1).csv")
summary(trumptweets)

hillarytweets <- read_csv("hillarytweets.csv")
summary(hillarytweets)

# source("graph_code.R")

# Rather than calculate sentiment scores for all of the Tweets (thousands of
# observations, which would substantially slow things down, I took a subset
# of observations).

trump_sentiment_scores <- sentiment(trumptweets$text[1:100])
hillary_sentiment_scores <- sentiment(hillarytweets$text[1:100])

dataframe_options <-
  c("hillary_sentiment_scores",
    "trump_sentiment_scores")

# UI definition:

library(shinythemes)

ui <- navbarPage(
  "Trisha's Final Project Milestones",
  tabPanel("Tweet Analysis",
           fluidPage(theme = shinytheme("cerulean"),
                     titlePanel("Sentiment Analysis: A Glimpse"),
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
                      br(),
                      br(),
                      br(),
                      br(),
                      sidebarPanel(
                        numericInput("tweetread", 
                                     "Pick the Tweet you'd like to view:",
                                     value = 5
                        )),
                    mainPanel(
                      gt_output(outputId = "tweetread")
                    ),
                     # The sidebars were great spots to both 1) provide some context around the
                     # graphics, and 2) align/style the page so that the graphs were aesthetically 
                     # appealing.

                     sidebarPanel(
                       p("Here, I visualize the distributions
                        of Trump and Clinton's Tweets' sentiment scores. 
                        On average, they are both relatively neutral on Twitter, 
                        but it's clear: Trump's Tweets see much more variation
                        in sentiment; by comparison, Clinton rarely reaches the 
                        most extreme sentiment scores (1 and -1)."),
                       selectInput(inputId = "candidate", 
                                   label = "Choose a Twitter account:",
                                   choices = dataframe_options)),
            mainPanel(
                      br(),
                      br(),
                      br(),
                      br(),
                       plotOutput(outputId = "bothPlot"),
                       
                       sliderInput("bins", 
                                   "Set the number of bins:",
                                   min = 0, max = 50, value = 20
                       )))),
#                      plotOutput(outputId = "donPlot"),
                     
#                       sliderInput("binsdon", 
#                                   "Set the number of bins:",
#                                    min = 0, max = 50, value = 20
#                        )),
#                 mainPanel(
#                     tabsetPanel(type = "tab",
#                                tabPanel("pdf", 
#                                          tags$ifframe(style = "height:400px; 
#                                                                 width: 100%; 
#                                                                 scrolling = yes", 
#                                                       src = "finalgraph.pdf"))))
    tabPanel("Model",
             titlePanel("How/why does Trump's sentiment on Twitter change?"),
             sidebarPanel(
             titlePanel("Approval Rating"),
               p("Here, I look at Donald Trump's daily approval
                     ratings and Twitter sentiment scores (the average sentiment
                     of his Tweets on a given day) over a 1 month period -- 
                     09/12/20 - 10/13/20. As we'd expect, Trump's approval 
                     ratings and sentiment scores seem to be weakly positively
                     correlated (as his approval rating increases, he also
                     becomes more positive on Twitter -- perhaps as he becomes
                     more popular, it puts him in a sunny mood). One must be 
                     cautious in drawing any conclusions, though -- not only is 
                     the relationship relatively weak, this is also a relatively 
                     short period of time; a longer period (like 1 year) -- with 
                     more datapoints -- would likely be more telling."),
               p("In this graph, we visualize the posterior distributions for
                 Trump's daily Twitter sentiment score in 3 hypothetical 
                 universes: one in which he has a 30% approval rating, one
                 in which he has a 45% approval rating, and one in which he has
                 a 60% approval rating. The distributions reflect the linear
                 relationship we observed above -- the hypothetical Trump with a
                 60% approval rating has a posterior distribution for sentiment
                 scores that is skewed to the right (more positive). It's also 
                 clear that we have a much more precise estimate for the 
                 hypothetical Trump with a 45% approval rating, given the data; 
                 while, on average, the 30% and 60% approval rating scenarios 
                 are less and more positive, respectively, the distributions are 
                 rather wide, so we wouldn't be surprised if the Trump with a 
                 30% approval rating had a positive daily Twitter sentiment 
                 score."),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             titlePanel("Stock Market"),
             p("Here, I look at daily stock market opening/closing differences
               and Donald Trump's corresponding Twitter sentiment scores over a 
               1 month period (09/12 - 10/13). Interestingly, the S&P 500's 
               opening/closing differences and Trump's sentiment scores seem to 
               be very weakly negatively correlated -- indeed the regression results 
               (which you can view below, in the interactive table!) produce a 
               coefficient for difference which is very small/negative. Overall, then, 
               it seems that the stock market doesn't greatly influence 
               Donald Trump's sentiment on Twitter, and any influence is such 
               that as the difference becomes more positive (a higher closing 
               index relative to the opening index) Trump becomes a bit more 
               negative on Twitter (perhaps he feels vindicated?).
               While the relationship does seem to be very weak, we can still
               use this dependent variable as a control in our regression of
               Trump's sentiment scores on his approval ratings -- as we do 
              below."),
             br(),
             br(),
             br(),
             br(),
             titlePanel("Interactive Regression Results"),
             selectInput(inputId = "regressiontable",
                         label = "Choose a variable:",
                         choices = c("Approval Rating", 
                                     "Stock Market",
                                     "Interaction")),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             titlePanel("Readability"),
             p("Here, I look at the relationship between the readability of
               Donald Trump's Tweets and the sentiment of those Tweets.
               Interestingly, readability seems to have close to no relationship 
               with sentiment; regression results confirm this. The
               visualization does pull out another trend, however; by only
               displaying the text for those Tweets below a certain length
               of characters, it becomes clear that Trump tends to write
               shorter Tweets when those Tweets are more positive. Clearly,
               he doesn't like to brag!")),
             mainPanel(
               plotOutput(outputId = "approvalSentiment"),
               plotOutput(outputId = "approvalPosterior"),
               plotOutput(outputId = "stockSentiment"),
               br(),
               br(),
               gt_output(outputId = "regressiontable"),
               br(),
               br(),
               br(),
               br(),
               plotOutput(outputId = "readability"))
             ),
    tabPanel("Visualization",
             titlePanel("Word Cloud"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "source",
                   label = "Pick a candidate:",
                   choices = c(
                     "Hillary Clinton; 2016" = "hill16",
                     "Donald Trump; 2020" = "don20")
                 ),
                 numericInput("num", "Maximum number of words",
                              value = 100, min = 5),
                 colourInput("col", "Background color", value = "white")),
             mainPanel(wordcloud2Output("cloud")))),
    tabPanel("Discussion",
             titlePanel("About The Data"),
             p("11/13 Status Report: This week, I began building the
               Model portion of this project -- working off of the graph
               I built last week (that looked at Donald Trump's daily
               approval ratings/associated Twitter sentiment scores),
               I created a regression model (with additional observations) 
               to quantify/better understand that relationship. I also
               found some new data -- on stock market volatility (specifically,
               the S&P 500's daily opening/closing differences), which, after 
               extensive wrangling/cleaning, I incorporated into my model 
               (I also created a separate model regressing Trump's sentiment 
               scores on the stock data). In doing all of this, I created 2 new
               visualizations -- one looking at the posterior distributions
               for Trump's daily Twitter sentiment score in 3 hypothetical 
               universes (in each, he has different approval ratings), and
               a geom_point ggplot looking at how, over a 1-month
               period, the sentiment in Donald Trump's Tweets changed relative
               to the S&P 500's opening/closing differences (there was a
               very weak negative relationship). With regards to the data
               in use, I'm still using Donald Trump's Tweets, from 
               7/13/20 to 10/13/20 (source: the Trump Twitter Archive) and the
               FiveThirtyEight Trump approval rating data; I'm also now using 
               the CBOE Volatility Index's stock data. As always, all of this
               data is in the Dropbox link below. Looking ahead, my big
               goal for next week is to make everything in my app interactive,
               as opposed to static (as it currently is). Ideally, I'd like
               to combine a few of my visualizations by giving users the ability
               to choose which they'd like to visualize, dynamically; I also 
               want to create an interactive regression table, where users can 
               regress Donald Trump's Twitter sentiment scores on the dependent 
               variable of their choice."), 
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
    
    candidateInput <- reactive({
      switch(input$candidate,
            
             "hillary_sentiment_scores" = hillary_sentiment_scores,
             "trump_sentiment_scores" = trump_sentiment_scores)
    })
    
    output$summary <- renderPrint({
        dataset <- datasetInput()
        tib <- dataset %>%
          rename("Tweets" = "element_id",
                 "Sentence Number" = "sentence_id",
                 "Word Count" = "word_count",
                 "Sentiment" = "sentiment") # %>%
#        group_by(Tweets) %>%
#         summarize(sentimentmeans = mean(sentiment, na.rm = TRUE),
#                    .groups = "drop")
        summary(tib)
    })
    
    output$view <- renderTable({
        dataset <- datasetInput()
        nicetib <- dataset %>%
          rename("Tweets" = "element_id",
                 "Sentence Number" = "sentence_id",
                 "Word Count" = "word_count",
                 "Sentiment" = "sentiment") 
        
          head(nicetib, n = input$obs)
    })
    
#    hillsentimentmean <- mean(hillary_sentiment_scores$sentiment)
    
    output$bothPlot <- renderPlot({
      candidate <- candidateInput()
      candidate %>%
            ggplot(aes(x = sentiment)) +
            geom_histogram(bins = input$bins, 
                           color = "white",
                           fill = "dodgerblue") +
            labs(x = "Sentiment Score",
                 y = "Count",
                 subtitle = "Overall, Hillary is very neutral in her Tweets; Trump is too, but with more variation",
                 title = "Sentiment Expressed In Tweets",
                 caption = "Source: Trump Twitter Archive") +
            
# I thought that explicitly graphing the mean of both Trump and Clinton's 
# sentiment scores could help viewers better visualize the distribution overall
# (I also thought it was interesting that, on average, they are both very
# neutral -- likely a result of Trump's more positive Tweets "canceling out"
# his more negative Tweets).
            
            geom_vline(xintercept = mean(candidate$sentiment), 
                       linetype = "dashed") +
            theme_classic()
        
    })
    
#    donsentimentmean <- mean(trump_sentiment_scores$sentiment)
    
#    output$donPlot <- renderPlot({
        
#       trump_sentiment_scores %>%
#            ggplot(aes(x = sentiment)) +
#              geom_histogram(bins = input$binsdon, color = "white") +
#              labs(x = "Sentiment Score",
#                   y = "Count",
                   
# I know that the line below surpasses the 80 character limit, but cutting it
# off was not aesthetically appealing on my graph. Apologies!
                   
#                   subtitle = "On average, Trump is too, but with much more variation -- both positive and negative",
#                   title = "Sentiment Expressed In Trump's Tweets",
#                   caption = "Source: Trump Twitter Archive") +
#            geom_vline(xintercept = donsentimentmean, 
#                       linetype = "dashed",
#                       label = "0.01336323") +
#            theme_bw()
        
 #   })
    
        
#       output$newtabs <- renderUI({
#      
#       })
    
    
    output$approvalSentiment <- renderPlot({
        
# I didn't know this, but because "approvalgraph_code.R" is in the same folder 
# as this app, I can use objects/functions defined in that R script here! 
# Super handy. Update: unfortunately, I need to read the R script in to this
# App in order for the graph below to render in my published ShinyApp. Currently
# troubleshooting.
        
        finalgraphtib %>%
            ggplot(aes(x = approval_ratings, y = meanofmeans)) +
            geom_point() +
            geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
            
# I know that the lines below surpasses the 80 character limit, but cutting them
# off was not aesthetically appealing on my graph. Apologies!
            
            labs(title = "Trump's daily approval ratings and sentiment scores on Twitter, 09/12 - 10/13",
                 subtitle = "Trump's approval ratings and sentiment scores seem to be weakly positively correlated",
                 x = "Approval Rating",
                 y = "Sentiment Score",
                 caption = "Source: Trump Twitter Archive") +
            scale_x_continuous(labels = scales::percent_format()) +
            theme_bw()
        
    })
    
    output$approvalPosterior <- renderPlot({
      
      approvalratingdistribution <- pp %>% 
        rename(`30` = `1`) %>% 
        rename(`45` = `2`) %>% 
        rename(`60` = `3`) %>% 
        pivot_longer(cols = `30`:`60`,
                     names_to = "parameter",
                     values_to = "score") %>%
        ggplot(aes(x = score, fill = parameter)) +
        geom_histogram(aes(y = after_stat(count/sum(count))), 
                       alpha = 0.7,
                       bins = 100,
                       color = "white",
                       position = "identity") +
        labs(title = "Posterior Distributions for Sentiment Score",
             subtitle = "We have a much more precise estimate for the hypothetical Trump with a 45% approval rating, given the data",
             x = "Sentiment Score",
             y = "Proportion",
             caption = "Source: Trump Twitter Archive, FiveThirtyEight") +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_manual(name = "Approval Rating (%)",
                          values = c("dodgerblue", "salmon", "green")) +
        theme_bw()
      
      approvalratingdistribution
    
    })
    
    output$stockSentiment <- renderPlot({
      
      stockgraph <- finalstocktib %>%
        ggplot(aes(x = range, y = meanofmeans)) +
        geom_point() +
        geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
        
# I know that the lines below surpasses the 80 character limit, but cutting them
# off was not aesthetically appealing on my graph. Apologies!
        
        labs(title = "Stock opening/closing differences and Trump's daily sentiment scores on Twitter, 09/12 - 10/13",
             subtitle = "The S&P 500's opening/closing differences and Trump's sentiment scores seem to be very, very weakly negatively correlated",
             x = "Difference",
             y = "Sentiment Score",
             caption = "Source: Trump Twitter Archive; CBOE Volatility Index") +
        theme_bw()
      
      stockgraph
      
    })
    
    regressiontableInput <- reactive({
     switch(input$regressiontable,
             
        "Approval Rating" = formula(finalstocktib$meanofmeans ~ finalstocktib$approval_ratings),
        "Stock Market" = formula(finalstocktib$meanofmeans ~ finalstocktib$range),
        "Interaction" = formula(finalstocktib$meanofmeans ~ finalstocktib$approval_ratings * finalstocktib$range))
    })
    
    output$regressiontable <- render_gt({
      
     formula <- regressiontableInput()
      
      set.seed(10)
      fit_obj <- stan_glm(formula,
                          data = finalstocktib, 
                          family = gaussian(),
                          refresh = 0)
      
      fit_obj %>%
        tidy() %>%
        mutate(confidencelow = estimate - (std.error * 2)) %>%
        mutate(confidencehigh = estimate + (std.error * 2)) %>%
#       mutate(term = recode(term, regressiontableInput = "Formula")) %>%
        gt() %>%
#       tbl_regression() %>%
#       as_gt() %>%
#       fmt_number(columns = 2,
#                   decimals = 2) %>%
        cols_label(term = "Predictor",
                   estimate = "Beta",
                   std.error = "Standard Error",
                   confidencelow = "CI Low",
                   confidencehigh = "CI High") %>%
        tab_header(title = "Regression of Trump's Twitter Sentiment Scores") %>% 
        tab_source_note("Source: Trump Twitter Archive") 
      
    }) 
    
    
   output$tweetread <- render_gt({
      
      tweetib1 %>%
       filter(element_id == input$tweetread) %>%
       ungroup() %>%
       select(text, sentimentmeans, Flesch) %>%
       rename("Tweet" = "text",
              "Sentiment" = "sentimentmeans",
              "Readability" = "Flesch") %>%
       gt() %>%
       tab_header(title = "Sentiment and Readability of Trump's Tweets", 
                  subtitle = "Readability: 0 - 100, 100 is most readable; Sentiment: -1 to 1, 1 is most positive") %>% 
       tab_source_note("Source: Trump Twitter Archive") %>%
       tab_style(
         style = list(
           cell_fill(color = "lightgreen")
         ),
         locations = cells_body(
           rows = Sentiment > 0)
       ) %>%
       tab_style(
         style = list(
           cell_fill(color = "red")
         ),
         locations = cells_body(
           rows = Sentiment < 0)
       )
     
   })
   
   output$readability <- renderPlot({
     
     tweetgraph <- tweetib1 %>%
       ggplot(aes(x = Flesch, y = sentimentmeans, color = str_length(text))) +
       geom_point() +
       geom_label_repel(aes(label = ifelse(str_length(text) < 35, as.character(text),'')),
                        box.padding   = 0.35, 
                        point.padding = 0.5,
                        segment.color = 'grey50') +
       geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
       labs(title = "Readability and Sentiment of Trump's Tweets (09/12/20 - 10/13/20)",
            subtitle = "Readability has little relationship with Trump's sentiment on Twitter",
            x = "Readability (0 - 100; 0 is the least readable)",
            y = "Sentiment Score",
            caption = "Source: Trump Twitter Archive",
            color = "Character Count") +
       xlim(0, 100) +
       ylim(-1, 1) +
       theme_bw()
     
     tweetgraph
     
   })
   
   data_source <- reactive({
     
     if (input$source == "hill16") {
       data <- hillarytweets$text[1:100]
     } else if (input$source == "don20") {
       data <- trumptweets$text[1:100]
     return(data)
     }
     
   })
   
create_wordcloud <- function(data, num_words = 100, background = "white") {
  
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
#    corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
#    corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
    # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
    # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
    # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
    # corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background)
  
  }


output$cloud <- renderWordcloud2({
  create_wordcloud(data_source(),
                   num_words = input$num,
                   background = input$col)
  
  })
    
}

# Run the application:

shinyApp(ui = ui, server = server)
