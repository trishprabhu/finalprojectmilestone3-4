# Load the dplyr and sentimentr library.

library(magrittr)
library(dplyr)
library(sentimentr)
library(tidyverse)
library(readr)

# Load the lubridate library; use as.Date to create a new column with the
# created_at values in MDY format.

library(lubridate)
trumptweets$newdates <- (as.Date(mdy_hms(trumptweets$created_at)))

# Create a subset of the trumptweets tibble with a new element_id column (to
# calculate sentiment means later) and a smaller set of observations (to begin
# with, for Milestone purposes).

newtrumptib <- trumptweets %>%
  mutate(element_id = 1:2927) %>%
  group_by(newdates) %>%
  select(text, newdates, element_id) %>%
  head(., 500)

# Use sentiment() to calculate sentiment scores for 500 Tweets in the 
# trumptweets dataset.

trump_ss <- sentiment(get_sentences(trumptweets$text[1:500])) 

# By grouping by element_id, we're able to take the mean of the sentiment scores
# for each Tweet.

senttib <- trump_ss %>%
  group_by(element_id) %>%
  summarize(sentimentmeans = mean(sentiment, na.rm = TRUE),
            .groups = "drop")

# Use inner_join to get the Tweets, dates, and "sentimentmeans" in one tibble. 

graphtib1 <- inner_join(newtrumptib, senttib, by = "element_id")

# But wait! These are sentiment means for each Tweet, and we want averages for 
# each day -- as we'll be looking at Trump's approval rating on the comparable
# day. Luckily, grouping by newdates, and taking the mean of the means does
# the trick.

graphtib2 <- graphtib1 %>%
  group_by(newdates) %>%
  summarize(meanofmeans = mean(sentimentmeans),
            .groups = "drop")

# Read in the approval_polllist dataset.

approval_polllist <- read_csv("finalprojectmilestone3-4/approval_polllist.csv")

# Modify the dataset to only include the relevant time period (the time period
# that corresponds with graphtib2).

trump_approvals_almost <- approval_polllist %>%
  mutate(id = 1:15857) %>%
  filter(id >= 15796 & id <= 15851)
  
# But we need to make one last alteration (removing Row 48, which includes the
# approval rating from a date not in the relevant time period).
  
trump_approvals <- trump_approvals_almost[-48, ]
  
# Create a vector of ending dates to iterate over.

enddates <- unique(trump_approvals$enddate)
  
#For loop is below:

# Define n value:

n <- length(enddates)
n

# Empty results vector that is the length of n:

results <- rep(NA, n)

# Add comments here.

for (i in 1:n) {
  step1 <- trump_approvals %>%
    filter(enddate == enddates[i])
  results[i] <- mean(step1$approve)
  # Pull the approval polls from approval_pollist (done!) and put in 
  # graphtib2 (TBD!).
}
results
