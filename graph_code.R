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

newtrumptib <- trumptweets %>%
  mutate(element_id = 1:2927) %>%
  group_by(newdates) %>%
  select(text, newdates, element_id) %>%
  head(., 500)

trump_ss <- sentiment(get_sentences(trumptweets$text[1:500])) 

senttib <- trump_ss %>%
  group_by(element_id) %>%
  summarize(sentimentmeans = mean(sentiment, na.rm = TRUE),
            .groups = "drop")

graphtib <- inner_join(newtrumptib, senttib, by = "element_id")

# Read in the approval_polllist dataset.

approval_polllist <- read_csv("finalprojectmilestone3-4/approval_polllist.csv")

# Modify the dataset to only include the relevant time period.

trump_approvals <- approval_polllist %>%
  mutate(id = 1:15857) %>%
  filter(id >= 15796)
  
# Create a vector of ending dates to iterate over.

enddates <- unique(approval_polllist$enddate)
  
#For loop is below:

n <- length(enddates)
n

results <- rep(NA, n)

for (i in 1:n) {
  step1 <- approval_polllist %>%
    filter(enddate == enddates[i])
  results[i] <- mean(step1$approve)
  #Pull the approval polls from approval_pollist and put in trump_tweets
}
results


