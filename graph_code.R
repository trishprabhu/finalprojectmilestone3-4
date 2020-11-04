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

# Add comments here.

newtrumptib <- trumptweets %>%
  mutate(element_id = 1:2927) %>%
  group_by(newdates) %>%
  select(text, newdates, element_id) %>%
  head(., 500)

# Add comments here.

trump_ss <- sentiment(get_sentences(trumptweets$text[1:500])) 

# Add comments here.

senttib <- trump_ss %>%
  group_by(element_id) %>%
  summarize(sentimentmeans = mean(sentiment, na.rm = TRUE),
            .groups = "drop")

# Add comments here.

graphtib1 <- inner_join(newtrumptib, senttib, by = "element_id")

# Add comments here.

graphtib2 <- graphtib %>%
  group_by(newdates) %>%
  summarize(meanofmeans = mean(sentimentmeans),
            .group = "drops")

# Read in the approval_polllist dataset.

approval_polllist <- read_csv("finalprojectmilestone3-4/approval_polllist.csv")

# Modify the dataset to only include the relevant time period (the time period
# that corresponds with graphtib2).

trump_approvals <- approval_polllist %>%
  mutate(id = 1:15857) %>%
  filter(id >= 15796 & id <= 15851) %>%
  #Remove Row #48!
  
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
