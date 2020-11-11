# Just a note: a lot of my comments here are explanatory (simply describing what 
# the code below does) -- this is because this data wrangling was very 
# complicated, and explanation/reminder-focused comments were helpful 
# (especially when I'd revisit the code after a while!). I hope that's okay.

# Load the dplyr and sentimentr library.

library(magrittr)
library(dplyr)
library(sentimentr)
library(tidyverse)
library(readr)
library(ggthemes)


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
  head(., 1264)

# Use sentiment() to calculate sentiment scores for 500 Tweets in the 
# trumptweets dataset.

trump_ss <- sentiment(get_sentences(trumptweets$text[1:1264])) 

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
  filter(id >= 15725 & id <= 15851)
  
# But we need to make one last alteration (removing Row 48, which includes the
# approval rating from a date not in the relevant time period).
  
trump_approvals <- trump_approvals_almost[-119, ]
  
# Create a vector of ending dates to iterate over; order appropriately.

trump_approvals1 <- trump_approvals %>%
  mutate(enddate = as.Date(enddate, "%m/%d/%Y")) %>%
  mutate(enddate = as.character(sort(enddate)))

enddates <- unique(trump_approvals1$enddate)
  
#For loop is below:

# Define n value:

n <- length(enddates)
n

# Empty results vector that is the length of n:

results <- rep(NA, n)

# In this forloop, we filter trump_approvals (our trimmed dataset) to those
# values for which enddate is equivalent to one of the unique values of 
# enddates. Assigning that to step1, we then use the base $ operator and mean
# (no na.rm argument necessary) to calculate the mean approval rating for that
# given enddate, and deposit it in a results vector the length of enddates.

for (i in 1:n) {
  step1 <- trump_approvals1 %>%
    filter(enddate == enddates[i])
  results[i] <- mean(step1$approve)
}
results

# Add the results vector to finalgraphtib.

finalgraphtib <- graphtib2 %>%
  mutate(approval_ratings = results/100)

# Use ggplot to create our graph (FINALLY)!

finalgraph <- finalgraphtib %>%
  ggplot(aes(x = approval_ratings, y = meanofmeans)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  labs(title = "Trump's daily approval ratings and sentiment scores on Twitter, 09/12 - 10/13",
       subtitle = "Trump's approval ratings and sentiment scores seem to be weakly positively correlated",
       x = "Approval Rating",
       y = "Sentiment Score",
       caption = "Source: Trump Twitter Archive") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw()

finalgraph
