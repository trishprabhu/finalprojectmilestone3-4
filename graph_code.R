# Load the dplyr library.

library(dplyr)

# Create a vector of ending dates to iterate over.

enddates <- unique(approval_polllist$enddate)

#Create an empty column to deposit approval ratings in.

trumptweets$approvalpolls <- c()

# Load the lubridate library; use as.Date to create a new column with the
# created_at values in MDY format.

library(lubridate)
trumptweets$newdates <- (as.Date(mdy_hms(trumptweets$created_at)))

# (Attempt to) create a new tibble that calculates a sentiment score for each 
# date in trumptweets (currently not working, because I'm not sure how to handle
# the fact that there are multiple Tweets for each date.

graphtibble <- trumptweets %>%
  select(text, newdates) %>%
  group_by(newdates) %>%
  sample()
  summarize(sentimentdate = sentiment(get_sentences(text)),
            .groups = "drop") %>%
  mutate(sentimentdatemean = mean(sentimentdate$sentiment)) %>%
  select(newdates, sentimentdatemean) %>%
  slice_head(n = 1)

graphtibble

#For loop is below:

n <- length(enddates)
n

results <- rep(NA, n)

for (i in 1:n) {
  results[i] <- approval_polllist$approve[i]
  #Pull the approval polls from approval_pollist and put in trump_tweets
}
results
