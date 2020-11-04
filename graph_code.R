library(dplyr)

# Create a vector of ending dates to iterate over.
enddates <- unique(approval_polllist$enddate)

#Create an empty column to deposit approval ratings in.
trumptweets$approvalpolls <- c()

library(lubridate)
trumptweets$newdates <- (as.Date(mdy_hms(trumptweets$created_at)))
View(trumptweets)

n <- length(enddates)
n

results <- rep(NA, n)

for (i in 1:n) {
  results[i] <- approval_polllist$approve[i]
  #Pull the approval polls from approval_pollist and put in trump_tweets
}
results

