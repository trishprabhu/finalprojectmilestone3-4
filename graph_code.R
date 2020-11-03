
enddates <- unique(approval_polllist$enddate)
enddates

trumptweets$approvalpolls <- c()
View(trumptweets)

trumptweets$created_at <- 
  
format(as.POSIXct(trumptweets$created_at,format='%m/%d/%Y %H:%M:%S'),
       format='%m/%d/%Y')

for (i in 1:length(enddates)) {
  #Pull the approval polls from approval_pollist and put in trump_tweets
}

