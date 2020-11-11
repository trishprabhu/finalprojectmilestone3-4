# Load the MASS library.

library(MASS)
library(rstanarm)
library(gt)
library(gtsummary)

# A form of weighted least squares (mm estimator; not as efficient, given the
# size of data, but it's going to handle the outlier).

fit_obj <- stan_glm(meanofmeans ~ approval_ratings,
              data = finalgraphtib, 
              refresh = 0)

print(fit_obj, view = FALSE)

# Create a table of the regression results:

tbl_regression(fit_obj) %>%
  as_gt() %>%
  tab_header(title = "Regression of Trump's Twitter Sentiment Scores", 
             subtitle = "The Effect of Approval Ratings on Trump's Twitter Sentiment Score") %>% 
  tab_source_note("Source: Trump Twitter Archive") 

# What sentiment score would we expect on 3 different days, with Donald Trump's
# approval rating at 30%, 45%, and 60%, respectively?

new <- tibble(approval_ratings = c(0.30, 0.45, 0.60))

set.seed(27)
pp <- posterior_predict(fit_obj, newdata = new) %>%
  as_tibble() %>%
  mutate_all(as.numeric)

head(pp, 10)

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
       subtitle = "We have a much more precise estimate for a hypothetical Trump 
       with a 45% approval rating, given the data",
       x = "Sentiment Score",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = "Approval Rating",
                    values = c("dodgerblue", "salmon", "green")) +
  theme_bw()

approvalratingdistribution

# Read in stock data (another variable that could potentially influence Trump's
# daily Twitter score/can serve as a control).

stock_data <- read_csv("data/current_stock_data.csv")

# (Substantially) clean (yikes!) and subset the data to the relevant date range.

stock_data %>%
  rename(Date = Sepal.Length,
         sepal_width = Sepal.Width)

  
