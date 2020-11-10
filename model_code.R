
library(MASS)

# A form of weighted least squares (mm estimator; not as efficient, given the
# size of data, but it's going to handle the outlier).

robust <- rlm(meanofmeans ~ approval_ratings,
              data = finalgraphtib)

summary(robust)