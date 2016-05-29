# Chapter excercises 
# https://rstudio-pubs-static.s3.amazonaws.com/49328_b22618c74f90457c8325344e3234e60d.html has the excercises displayed as well

install.packages("ISLR")
install.packages("leaps")
library(ISLR)

### Best Subset Selection
## Predict a baseball player's salary
fix(Hitters)
names(Hitters)
dim(Hitters)

# Remove NA values
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Best subset selection library
library(leaps)

# By default, regsubsets() will only report models with up to 8 variables
regfit.full <- regsubsets(Salary ~ ., Hitters)
# Output the best set of variables for each model size
summary(regfit.full)

# Now fit up to a 19-variable model
regfit.full <- regsubsets(Salary ~ ., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$which            # True/False for which variables enter a model
reg.summary$rsq              # The unadjusted R-squared increases with the addition of a new variable
reg.summary$rss




