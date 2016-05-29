setwd("C:/Users/grozeve/Documents/_9_Misc/DataScience/Introduction to Statistical Learning\Datasets_and_Excersizes")
rm(list=ls())

##### PLOTS

# Plot basic
set.seed(3)  # Create some data
x <- rnorm(100, mean=10, sd=5)
y <- x + rnorm(100, mean=2, sd=1)
mean(x)
sd(x)
mean(y)
sd(y)
cor(x,y)

# pdf("Figure.pdf")                          # Creates pdf file and saves the plot 
plot(x, y, xlab="X data with mean 10", ylab="Y data with mean 12", 
     main="Plot of X vs. Y", col="green")
#dev.off()                                   # Tells R that we are done with the pdf file

## Countour - 3-D plot
# First practice with the outer product function, which appears in example below
z <- 1:9
names(z) <- z
z %o% z  # Creates a multiplication table
w <- 2:8
names(w) <- paste(w,":", sep="-")             # paste function is similar to concatenate
outer(z, w, "^")                              # raises z to the power of w
outer(month.abb, 1999:2003, FUN = "paste")    # Creates a matrix with dates "Jan 1999", etc.

# Now create f using the outer function
x <- seq(from = -pi, to = pi, length(50))
dim(x)
y = x
f=outer(x, y, function(x,y) cos(y)/(1+x^2))   
contour(x, y, f, nlevels=16, add=F)           # Not producing any plot of add=T

# Use Contour on a sum
x <- 1:10
y <- 2:8
f=outer(x, y, "+")
contour(x, y, f)
contour(x, y, f, nlevels=45, add=F)


## Image - produce a heat map using the Image function
image(x, y, f) # 2-D
persp(x, y, f, theta=30)                      # 3-D, theta is the viewing degree angle
persp(x, y, f, theta=30, phi=20)


### LOADING DATA
getwd()
setwd("C:/Users/grozeve/Documents/_9_Misc/DataScience/Introduction to Statistical Learning/Datasets_and_Excersizes")
list.files()
Auto <- read.table("Auto.data", header=TRUE, na.strings="?")  # Sets ? characters as NA
fix(Auto)                                     # View the data in a speadsheet-like window
dim(Auto)
Auto[1:4, ]
names(Auto)                                   # Checks the names of the variables
summary(Auto)                                 # Summary statistics for each variable
#rownames(Auto) <- Auto[ ,9]                  # Would store the values in the 9th column as row name
#Auto <- Auto[ , -9]                          # Eliminates the 9th column from the dataframe
range(year)                                   # Range - works only for quantitative variables
table(cylinders)                              # Frequency table of categorical variables


## Dealing with NAs
sum(is.na(Auto))                              # Counts all NA elements
sum(is.na(Auto$mpg))                          # Counts NA obs in a column (mpg)
sapply(Auto, function(x) sum(is.na(x)))       #Shows which columns have missing values
Auto <- na.omit(Auto)                         #Keeps only rows with non NA values
dim(Auto)
summary(Auto)



## Visual inspection
plot(Auto$cylinders, Auto$mpg)
attach(Auto)                                 # Tells R to make the vars available by name
plot(cylinders, mpg)
cylinders <- as.factor(cylinders)            # Treats cylinders as categorical
plot(cylinders, mpg)                         # Returns a boxplot since cylinders is categorical
plot(cylinders, mpg, col="red", varwidth=TRUE, horizontal=TRUE, xlab="mpg", ylab="cylinders")
                                             # Note use of varwidth and horizontal
hist(mpg, col=2, breaks=15)                  # Histogram
#par(mfrow=c(2,2))                           # Will partition the window into 2x2 panels

#pairs(Auto)                                   # Scatterplot of every pair of variables
pairs(~mpg + displacement + horsepower, Auto) # Only a subset

#Using identify - interactively identifies values on a plot
plot(mpg, horsepower)
identify(mpg, horsepower, name)              # Will identify the name of the car
# Esc on the plot to exit

# Savehistory() saves a history of the commands; loadhistory()
# Workspace() saves the workspace with all data and objects

### CREATING AN INDICATOR VARIABLE
# Suppose we want to create an indicator variable indicating if a car has High mpg 
HighMpg <- rep("No", nrow(Auto))
HighMpg[mpg>23] <- "Yes"
HighMpg <- as.factor(HighMpg)
Auto <- data.frame(Auto, HighMpg)


## WORKING WITH SUBSETS
summary(subset(Auto, HighMpg=="Yes", select=year))  # Runs summary stat for Year on a Subset
summary(subset(Auto, HighMpg=="No", select=year))
#OR
summary(Auto[HighMpg=="Yes", ])


### REGRESSION ANALYSIS

## NON-LINEARITY of the model - plot residuals vs. fitted values to check for a pattern
## If non-linearity exists, consider non-linear transformations of the predictors
## For CORRELATED RESIDUALS, plot the residuals vs. time (in time series data)
## For HETEROSKEDASTICITY, plot residuals vs. fitted values and check for a funnel shape
## For OUTLIERS, again plot residuals vs. fitted values or the studentized residuals
## For LEVERAGE, plot X vs. Y, or X1 vs. X2 or Leverage statistic vs. Studendized residuals
## Multicollinearity - get the VIF plot: if VIF is greater than 5 or 10, then collinearity is problematic

listtoremove <- ls()
rm(list=listtoremove)
ls()
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

## Simple Linear Regression

lm.fit <- lm(medv ~ lstat, data=Boston)           # Specifying the dataset
attach(Boston)
lm.fit <- lm(medv ~ lstat)                        # After using attach()
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)                                      # Equivalent but better than lm.fit$coefficients

confint(lm.fit)                                   # Confidence interval for the coef. estimates (5% is default)

# Prediction vs Confidence intervals
# A confidence interval is an interval associated with a parameter and is a frequentist concept; the parameter is assumed to be non-random but unknown
# A prediction interval is an interval associated with a random variable yet to be observed; both Bayesian and a frequentist concept
# E.g., in regression context, we may be interested in the confidence interval associated with the -mean value- of y for a given value of x;
# while we may be interested in the prediction interval associated with y for a given value of x.
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")   # Confidence interval around Y
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")   # Prediction interval around Y

plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd=3, col="red")                  # LWD option increases the line's thickness by a factor of 3

plot(lstat, medv, pch=20)                         # PCH option adds symbols
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)                        # Shows the available symbols

## Diagnostic plots

par(mfrow=c(2, 2))                                # Tells R to partition the plot into 4 panels
plot(lm.fit)                                      # Produces 4 standard plots based on the model

# Alternatively, can manually plot these graphs
plot(predict(lm.fit), residuals(lm.fit))          # Residuals vs. fitted values
plot(predict(lm.fit), rstudent(lm.fit))           # Studentized residuals vs. fitted values
plot(hatvalues(lm.fit))                           # Leverage - look for values > (p+1)/n
which.max(hatvalues(lm.fit))                      # Indicates the index of the highest obs in the vector

## See Dr. Fox Regression Diagnostics materials for more


## Multivariate Linear Regression

lm.fit <- lm(medv~lstat + age, data=Boston)
summary(lm.fit)

lm.fit <- lm(medv~ . , data=Boston)                 # Uses all of the available variables in the dataset
summary(lm.fit)

?summary.lm
summary(lm.fit)$r.sq                                # R-squared
summary(lm.fit)$sigma                               # RSE

# The CAR package has advanced regression diagnostics (ls and glm) including VIF for collinearity
install.packages("car")
library(car)
vif(lm.fit)                                       # Collinearity: VIF - look for values > 10

# Updating the model
lm.fit1 <- lm(medv ~ . -tax , data=Boston)        # Uses all but one variable (drops tax)
summary(lm.fit1)
# lm.fit1 <- update(lm.fit, ~. -tax)              # An alternative way to update the model

# Interaction variables
summary(lm(medv ~ lstat*age, data=Boston))        # medv on age + lstat + lstat x age
summary(lm(medv ~ lstat:age, data=Boston))        # medv on lstat x age

# Non-linear transformations of the predictors
# Quadratic term
lm.fit <- lm(medv ~ lstat)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))          # Quadratic term included
summary(lm.fit2)

# Use ANOVA() to examine if the quadratic model produces superior fit
# The NULL hypothesis is that the two models fit the data equally well
# The ALTERNATIVE hypothesis is that the full model (with quadratic term) fits the data better
anova(lm.fit, lm.fit2)                            # In this case, the quadratic model is better
par(mfrow=c(2,2))
plot(lm.fit2)

# Fifth polynomial - use POLY() function (consider diff. b/w raw or orthogonal, see below)
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

# Log transformation
lm.fitLog <- lm(medv ~ log(rm), data=Boston)

# Regression without intercept
lm.fit <- lm(medv ~ lstat + 0)

## Qualitative predictors

fix(Carseats)
names(Carseats)
dim(Carseats)
lm.fit <- lm(Sales ~. + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)                             # Shows the coding of the dummary variables
?contrasts()


## Excercise 13 - Chapter 3
set.seed(1)
xvar <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, 0.1)                         # Vary the variance of the error to introduce more/less noise
yvar <- -1 + 0.5*xvar + eps
df <- data.frame(xvar, yvar)
length(df$xvar)
length(df$yvar)

plot(xvar, yvar)
lm.fit <- lm(yvar ~ xvar, df)
summary(lm.fit)
abline(lm.fit, col="red", lwd=2)
#legend("top", title="Scatterplot", legend="legend")   # Explore the legend function


lm.fit2raw <- lm(yvar ~ poly(xvar, 2, raw=TRUE), df)   # Note selection of raw vs. orthog polynomials
summary(lm.fit2raw)                                    # Coefficient are different! b/c they are not transformed
names(lm.fit2raw)
yhatraw <- predict(lm.fit2raw, newdata=df)             # Both raw and ortho produce same fitted values
yhatraw1 <- lm.fit2raw$coefficients[1]+lm.fit2raw$coefficients[2]*xvar[1] + lm.fit2raw$coefficients[3]*xvar[1]*xvar[1]
yhatraw1

lm.fit2 <- lm(yvar ~ poly(xvar, 2), df)                   # Now use default (orthogonal polynomials)
summary(lm.fit2)
yhat <- predict(lm.fit2, newdata=df)
yhat1 <- lm.fit2$coefficients[1]+lm.fit2$coefficients[2]*xvar[1] + lm.fit2$coefficients[3]*xvar[1]*xvar[1]
yhat1                                                  # Note that this does not match the fitted value from predict()
                                                       # because of the orthogonal transform of the coefficients
# Now let's try to plot the quadratic fit

# plot(df$xvar, df$yvar, type="l", lwd=3, col="green")    # Would connect all plots - not useful here
points(df$xvar, predict(lm.fit2), type="l", col="green", lwd=2)   # Clearly the quadratic fit is not much different but the plot is not very clear when there is more noise


# Let's try using GGPLOT to plot a quadratic line with CI          #install.packages("ggplot2")   
library(ggplot2)
#predictx <- data.frame(x = seq(from = range(x)[1], to = range(x)[2], length.out = 100))
#prd <- data.frame(df)
err <- predict(lm.fit2, newdata=df, se.fit = TRUE)                 # Fitted values and st. errors 
names(err)
df <- data.frame(xvar, yvar, err$fit)
dim(df)

lci <- err$fit - 1.96 * err$se.fit                     # Define confidence intervals
fit <- err$fit
uci <- err$fit + 1.96 * err$se.fit

ggplot(data=df, aes(x = xvar, y = fit)) +              # PRD should be a dataframe and x and y should be vars in the DF
    theme_bw() +
    geom_line() +
    geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +   # "identity" sets up the CI
    geom_point(data = df, aes(x = xvar, y = fit))



### EXPLORING GGPLOT2
### http://www.noamross.net/blog/2012/10/5/ggplot-introduction.html
### Great for slicing and dicing data visually

## Example 1 - plot 2-variable data with Plant and Type subcategories
data.df <- data.frame(Plant = c("Plant1", "Plant1", "Plant1", "Plant2", "Plant2", 
                                "Plant2"), Type = c(1, 2, 3, 1, 2, 3), Axis1 = c(0.2, -0.4, 0.8, -0.2, -0.7, 
                                                                                 0.1), Axis2 = c(0.5, 0.3, -0.1, -0.3, -0.1, -0.8))
# Plot the data points using different colors for each Plant type and different shapes for Plant 1 and Plant 2
ggplot(data.df, aes(x = Axis1, y = Axis2, shape = Plant, color = Type)) + geom_point(size = 5)

## Example 2 - Scatter plots
head(msleep)                                   # Pre-loaded data on sleep habits of animals
msleep[msleep$name=="Cheetah", ]               # Exploring some rows
msleep[9, ]
names(msleep)

# Scatter plot of body weight vs. hrs of sleep
a <- ggplot(data = msleep, aes(x = bodywt, y = sleep_total))
a <- a + geom_point()
a <- a + xlab("Body Weight") + ylab("Total Hours Sleep") + ggtitle("Some Sleep Data")
a
ggsave("graphname.png", plot=a, width=4, height=4)

# Now colorcode the animals by type of diet and also use transformed variables
a <- ggplot(data = msleep, aes(x = log(bodywt), y = sleep_rem/sleep_total, color=vore))
a <- a + geom_point(size=5)
a <- a + xlab("Log Body Weight") + ylab("%REM Hours Sleep") + ggtitle("Some Sleep Data") + 
    scale_color_discrete(name = "Trophic Level")
a

# Now use facet_wrap to view the plot by feature
a <- ggplot(data=msleep, aes(x = log(bodywt), y=sleep_rem/sleep_total))
a <- a + geom_point(size=4)
a <- a + facet_wrap(~vore)                         # Facet with a single variable
a <- a + xlab("Log Body Weight") + ylab("%REM Hours Sleep") + ggtitle("Some sleep data")
a

a <- ggplot(data=msleep, aes(x = log(bodywt), y=sleep_rem/sleep_total))
a <- a + geom_point(size=4)
a <- a + facet_wrap(conservation~vore)                         # Facet with a single variable
a <- a + xlab("Log Body Weight") + ylab("%REM Hours Sleep") + ggtitle("Some sleep data")
a

## Example 3 - Line plots

head(economics)
names(economics)
dim(economics)

a <- ggplot(data=economics, aes(x=date, y=unemploy/pop))
a <- a + geom_line()
a <- a + geom_smooth()                                        # Adds smoothing line and below we add label for the highest value
a <- a + geom_text(data = economics[economics$unemploy/economics$pop == max(economics$unemploy/economics$pop), ], aes(label = date), hjust = 0.7, vjust = 1)
a

# More web resourses on ggplot2
# https://learnr.wordpress.com/ - various graph examples
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#working-with-the-legend
# http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r
# http://sape.inf.usi.ch/quick-reference/ggplot2 - pictorial syntax reference
# https://github.com/hadley/ggplot2/wiki - obscure display options



### Chapter 4 - Logistic regression, LDA and K nearest neighbor (KNN)
library(ISLR)
data(Smarket)
dim(Smarket)
names(Smarket)
summary(Smarket)
range(Smarket$Year)
pairs(Smarket)
cor(Smarket[ , -9])

attach(Smarket)
plot(Volume)

## Logistic regression - GLM with option family=binomial
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit)
1-pchisq(1727, 1243)                                # If highly sign. (<0.05), then not a good fit
# See http://data.princeton.edu/R/glms.html for more details
contrasts(Direction)                                # Checking encoding of Up/Down
glm.probs <- predict(glm.fit, type="response")      # Type="response" specifies cond. probabilities
glm.probs[1:10]
glm.predict <- rep("Down", 1250)                    # Create vector of class prediction using 50% as a threshold
glm.predict[glm.probs>0.5] <- "Up"                  
table(glm.predict, Direction)                       # Confusion matrix 
(145+507)/1250                                      # Manually calculate % correct predictions on the training set
mean(glm.predict==Direction)                        # Alternative way of calculating the above

# Split the dataset into training and test samples and get the test error rate
rm(glm.predict, glm.probs, glm.fit)
train <-(Year<2005)                                  # Note that vector 'train' exists ouside the dataframe
names(Smarket)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
summary(Direction.2005)

glm.fit <- glm(Direction ~ Lag1+ Lag2+ Lag3+Lag4+ Lag5+Volume, data=Smarket, family=binomial, subset=train) # Note subset option
glm.probs <- predict(glm.fit, Smarket.2005, type="response") # Predicted prob. on test data; do not specify data=Smarket.2005, just Smarket.2005
glm.predict <- rep("Down", 252)
glm.predict[glm.probs > 0.5] <- "Up"
table(glm.predict)
table(glm.predict, Direction.2005)
mean(glm.predict==Direction.2005)                   # Percent correctly predicted
mean(glm.predict!=Direction.2005)                   # Error rate
# With error rate of 52%, the results are not better than a chance prediction that equally weighs Up and Down

# Prediction for specific values of X
glm.fit <- glm(Direction ~ Lag1+ Lag2, data=Smarket, family=binomial) # Note simpler model
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")


## Linear Discriminant Analysis (LDA)
library(MASS )
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit      
names(lda.fit)
lda.fit$prior                                       # Prior prob. of Y
lda.fit$count                                       # Number of Y obs in each class
lda.fit$means                                       # The avgs of each predictor for each class
lda.fit$scaling                                     # Coef. of the Linear Discriminants
plot(lda.fit)                                       # Plots the linear discriminants
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class                         # Stores the prediction
table(lda.class, Direction.2005)                    # Tabulates prediction vs. true value
mean(lda.class==Direction.2005)                     # Share correctly classified

sum(lda.pred$posterior[ , 1] >= 0.5)                # Number of obs predicted in the first class
sum(lda.pred$posterior[ , 1] < 0.5)                 # Can vary the threshold from here

## Quadratic Discriminant Analysis (QDA)
qda.fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit                                             # Note that LD coef are not displayed
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005) 
mean(qda.class==Direction.2005)


## K-Nearest Neighbor
install.packages("Class")
library(class)
# Designate the predictors assoc. with the training data, testing data and the Ys of the training data
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]

set.seed(1)                                       # Ties are randomly assigned
knn.pred <- knn(train.X, test.X, train.Direction, k=10)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

## Application to insurance data
library(ISLR)
data(Caravan)
dim(Caravan)
names(Caravan)
attach(Caravan)
summary(Purchase)
348/(348+5474)                                   # No of people who purchased insurance

# With multiple predictors on different scales, need to standardize the data
# before applying KNN regression
standardized.X <- scale(Caravan[ , -86])         # Scale() does this; exclude qualit. vars (column 86)
# verify that the variables have been centered and scaled
var(Caravan[ , 1])
var(Caravan[ , 2])
mean(Caravan[ , 1])
mean(Caravan[ , 2])
var(standardized.X[ , 1])
var(standardized.X[ , 2])
mean(standardized.X[ , 1])
mean(standardized.X[ , 2])

# Split the sample into test and training data
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

# Run KNN
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)             # k corresponds to #neighbors considered
mean(test.Y != knn.pred)                                   # Error rate
mean(test.Y != "No")
# The KNN error rate on the 1,000 test observations is just under 12 %. 
# At first glance, this may appear to be fairly good. 
# However, since only 6 % of customers purchased insurance, we could get the error rate 
# down to 6 % by always predicting No regardless of the values of the predictors. 
#  Instead, the fraction of individuals that are correctly predicted to buy insurance is of interest.
table(knn.pred, test.Y)
9/(9+68)                                                  
# We correctly predict 12% of the people buying ins. (better than the 6% random guess)

# What if we use logistic regression?
glm.fit <- glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)
glm.probs <- predict(glm.fit, Caravan[test, ], type="response")     # Note subsetting with test; Do NOT use "data="
summary(glm.probs)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"                                  # The "<-" DIRECTION MATTERS!!!
length(glm.pred)
length(test.Y)
table(glm.pred, test.Y)                                            # 0 correctly predicted purchases with threshold of 50%

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"                                 # Lower the threshold to 25%
table(glm.pred, test.Y) 
11/(22+11)
# we predict that 33 people will purchase insurance, and we are correct for about 33 % of these people. 
# This is over five times better than random guessing!


### Chapter 5 - Cross-Validation and Bootstrap

## Validation
library(ISLR)
set.seed(2)
data(Auto)
names(Auto)
dim(Auto)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset=train)
attach(Auto)
mse <- mean((mpg - predict(lm.fit, Auto))[-train]^2)              # Test MSE (note syntax)
mse

## Leave One Out Cross Validation (LOOCV)
# Using GLM allows automatic application of LOOCV 
glm.fit <- glm(mpg ~ horsepower, data=Auto)                       # Linear regression
coef(glm.fit)
lm.fit <- glm(mpg ~ horsepower, data=Auto)                        # Coef. are the same
coef(lm.fit)

library(boot)                                                     # Has CV.GLM command
cv.err <- cv.glm(Auto, glm.fit)
names(cv.err)
cv.err$delta

# Automate the computation of the LOOCV MSE via a for loop
# Note that the cv.glm function does not use the shortcut formula for least squares
# Hence, computation time can be long
cv.error <- rep(0, 5)
for (i in 1:5) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
    cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
} 
cv.error                                                          # Here, a quadratic fit is best


## K-Fold CV with cv.glm and option "K="
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
    glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]      # [1] specifies the standard estimate; [2] is the bias-corrected one
}
cv.error.10

## Bootstrap - Two Steps: 1) Calculate function of interest; 2) Apply boot() to resample 
# Example 1 - estimating the mean and variance of a custom function
data(Portfolio)
names(Portfolio)
dim(Portfolio)
attach(Portfolio)

alpha.fn <- function(data, index) {
    X <- data$X[index]
    Y <- data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+ var(Y) -2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)                          # Applying the function to the original sample

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=TRUE)) # Sample() randomly selects 100 obs from the range 1:100 with replacement
# We can then perform the above multiple times and record each outcome
# Instead we can use the BOOT() function
boot(Portfolio, alpha.fn, R=1000)                   # 1000 replications; implicitly forms samples of the original size, 100


# Example 2 - Estimating the Accuracy of a Linear Regression Model
rm(list=ls())
data(Auto)
names(Auto)
dim(Auto)
detach()                                            # DETACH here detaches the package
attach(Auto)
search()                                            # http://www.ats.ucla.edu/stat/r/faq/referencing_objects.htm

# LM applied to the full set of observations
boot.fn <- function(data, index)                    # 1-line: no need for {}
    return(lm(mpg ~ horsepower, data=data, subset=index)$coef)
boot.fn(Auto, 1:392)

# Now use the function created above but sample with replacement twice from the original data
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=TRUE))
boot.fn(Auto, sample(392, 392, replace=TRUE))

# Now apply the BOOT() function to do this 1000 times
boot(Auto, boot.fn, R=1000)

# Compare against LS model
summary(lm(mpg~horsepower, data=Auto))$coef
# Note that the st. errors are different - this is because LS depends on assumptions about the
# structure of the error term. It can be seen that the relationship between mpg and horsepower
# is quadratic, so the linear fit error terms will be inflated. The bootstrap estimates show
# higher error terms indeed.















