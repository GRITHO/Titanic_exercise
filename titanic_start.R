# Titanic kaggle competition
# Lets start with the settings

# Set working directory
setwd("V:/Analytics/kaggle")

# Import the training dataset
train <- read.csv("V:/Analytics/kaggle/train.csv")

# No further formating needed
# How is it looking like
str(train)
# 'data.frame':  891 obs. of  12 variables:
# Not very big

# How many in the training set survived
prop.table(table(train$Survived))

# Have a look at the test set
test <- read.csv("V:/Analytics/kaggle/test.csv")

# size of the test dataset?
str(test)

# Case all died
test$Survived <- rep(0, 418)

# Extract file for the submission to kaggle
submit.kaggle <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit.kaggle, file = "theyallperish.csv", row.names = FALSE)

# Further improve the model
summary(train$Sex)
# Diff ot he probability males/females
prop.table(table(train$Sex, train$Survived),1)

# All females survived
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# Use also the age (child vs grownup)
train$Child <- 0
train$Child[train$Age < 18] <- 1

# aggregate by two variables
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# Take now into account the proportion
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Use additional variables: ticket price
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# model with ticket price, class, and sex
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# New submission
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0




