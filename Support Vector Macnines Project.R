# open loan_data and save it as a dataframe
loans <- read.csv('loan_data.csv')

# check the summary and structure of loans.
summary(loans)
str(loans)

# convet the columns to categorical data using factor()
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

# EDA
# explore data using visualization

library(ggplot2)
library(dplyr)

# create a histogram of fico scores
ggplot(loans,aes(fico)) + geom_histogram(aes(color=not.fully.paid)) + theme_bw()

# create a barplot of purpose counts, colored by not.fully.paid.
ggplot(loans,aes(factor(purpose))) + geom_bar(aes(color=not.fully.paid), position = 'dodge')

# create a scatterplot of fico score versus int.rate.
ggplot(loans,aes(int.rate,fico)) + geom_point(aes(color=not.fully.paid),alpha=0.4)

# Building the Model
# train and test sets
# split your data into training and test sets using the caTools library
library(caTools)
set.seed(101)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loan.train <- subset(loans, sample == TRUE)
loan.test <- subset(loans, sample == FALSE)

library(e1071)

loan.model <- svm(not.fully.paid ~., data = loan.train)
summary(loan.model)

loan.predicted <- predict(loan.model,loan.test[1:13])

table(loan.predicted,loan.test$not.fully.paid)


# Tuning the Model
tuned.loan <- tune(svm, train.x = not.fully.paid ~., data = loan.train, kernel = 'radial',
                   ranges = list(cost=c(10,20), gamma = c(0.1)))
summary(tuned.loan)

final.model <- svm(not.fully.paid ~., data = loan.train, cost=10, gamma = 0.1)
predicted.values <- predict(final.model,loan.test[1:13])

#

table(predicted.values,loan.test$not.fully.paid)

# predicted.values    0    1
#                0   2350 425
#                1    63  35

# 83% accuracy

