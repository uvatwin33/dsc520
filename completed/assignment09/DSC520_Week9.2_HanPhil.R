# Assignment: ASSIGNMENT 9.2
# Name: Han, Phil
# Date: 2021-08-04


## Set the working directory to the root of your DSC 520 directory
setwd("~/Documents/GitHub/dsc520")

## 9.2.1 Load the `data/ThoraricSurgery.csv` to
ThorSurgery_df <- read.csv("data/ThoraricSurgery1.csv")
ThorSurgery_df

install.packages('ISLR')
install.packages('SDMtune')
library(ISLR)
library(tidyverse)
library(caret)
library(magrittr)
library(SDMtune)

# 9.2.1.2 Q1. Logistic Regression

head(ThorSurgery_df)

colnames(ThorSurgery_df) <- c(
  "diagnosis",
  "FVC",
  "FEV1",
  "Perf Stat",
  "Pain",
  "Haemoptysis",
  "Dyspnoea",
  "Cough",
  "Weakness",
  "T Size",
  "T2 Diab",
  "MI",
  "PAD",
  "Smoking",
  "Asthma",
  "Age",
  "Risk1Yr"
  )

head(ThorSurgery_df)
str(ThorSurgery_df)

## 9.2.1.2 Fit a binary logitic regression model to the data set that predicts 
# survival chances for one year

glm.fit <- glm(Risk1Yr ~ ., data = ThorSurgery_df,
                   family = binomial)

summary(glm.fit)

## 9.2.1.2 Q3.
# Based on the P-Values (e.g. less than .05), Smoking (PRE30), Dyspnoea before surgery (PRE9),
# Size of the original tumor (largest & 2nd largest), and Diabetes mellitus (PRE17) had the 
# greatest effect on the survival rate.

## 9.2.1.3
library(lattice)
library(ggplot2)
library(caret)
library(caTools)

ThorSurgery_df <- read.csv("data/ThoraricSurgery1.csv")
ThorSurgery_df

split <- sample.split(ThorSurgery_df, SplitRatio = 0.8)
split

train<- subset(ThorSurgery_df, split = "TRUE")
test<- subset(ThorSurgery_df, split = "FALSE")
train



tr_model <- glm(Risk1Yr ~ ., data = train,
                family = binomial)

res <- predict(tr_model, data = train, type = "response")
res

res <- predict(tr_model, data = test, type = "response")
res

confmatrix <- table(Actual_value = train$Risk1Yr, Predicted_value = res > 0.5)
confmatrix

# 9.2.1.3 Q3 Answer: Accuracy of the train model is 83.62%.  Thus, this model is pretty accurate to predict
# the survival rate using other variables such as the size of tumor before the surgery
# and the history of smoking, & etc.

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)



## 9.2.2 Fit a Logistic Regression Model to a binary classifier data set
## Set the working directory to the root of your DSC 520 directory
setwd("~/Documents/GitHub/dsc520")

## 9.2.1 Load the `data/binary-classifier-data.csv` to
binary_df <- read.csv("data/binary-classifier-data.csv")

split <- sample.split(binary_df, SplitRatio = 0.8)
split

train<- subset(binary_df, split = "TRUE")
test<- subset(binary_df, split = "FALSE")
train


binary_df$label <- as.factor(binary_df$label)
binary_df$x <- as.factor(binary_df$x)
binary_df$y <- as.factor(binary_df$y)

binary_class_model<- glm(label ~ x + y, data = train, family = binomial)
summary(binary_class_model)

res <- predict(binary_class_model, data = train, type = "response")
res

res <- predict(binary_class_model, data = test, type = "response")
res

confmatrix <- table(Actual_value = train$label, Predicted_value = res > 0.5)
confmatrix

## Accuracy of the classifier is 58.34%.  Thus, it is not accurate to predict label given 
# x and y values.

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)





