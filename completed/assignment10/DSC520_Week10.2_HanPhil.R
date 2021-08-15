# Assignment: ASSIGNMENT 10.2
# Name: Han, Phil
# Date: 2021-08-13



## K-means function for the binary classifier data
binary_df
binaryTrain <- binary_df[, which(names(binary_df) !="label")]
binaryTrain

set.see(1500)
binaryK<- kmeans(binaryTrain[,1:2], 4)
binaryK$cluster

require(useful)
library(ggplot2)
plot(binaryK, data = binaryTrain)

## 10.2.1 Intro to Machine Learning. Euclidean distance & KNN on binary classifier data
setwd("~/Documents/GitHub/dsc520")

## 10.2 Load the `data/binary-classifier-data.csv` to
binary_df <- read.csv("data/binary-classifier-data.csv")
head(binary_df)

## 10.2.5. Euclidean distance
bi_eucl<- binary_df[, c(2,3)]
bi_eucl
dist(bi_eucl)

euclidean<- function(a, b) sqrt(sum((a-b)^2))

## Euclidean distance between x and y
euclidean(binary_df$x, binary_df$y)

## Fitting an accuracy model test
library(caret)
control <- trainControl(method ="cv", number = 5)
set.seed(7)
#fit<- train(x~y, data = bi_eucl, method = "glm", metric = "Accuracy", trControl=control)

#print(fit)

binary_df
## Generate a randome number that is 90% of the total number of rows in dataset.
bi_random<- sample(1:nrow(binary_df), 0.9 * nrow(binary_df))

## the normalization function is created
norm<- function(x) {
return((x -min(x)) /(max(x)-min(x))) }

binary_norm<- as.data.frame(lapply(binary_df[,c(2,3)], norm))

summary(binary_norm)

## extract training set
binary_train<- binary_norm[bi_random,]

## extract testing set
binary_test<- binary_norm[-bi_random,]

## extract 1st column of train dataset to use it as 'cl' arguement in KNN func.
binary_target_category<- binary_df[bi_random,1]

## extract 1st column of test dataset to measure accuracy
binary_test_category<- binary_df[-bi_random,1]

## load the package class
library(class)

## run KNN function by fitting a knn model for k=3
pr<- knn(binary_train, binary_test, cl=binary_target_category, k=3)

## create confusion matrix
tab<- table(pr, binary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)

## run KNN function by fitting a knn model for k=5
pr<- knn(binary_train, binary_test, cl=binary_target_category, k=5)

## create confusion matrix
tab<- table(pr, binary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)

## run KNN function by fitting a knn model for k=10
pr<- knn(binary_train, binary_test, cl=binary_target_category, k=10)

## create confusion matrix
tab<- table(pr, binary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)


## run KNN function by fitting a knn model for k=15
pr<- knn(binary_train, binary_test, cl=binary_target_category, k=15)

## create confusion matrix
tab<- table(pr, binary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)

## run KNN function by fitting a knn model for k=20
pr<- knn(binary_train, binary_test, cl=binary_target_category, k=20)

## create confusion matrix
tab<- table(pr, binary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)

trinary_df <- read.csv("data/trinary-classifier-data.csv")
trinary_df
summary(trinary_df)

trinary_df
trinaryTrain <- trinary_df[, which(names(binary_df) !="label")]
trinaryTrain

set.see(1500)
trinaryK<- kmeans(trinaryTrain[,1:2], 3)
trinaryK$cluster

plot(trinaryK, data = trinaryTrain)

trinary_df

## Generate a randome number that is 90% of the total number of rows in dataset.
tri_random<- sample(1:nrow(trinary_df), 0.9 * nrow(trinary_df))

## the normalization function is created
norm<- function(x) {
  return((x -min(x)) /(max(x)-min(x))) }

trinary_norm<- as.data.frame(lapply(trinary_df[,c(2,3)], norm))

summary(trinary_norm)

## extract training set
trinary_train<- trinary_norm[tri_random,]

## extract testing set
trinary_test<- trinary_norm[-tri_random,]

## extract 1st column of train dataset to use it as 'cl' arguement in KNN func.
trinary_target_category<- trinary_df[tri_random,1]

## extract 1st column of test dataset to measure accuracy
trinary_test_category<- trinary_df[-tri_random,1]

## load the package class
library(class)

## run KNN function by fitting a knn model for k=3
pr<- knn(trinary_train, trinary_test, cl=trinary_target_category, k=3)

## create confusion matrix
tab<- table(pr, trinary_test_category)

## this function calculates accuracy by dividing the correct predictions by total
## number of predictions that will tell us how accurate the model is.
accuracy<- function(x) (sum(diag(x)/(sum(rowSums(x)))) * 100)
accuracy(tab)

## run KNN function by fitting a knn model for k=5
pr<- knn(trinary_train, trinary_test, cl=trinary_target_category, k=5)

## create confusion matrix
tab<- table(pr, trinary_test_category)

## 10.2. Clustering and K-means function for the binary classifier data

cluster_df <- read.csv("data/clustering-data.csv")
cluster_df
summary(cluster_df)


## Plotting the data set
plot(cluster_df)

ggplot(cluster_df, aes(x = x, y = y)) +
  geom_point()

## K-means function for the cluster data

set.seed(123)
km.cluster<- kmeans(cluster_df[,1:2], centers = 2, nstart=12,iter.max = 12)
plot(cluster_df$x, cluster_df$y,
     main=paste("centers:", 2), 
     xlab = "X-axis",
     ylab = "Y-axis",
     col = km.cluster$cluster)
points(km.cluster$centers, col=2, pch=8, cex=2)

euclidean<- function(a, b) sqrt(sum((a-b)^2))

## Euclidean distance between x and y
euclidean(cluster_df$x, cluster_df$y)


str(km.cluster)

