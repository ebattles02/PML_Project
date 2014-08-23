#Loading Data
setwd("D:/Documents/Coursera/Practical Machine Learning/Project")
data <- read.table("pml-training.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
projectTest <-read.table("pml-testing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
library(caret)
library(randomForest)
library(ggplot2)
library(plyr)
ncol(data)
#Creating a building data set and validation set
inBuild <- createDataPartition(y=data$classe,
                               p = 0.7, list = FALSE)

val <- data[-inBuild, ]
buildData <- data[inBuild,]

inTrain <-createDataPartition(y=buildData$classe,
                              p = 0.7, list = FALSE)
train <- buildData[inTrain, ]
test <- buildData[-inTrain, ]

#inspecting training set
dim(train)
summary(train)

#setting user_name and predictor to factors
train$classe <- as.factor(train$classe)
train$user_name <- as.factor(train$user_name)

#removing "#DIV/0!"
for(i in 7:(ncol(train)-1)){
    if(class(train[,i])=="character"){
        train[,i] <- sub("#DIV/0!","",train[,i])
        train[,i] <- as.numeric(train[,i])
    }
}
colSums(is.na(train))

#reducing size of training data
largeNA <- colSums(is.na(train))< 9400
trainReduced <- train[,largeNA]
ncol(trainReduced)



