#Loading Data
setwd("D:/Documents/Coursera/Practical Machine Learning/Project")
data <- read.csv("pml-training.csv")
projectTest <-read.csv("pml-testing.csv")
library(caret)
library(randomForest)
library(ggplot2)

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


