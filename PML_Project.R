#Loading Data
setwd("D:/Documents/Coursera/Practical Machine Learning/Project")
data <- read.table("pml-training.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
data$classe <- as.factor(data$classe)
data$user_name <- as.factor(data$user_name)
projectTest <-read.table("pml-testing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
projectTest$classe <- as.factor(projectTest$classe)
projectTest$user_name <- as.factor(projectTest$user_name)
library(caret) 
library(randomForest)
library(ggplot2)
library(plyr)
library(rpart)

ncol(data)
#Creating a building data set and validation set
set.seed(55)
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


#removing "#DIV/0!"
for(i in 7:(ncol(train)-1)){
    if(class(train[,i])=="character"){
        train[,i] <- sub("#DIV/0!","",train[,i])
        train[,i] <- as.numeric(train[,i])
    }
}
colSums(is.na(train))

#reducing size of training data
largeNA <- colSums(is.na(train))< (nrow(train)*.9)
trainReduced <- train[,largeNA]
ncol(trainReduced)
vec <-c(7:60)


#training model
set.seed(300)
mod2 <- train(classe ~., data = trainReduced[,7:60], method = "rpart")
pred2 <- predict(mod2, trainReduced[,7:60])
confusionMatrix(trainReduced[,7:60]$classe, pred2)

set.seed(500)
mod3 <- train(classe ~., data = trainReduced[,7:60], preProcess = c("center","scale"),method = "rf")
pred3 <- predict(mod3, trainReduced[,7:60])
confusionMatrix(trainReduced[,7:60]$classe, pred3)

set.seed(100)
mod4 = randomForest(classe~., data = trainReduced[,vec], ntree=200, nodesize=25 )
pred4 <- predict(mod4, trainReduced[,vec])
confusionMatrix(trainReduced[,vec]$classe, pred4)

#test prediction
predTest <- predict(mod4, newdata = test)
confusionMatrix(test$classe, predTest)

#cross validation
predCV <- predict(mod4, newdata = val)
confusionMatrix(val$classe, predCV)

#project submission
predSub <- predict(mod4, newdata = projectTest)

setwd("D:/Documents/Coursera/Practical Machine Learning/Submission")
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(predSub)
