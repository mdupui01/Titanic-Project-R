#!usr/bin/Rscript

print('This is the titanic project script for a random forest.')

library(randomForest)
library(Amelia)

graphics.off()

# Kaggle Project
# Titanic Project

# Load the data

training.dummy <- read.csv('train.csv')

# Visually inspecting missing data

# missmap(training.dummy, main = "Missing Map")

# Preping the data with a custom function

training.dummy <- prepData(training.dummy)

# Use the title to make a guess at the age

training.dummy <- fillAge(training.dummy)

# Sort embarkation into different columns

temp <- data.frame(C = 1:891,Q = 1:891, S = 1:891)
temp$C <- ifelse(training.dummy$Embarked == "C",1,0)
temp$Q <- ifelse(training.dummy$Embarked == "Q",1,0)
temp$S <- ifelse(training.dummy$Embarked == "S",1,0)

training.dummy2 <- cbind(training.dummy,temp)
training.dummy2$Embarked <- NULL

temp <- data.frame(Mr = 1:891, Miss = 1:891, Mrs = 1:891)
temp$Mr <- ifelse(training.dummy$Name == "Mr.",1,0)
temp$Miss <- ifelse(training.dummy$Name == "Miss.",1,0)
temp$Mrs <- ifelse(training.dummy$Name == "Mrs.",1,0)

training.dummy2 <- cbind(training.dummy2,temp)
training.dummy2[,16] <- training.dummy$Survived
training.dummy2$Name <- NULL
training.dummy2$Survived <- NULL


colnames(training.dummy2) <- c('ID','Class','Sex','Age','Sibs','Par','Fare','Cherbourg','Queenstown','Southampton','Mr','Miss','Mrs','Status')

training <- sapply(training.dummy2,as.numeric)
training <- as.data.frame(training.dummy2)
training[,"Status"] <- as.factor(training[,"Status"])
training[is.na(training)] <- 0

# Training the RF

trainData <- training[1:400,]
testData <- training[401:891,]

training_rf <- randomForest(x = trainData[,1:(ncol(trainData)-1)], y = trainData[,"Status"], importance = TRUE, do.trace = 1000)

test_predict <- predict(training_rf, testData[,1:(ncol(trainData)-1)])

output = matrix(0,nrow(testData),1)
for(i in 1:nrow(testData)){
  if(test_predict[i] == testData[i,ncol(testData)]){
    output[i] = 1
  }else{
    output[i] = 0
  }
}

accuracy = sum(output)/length(output)
