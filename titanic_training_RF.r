#!usr/bin/Rscript

print('This is the titanic project script for a random forest.')

library(randomForest)
library(Amelia)

# Kaggle Project
# Titanic Project

# Load the data

training.dummy <- read.csv('train.csv')

# Visually inspecting missing data

missmap(training.dummy, main = "Missing Map")

# Extracting the titles from the name

title <- as.character(training.dummy$Name)
title <- strsplit(title," ")
maxLen <- max(sapply(title,length))
title <- t(sapply(title, function(x) c(x, rep(NA, maxLen - length(x)))))
titleTemp <- title[,2]

titleTest <- grepl("\\.",titleTemp)

for(i in 1:length(titleTest)){
  if(titleTest[i] == FALSE){
    titleTemp[i] = title[i,3]
  }
}

titleTemp <- ifelse(titleTemp == "Miss." | titleTemp == "Mrs.",titleTemp,"Mr.")

# Prior to removing the names, I use them to take a guess at the NA ages

ages.Miss <- matrix(0, nrow = 891, ncol = 1)
ages.Mr <- matrix(0, nrow = 891, ncol = 1)
ages.Mrs <- matrix(0, nrow = 891, ncol = 1)
ages.dead <- matrix(0, nrow = 891, ncol = 1)

grep1 <- grepl('Miss',training.dummy$Name)
grep2 <- grepl('Mr',training.dummy$Name)
grep3 <- grepl('Mrs',training.dummy$Name)

condition.test <- (training.dummy$Age != 'NA' && training.dummy$Survived == 0)

training.dummy$Age[is.na(training.dummy$Age)] <- 0

for(i in 1:891){
  if(training.dummy$Age[i] != 0){
    if(training.dummy$Survived[i] ==0){
      if(grep1[i] == T){
        ages.Miss[i] <- training.dummy$Age[i]
      }
      if(grep2[i] == T){
        ages.Mr[i] <- training.dummy$Age[i]
      }
      if(grep3[i] == T){
        ages.Mrs[i] <- training.dummy$Age[i]
      }
      if(training.dummy$Survived[i] == 0){
        ages.dead[i] <- training.dummy$Age[i]
      }
    }
  }
}

ages.Miss <- ages.Miss[ages.Miss != 0]
ages.Mr <- ages.Mr[ages.Mr != 0]
ages.Mrs <- ages.Mrs[ages.Mrs != 0]
ages.dead <- ages.dead[ages.dead != 0]

average.Miss <- mean(ages.Miss)
average.Mr <- mean(ages.Mr)
average.Mrs <- mean(ages.Mrs)
average.dead <- mean(ages.dead)
for(i in 1:891){
  if(training.dummy$Age[i] == 0){
    if(grep1[i] == T){
      training.dummy$Age[i] <- average.Miss
    }
    if(grep2[i] == T){
      training.dummy$Age[i] <- average.Mr
    }
    if(grep3[i] == T){
      training.dummy$Age[i] <- average.Mrs
    }
    if(grep1[i] == F & grep2[i] == F & grep3[i] == F){
      training.dummy$Age[i] <- average.dead
    }
  }
}

training.dummy$Name <- NULL
training.dummy$Ticket <- NULL

# Sort embarkation into different columns

temp <- matrix(data = 0, nrow = 891, ncol = 3)
for(i in 1:891){
  if(training.dummy$Embarked[i] == 'C'){
    temp[i,1] <- 1
  }
  if(training.dummy$Embarked[i] == 'Q'){
    temp[i,2] <- 1
  }
  if(training.dummy$Embarked[i] == 'S'){
    temp[i,3] <- 1
  }
}

training.dummy2 <- cbind(training.dummy,temp)
training.dummy2[,14] <- training.dummy$Survived
training.dummy2$Embarked <- NULL
training.dummy2$Survived <- NULL

colnames(training.dummy2) <- c('ID','Class','Sex','Age','Sibs','Par','Fare','Cabin','Cherbourg','Queenstown','Southampton','Status')

training.dummy2$ID <- NULL
training.dummy2$Cabin <- NULL

training <- sapply(training.dummy2,as.numeric)
training <- as.data.frame(training)
training[,"Status"] <- as.factor(training[,"Status"])

training[is.na(training)] <- 0

# Training the RF

#ind <- sample(2, nrow(training), replace = TRUE, prob(0.7,0.3))
trainData <- training[1:400,]
testData <- training[401:891,]

trainData = training
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