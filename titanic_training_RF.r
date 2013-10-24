#!usr/bin/Rscript

print('This is the titanic project script for a random forest.')

library(randomForest)
library(Amelia)

graphics.off()

# Kaggle Project
# Titanic Project

# Load the data

training.dummy <- read.csv('train.csv')
temp <- training.dummy

# Visually inspecting missing data

missmap(training.dummy, main = "Missing Map")

# Preping the data with a custom function

training.dummy <- prepData(training.dummy)

# Use the title to make a guess at the age
age.Miss <- ifelse((training.dummy$Name == "Miss."),1,NA)
age.Miss <- age.Miss*training.dummy$Age
mean.age.Miss <- mean(age.Miss,na.rm = T)
training.dummy$Age <- ifelse(training.dummy$Name == "Miss." & training.dummy$Age == NA, mean.age.Miss, Age)

age.Mrs <- ifelse(training.dummy$Name == "Mrs.",training.dummy$Age,NA)
mean.age.Mrs <- mean(age.Mrs,na.rm = T)
training.dummy$Age <- ifelse(training.gummy$Name == "Mrs." & training.dummy$Age == NA, mean.age.Mrs, Age)

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