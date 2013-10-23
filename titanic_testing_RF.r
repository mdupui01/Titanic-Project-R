#!usr/bin/Rscript

print('This is the Titanic script for outputting the results.')

testing.dummy <- read.csv('test.csv')

# Prior to removing the names, I use them to take a guess at the NA ages

condition.test <- (testing.dummy$Age != 'NA' && testing.dummy$Survived == 0)

testing.dummy$Age[is.na(testing.dummy$Age)] <- 0

for(i in 1:nrow(testing.dummy)){
  if(testing.dummy$Age[i] == 0){
    if(grep1[i] == T){
      testing.dummy$Age[i] <- average.Miss
    }
    if(grep2[i] == T){
      testing.dummy$Age[i] <- average.Mr
    }
    if(grep3[i] == T){
      testing.dummy$Age[i] <- average.Mrs
    }
    if(grep1[i] == F & grep2[i] == F & grep3[i] == F){
      testing.dummy$Age[i] <- average.dead
    }
  }
}

testing.dummy$Name <- NULL
testing.dummy$Ticket <- NULL

# Sort embarkation into different columns

temp <- matrix(data = 0, nrow = nrow(testing.dummy), ncol = 3)
for(i in 1:nrow(testing.dummy)){
  if(testing.dummy$Embarked[i] == 'C'){
    temp[i,1] <- 1
  }
  if(testing.dummy$Embarked[i] == 'Q'){
    temp[i,2] <- 1
  }
  if(testing.dummy$Embarked[i] == 'S'){
    temp[i,3] <- 1
  }
}
testing.dummy$Embarked <- NULL
testing.dummy2 <- cbind(testing.dummy,temp)
colnames(testing.dummy2) <- c('ID','Class','Sex','Age','Sibs','Par','Fare','Cabin','Cherbourg','Queenstown','Southampton')
ID <- testing.dummy2$ID
testing.dummy2$ID <- NULL
testing.dummy2$Cabin <- NULL

testing <- sapply(testing.dummy2,as.numeric)
testing <- as.data.frame(testing)

testing[is.na(testing)] <- 0

prediction.submission <- predict(training_rf, testing)

# Write the submission predictions to a csv file

submission <- cbind(ID,prediction.submission)

submission[,2] <- submission[,2]-1

colnames(submission) <- c('PassengerId','Survived')
write.table(submission,file="submission.csv",sep=",",row.names=F)
