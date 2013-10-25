#!usr/bin/Rscript

print('This is the Titanic script for outputting the results.')

testing.dummy <- read.csv('test.csv')

# Cleaning up the data

testing.dummy <- prepData(testing.dummy)

# Use the title to make a guess at the age

testing.dummy <- fillAge(testing.dummy)

# Sort embarkation into different columns

temp <- data.frame(C = 1:418,Q = 1:418, S = 1:418)
temp$C <- ifelse(testing.dummy$Embarked == "C",1,0)
temp$Q <- ifelse(testing.dummy$Embarked == "Q",1,0)
temp$S <- ifelse(testing.dummy$Embarked == "S",1,0)

testing.dummy2 <- cbind(testing.dummy,temp)
testing.dummy2$Embarked <- NULL

temp <- data.frame(Mr = 1:418, Miss = 1:418, Mrs = 1:418)
temp$Mr <- ifelse(testing.dummy$Name == "Mr.",1,0)
temp$Miss <- ifelse(testing.dummy$Name == "Miss.",1,0)
temp$Mrs <- ifelse(testing.dummy$Name == "Mrs.",1,0)

testing.dummy2 <- cbind(testing.dummy2,temp)
testing.dummy2$Name <- NULL


colnames(testing.dummy2) <- c('ID','Class','Sex','Age','Sibs','Par','Fare','Cherbourg','Queenstown','Southampton','Mr','Miss','Mrs')

testing <- sapply(testing.dummy2,as.numeric)
testing <- as.data.frame(testing.dummy2)

testing[is.na(testing)] <- 0

prediction.submission <- predict(training, testing)

# Write the submission predictions to a csv file

submission <- cbind(ID,prediction.submission)

submission[,2] <- submission[,2]-1

colnames(submission) <- c('PassengerId','Survived')
write.table(submission,file="submission.csv",sep=",",row.names=F)
