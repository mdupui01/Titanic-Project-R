#!usr/bin/Rscript

print('This is the Titanic script for outputting the results.')

rm(testing.dummy)

testing.dummy <- read.csv('test.csv')

testing.dummy$Age[is.na(testing.dummy$Age)] <- 0

grep1 <- grepl('Miss',testing.dummy$Name)
grep2 <- grepl('Mr',testing.dummy$Name)
grep3 <- grepl('Mrs',testing.dummy$Name)

for(i in 1:418){
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
testing.dummy$Tickets <- NULL

temp2 <- matrix(data = 0, nrow = 418, ncol = 3)
for (i in 1:418){
	if(testing.dummy$Embarked[i] == 'C'){
		temp2[i,2] <- 1
	}
	if(testing.dummy$Embarked[i] == 'Q'){
		temp2[i,2] <- 1
	}
	if(testing.dummy$Embarked[i] == 'S'){
		temp2[i,3] <- 1
	}
}

testing.dummy3 <- cbind(testing.dummy,temp2)
testing.dummy3[,14] <- testing.dummy$Survived
testing.dummy3$Embarked <- NULL
testing.dummy3$Survived <- NULL
colnames(testing.dummy3) <- c('ID','Class','Sex','Age','Sibs','Par','Fare','Cabin','Cherbourg','Queenstown','Southampton','Status')
testing <- sapply(testing.dummy3,as.numeric)

testing[is.na(testing)] <- 0

prediction.submission = predict(svp,testing[,2:11])

# Write the submission predictions to a csv file

submission <- cbind(testing[,1],prediction.submission)
colnames(submission) <- c('PassengerId','Survived')
write.table(submission,file="submission.csv",sep=",",row.names=F)
