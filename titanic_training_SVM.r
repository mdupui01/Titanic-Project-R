#!/usr/bin/Rscript

print('This is the Titanic project script for an SVM.')

library(kernlab)

# Kaggle Project
# Titanic Project

# Load the data

training.dummy <- read.csv('train.csv')

# Prior to removing the names, I use them to take a guess at the NA ages.

ages.Miss <- matrix(0, nrow = 891, ncol = 1)
ages.Mr <- matrix(0, nrow = 891, ncol = 1)
ages.Mrs <- matrix(0, nrow = 891, ncol = 1)
ages.dead <- matrix(0, nrow = 891, ncol = 1)

grep1 <- grepl('Miss',training.dummy$Name)
grep2 <- grepl('Mr',training.dummy$Name)
grep3 <- grepl('Mrs',training.dummy$Name)

condition.test <- (training.dummy$Age != 'NA' && training.dummy$Survived == 0)

training.dummy$Age[is.na(training.dummy$Age)] <- 0

for (i in 1:891){
	if(training.dummy$Age[i] != 0){
	if(training.dummy$Survived[i] == 0){
		if(grep1[i] == T){
			ages.Miss[i] <- training.dummy$Age[i]
		}
		if(grep2[i] == T){
			ages.Mr[i] <- training.dummy$Age[i]
		}
		if(grep3[i] == T){
			ages.Mrs[i] <- training.dummy$Age[i]
		}
	}
	if(training.dummy$Survived[i] == 0){
		ages.dead[i] <- training.dummy$Age[i]
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

for (i in 1:891){
	if(training.dummy$Age[i] ==0){
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
training.dummy$Ticket <- NULL # I removed both the names and tickets because they don't seem to contain much information (caution)

# Sort embarkation into different columns

temp <- matrix(data = 0,nrow=891,ncol=3)
for (i in 1:891){
	if(training.dummy$Embarked[i] =='C'){
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
training <- sapply(training.dummy2,as.numeric) # I did this because I don't like having strings as variables

training[is.na(training)] <- 0 # For now simply replace all NA values with 0

# Train the SVM

svp <- ksvm(training[,2:11],training[,12],type="C-svc",kernel='vanilladot',C=100,scaled=c(),cross=5)

prediction = predict(svp,training[,2:11])

accuracy = (sum(prediction==training[,12])/length(training[,12]))*100
cat("Classifier success rate: ",round(accuracy,digits=4),"% \n")
