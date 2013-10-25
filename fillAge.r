fillAge <- function(x){
  size <- dim(x)
  cols <- size[2]
  
  if(cols >11){
    age.Miss <- ifelse((x$Name == "Miss."),1,NA)
    age.Miss <- age.Miss*x$Age
    mean.age.Miss <- mean(age.Miss,na.rm = T)
    
    age.Mrs <- ifelse((x$Name == "Mrs."),1,NA)
    age.Mrs <- age.Mrs*x$Age
    mean.age.Mrs <- mean(age.Mrs,na.rm = T)
    
    age.Mr <- ifelse((x$Name == "Mr."),1,NA)
    age.Mr <- age.Mr*x$Age
    mean.age.Mr <- mean(age.Mr,na.rm = T)
    
    x$Age[is.na(x$Age)] <- 0 # ifelse() doesn't work with NA values
    
    x$Age <- ifelse(x$Age == 0 & x$Name == "Miss.", mean.age.Miss, x$Age)
    x$Age <- ifelse(x$Age == 0 & x$Name == "Mrs.", mean.age.Mrs, x$Age)
    x$Age <- ifelse(x$Age == 0 & x$Name == "Mr.", mean.age.Mr, x$Age)
  }else{
    x$Age[is.na(x$Age)] <- 0 # ifelse() doesn't work with NA values
    
    x$Age <- ifelse(x$Age == 0 & x$Name == "Miss.", mean.age.Miss, x$Age)
    x$Age <- ifelse(x$Age == 0 & x$Name == "Mrs.", mean.age.Mrs, x$Age)
    x$Age <- ifelse(x$Age == 0 & x$Name == "Mr.", mean.age.Mr, x$Age)
  }
  
  return(x)
}