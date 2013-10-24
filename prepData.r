prepData <- function(data){
  title <- as.character(data$Name)
  title <- strsplit(title," ")
  maxLen <- max(sapply(title,length))
  title <- t(sapply(title, function(x) c(x, rep(NA, maxLen - length(x)))))
  titleTemp <- title[,2]
  
  titleTest <- grepl("\\.",titleTemp)
  
  titleTemp <- ifelse(titleTest == FALSE,title[,3],title[,2])
  data$Name <- ifelse(titleTemp == "Miss." | titleTemp == "Mrs.",titleTemp,"Mr.")
  
  data$Ticket <- NULL
  data$Cabin <- NULL
  training.dummy2$ID <- NULL
  
  return(data)
}