#Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Add survived variable into the test data set
test.survived <- data.frame(Survived=rep("None", nrow(test)), test[,])

#Combine data sets
data.combined <- rbind(train, test.survived)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Pclass)

#Load ggplot2 package
library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_histogram(width=0.5, stat = "count") +
  xlab("PClass") +
  ylab("Total Count") + 
  labs(fill="Survived")
head(as.character(train$Name))

#Check for duplication of names
length(unique(as.character(data.combined$Name)))

#Store duplicated names into a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#Closer look in the suspectedly duplicated record
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
head(misses)

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
head(mrses)

#Have a look on male data
males <- data.combined[which(data.combined$Sex == "male"),]
head(males)

#Add title variable to explore a potential 3-D relationship between Survived and PClass

#Create a function to extract title
Extract_Title <- function(name){
  name <- as.character(name)
  
  if(length(grep("Mr.", name)) > 0 ){
    return("Mr.")
  } else if (length("Miss.", name) > 0){
    return("Miss")
  } else if (length(grep("Master.", name)) > 0){
    return("Master")
  } else if (length(grep("Mrs.", name)) > 0){
    return("Mrs.")
  } else {
    return("Other")
  }
}

titles <- NULL

#https://www.youtube.com/watch?v=32o0DnuRjfg&t=3968s 1:13:30