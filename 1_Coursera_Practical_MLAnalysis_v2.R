##############################################
##-------------------------------------------- 
## COURSERA PRACTICAL MACHINE LEARNING COURSE ASSIGNMENT
## Date created: 24.08.2014
##--------------------------------------------
##############################################

# Set working directory
setwd("~/_R_Projects/1_Coursera_PracticalMachineLearning")

# Load libraries
library(RCurl)
library(plyr)
library(reshape2)
library(ggplot2)
library(caret)


##############################################
## Load the data
##############################################

# Training dataset
urlfile<-'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
df_train<-read.csv(urlfile)

# Testing dataset
urlfile<-'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
df_test<-read.csv(urlfile)


##############################################
## Exploratory analyis and data preparation
##############################################

# View data
View(df_train)
View(df_test)

### COUNTS

# SELECT classe, COUNT(user_name) FROM df GROUP BY classe: Try ddply (works)
l<-ddply(df_tr, c("user_name","classe"),  summarise, freq=length(classe), .progress='win')
dcast(l, user_name ~ classe, value.var="freq")
# Same thing using tapply
tapply(df_tr$user_name, df_tr$user_name, FUN = count)


### CHOOSING ONLY SOME ROWS AND COLUMNS

# Subsetting
df_tr<-df_train[df_train$new_window=="no",!colnames(df_train) %in% grep("(max|min|avg|amplitude|skewness|kurtosis|stddev|var)\\_", names(df_train), value = TRUE)]

# Chart trial
e2<-melt(df_tr[,c(21:27, which(colnames(df_tr)=="classe"), which(colnames(df_tr)=="user_name"))], id=c("classe","user_name"))
ggplot(e2, aes(x=classe, y=value, fill=variable)) + 
  geom_boxplot() +
  ggtitle("Some variables according to user names") + facet_grid( variable ~ user_name, scales="free")

##############################################
## Crossvalidation: Split train and test sets
##############################################

## 75% of the sample size
smp_size <- floor(0.6 * nrow(df_tr))

## set the seed to make the partition reproductible
set.seed(123)

## Do the partitioning
train_ind <- sample(seq_len(nrow(df_tr)), size = smp_size)

train <- df_tr[train_ind, ]
test <- df_tr[-train_ind, ]

##############################################
## Modeling: Random forests
##############################################

## Set trainControl
tc <- trainControl("oob", number=3, repeats=3, classProbs=TRUE, savePred=T) 

## Do the modeling
RFFit <- train(classe ~., data=train, method="rf", trControl=tc, preProc=c("center", "scale"))

##############################################
## Validation on a test set
##############################################

## Do the prediction
test$prediction <- predict(RFFit, newdata = test)

## Count matches
test$match <- (test$classe == test$prediction)
ddply(test, c("match"),  summarise, freq=length(match), .progress='win')


##############################################
## Prediction of the true testing set
##############################################

# Subsetting
df_te<-df_test[df_test$new_window=="no",!colnames(df_test) %in% grep("(max|min|avg|amplitude|skewness|kurtosis|stddev|var)\\_", names(df_test), value = TRUE)]

# Prediction
df_te$prediction <- predict(RFFit, newdata = df_te)

# Generating the answerset
class(df_te$prediction)
answers<-as.character(df_te$prediction)
class(answers)
answers

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
