## create a function : 
## merge training data and test data sets
## convert data.frame to data.table
## arguments:dir is location of data, 
##           type is "X", "Y" or "subject", colnames is names of variables. 

## install.packages("data.table")
library("data.table")

readdata <- function(dir, type, colnames) {
        train <- paste(dir, "/train/", type,"_train", ".txt", sep="")
        test <- paste(dir, "/test/", type,"_test", ".txt", sep="")
        traindata <- read.table(train, col.names=colnames)
        testdata <- read.table(test, col.names=colnames)
        data <- rbind(traindata, testdata)
        data <- data.table(data)        
}

## 1. Merges the training and the test sets to create one data set.

## find the variable names of data sets
feature <- read.table("dataset/features.txt",col.names=c("id","cal"))
feature <- data.table(feature)
feature[,cal:=gsub("\\(\\)","",cal)]
feature[,cal:=gsub("\\,",".",cal)]
feature[,cal:=gsub("\\(","-",cal)]
feature[,cal:=gsub("\\)","",cal)]
varnames <- feature[,cal]

mdata <- readdata("dataset","X", varnames)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

dt <- mdata[, grepl("mean$|mean[^F]|std", names(mdata)), with=FALSE]


## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names. 

activity <- readdata("dataset", "Y", c("activity_id"))
activitylabel <- read.table("dataset/activity_labels.txt", col.names=c("activity_id","activity_label"))
activitylabel <- data.table(activitylabel)

## merge tables
setkey(activity, activity_id)
setkey(activitylabel, activity_id)
activity2 <- merge(activity, activitylabel, by="activity_id", all.x = TRUE)

data3 <- cbind(activity2$activity_label, dt)
setnames(data3, data3[,1], "activity_label")


## 5. Creates a second, independent tidy data set with the average of each variable for 
##    each activity and each subject. 

subject <- readdata("dataset", "subject", c("subject"))
data5 <- cbind(subject, data3)

## install.packages("reshape2")
library(reshape2)

newvars = colnames(data5)[3:68]
dataMelt <- melt(data5, id=c("activity_label","subject"), measure.vars=newvars)
tidyData <- dcast(dataMelt, activity_label + subject ~ variable, mean, drop = FALSE)

write.table(tidyData,"tidyData.txt")











