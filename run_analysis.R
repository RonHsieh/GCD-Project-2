#1. Merges the training and the test sets to create one data set.

## download the file and unzip, put all files in one directory and list out
setwd("/Users/user/Desktop/Coursera-DSS/getcleandata/UCI HAR Dataset")
alldata<- list.files()

library(data.table)
## fread {data.table} is similar to read.table but faster with more default arguments
## grep {base} is used for searching matches to argument 'pattern' wihin each element

## Part 1. combine subject ID
subject_test<-fread(alldata[grep("subject_test", alldata)])
subject_train<-fread(alldata[grep("subject_train", alldata)])
subjectdata<-rbind(subject_train, subject_test)
setnames(subjectdata, "V1", "subjectID")

## Part 2. combine activities ID
activity_test<-fread(alldata[grep("Y_test", alldata)])
activity_train<-fread(alldata[grep("Y_train", alldata)])
activityids<-rbind(activity_train, activity_test)
setnames(activityids, "V1", "activityNum")

## Part 3. combine the feature values, in table format
test<- read.table(alldata[grep("X_test", alldata)])
train<- read.table(alldata[grep("X_train", alldata)])
values<- data.table(rbind(train, test))

## Set column names: get features for value headers
header<-fread(alldata[grep("features.txt", alldata)])
header<-header$V2
setnames(values, paste0("V",1:561), header)

## cbind all 3 parts
mergedata<- cbind(subjectdata,activityids,values)

library(dplyr)
## reshape and ordering by subject id by dplyr
mergedata<-tbl_df(arrange(mergedata, subjectID))

#---------------------------------------
#2. Extracts only the measurements on the mean and standard deviation for each measurement.

## use *select function in dplyr
## the 'contains' argument cannot tells the difference from the original feature names
## need to set valid names for selection by 'make.names'

validnames <- make.names(names=names(mergedata), unique=TRUE, allow_ = TRUE)
names(mergedata) <- validnames
extractdata<- select(mergedata, 
                     subjectID,
                     activityNum,
                     contains("mean"),
                     contains("std"))

#---------------------------------------
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.

## read activity names by lable files
actnames<-fread(alldata[grep("activity.labels", alldata)])
setnames(actnames, c("V1","V2"), c("activityNum", "activity"))

cleandata<- merge(extractdata, actnames, by= "activityNum")
cleandata<- arrange(cleandata, subjectID)

#---------------------------------------
#5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## melting data, and casting it by dcast
meltdata<- melt(cleandata, id= c("activity", "subjectID"))
averagedata<- dcast(meltdata, subjectID + activity ~ variable, mean)

## rearrange data again
averagedata<- arrange(averagedata, subjectID, activityNum)

## rename the columns to reflect the new values (means of specific activity)
newave<- paste("ave", colnames(averagedata), sep=".")
setnames(averagedata, colnames(averagedata[4:ncol(averagedata)]),
         newave[4:length(newave)])
