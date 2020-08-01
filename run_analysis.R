library(dplyr)

#Getting data 
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if(!file.exists(zipFile)){
  download.file(zipurl, zipFile, mode = "wb")
}

#Unzip zip file containing data if data directory doesnt exist 
datapath <- "UCI HAR DATASET"
if(!file.exists(datapath)){
  unzip(zipFile)
}

#Read Data
trainingSubject <- read.table(file.path(datapath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(datapath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(datapath, "train", "y_train.txt"))

#read test data
testSubjects <- read.table(file.path(datapath, "test", "subject_test.txt"))
testValues <- read.table(file.path(datapath, "test", "X_test.txt"))
testActivity <- read.table(file.path(datapath, "test", "y_test.txt"))

#read features, dont convert text labels to factorrs
features <- read.table(file.path(datapath, "features.txt"), as.is = TRUE)
#note: feature names (in features[, 2]) are not unique
#e.g. fBodyAcc-bandsEnergy()-1, 8

#read activity labels 
activities <- read.table(file.path(datapath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#merge the training and the test sets to create one data set

#concatenate individual data tables to save memory
humanActivity <- rbind(
  cbind(trainingSubject, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

#remove individual data tables to save memory
rm(trainingSubject, trainingValues, trainingActivity,
   testSubjects, testValues, testActivity)

#assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")



#Extract onlt the measurements to the mean and SD  for each measurement
columnsToKeep <- grep("subject|activity|mean|std", colnames(humanActivity))

#..... and keep data in these colums only 
humanActivity <- humanActivity[, columnsToKeep]

#use descriptive activity names to name the activities in the data set 

#replace activity values with names factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


#Appropriately label the data set with descriptive variable names 

#get columnname s
humanActivityCols <- colnames(humanActivity)

#remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

#expand abbreviations and clean up names 
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

#Correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

#use new labels as column names 
colnames(humanActivity) <- humanActivityCols

#create a second, independent tidy set with the average of each variable for each activity and each subject

#group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

#output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote=FALSE)
