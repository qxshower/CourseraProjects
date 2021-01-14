library(dplyr)
library(tidyr)


# make sure to set your own path to the "UCI HAR Dataset" directory
setwd("./RData/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

features <- read.table("./features.txt", col.names = c("n","measurements"))
activities <- read.table("./activity_labels.txt", col.names = c("code", "activity"))

# read in train data set
train_subject <- read.table("./train/subject_train.txt", col.names = "subject")
train_X <- read.table("./train/X_train.txt", col.names = features$measurements, check.names = FALSE)
train_Y <- read.table("./train/Y_train.txt", col.names = "code")
trainset <- data.frame(train_subject, train_Y, train_X, check.names = FALSE)


# read in test data set
test_subject <- read.table("./test/subject_test.txt", col.names = "subject")
test_X <- read.table("./test/X_test.txt", col.names = features$measurements, check.names = FALSE)
test_Y <- read.table("./test/Y_test.txt", col.names = "code")
testset <- data.frame(test_subject, test_Y, test_X, check.names = FALSE)


#1. combine train and test data set
wholeset <- rbind(trainset, testset)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- wholeset %>% select(subject, code, contains("mean"), contains("std"))


#3. Uses descriptive activity names to name the activities in the data set
TidyData$code <- activities[TidyData$code, 2]


#4. Appropriately labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean\\(\\)", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std\\(\\)", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-meanfreq\\(\\)", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


#5.  tidy data set with the average of each variable for each activity and each subject.
GroupData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise(across(names(TidyData)[3]:names(TidyData)[length(names(TidyData))],mean))

write.table(GroupData, "FinalGroupData.txt", row.name=FALSE)
