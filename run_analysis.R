library(data.table)
library(dplyr)

# Load data from respective files
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
y_Test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
x_Test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

featuresD <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Task 1 - Merges the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_Test)
features <- rbind(x_train, x_Test)
colnames(features) <- t(featuresD[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
AllData <- cbind(features,activity,subject)


# Task 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
MeanSTD <- grep(".*Mean.*|.*Std.*", names(AllData), ignore.case=TRUE)
NoColumns <- c(MeanSTD, 562, 563)
ExtractedData <- AllData[,NoColumns]


# Task 3 - Uses descriptive activity names to name the activities in the data set
ExtractedData$Activity <- as.character(ExtractedData$Activity)
for (i in 1:6){
  ExtractedData$Activity[ExtractedData$Activity == i] <- as.character(activity_labels[i,2])
}

ExtractedData$Activity <- as.factor(ExtractedData$Activity)


# Task 4 - Appropriately labels the data set with descriptive variable names.
names(ExtractedData)<-gsub("-mean()", "Mean", names(ExtractedData))
names(ExtractedData)<-gsub("-std()", "STD", names(ExtractedData))
names(ExtractedData)<-gsub("^t", "Time", names(ExtractedData))
names(ExtractedData)<-gsub("^f", "Frequency", names(ExtractedData))
names(ExtractedData)<-gsub("-freq()", "Frequency", names(ExtractedData))
names(ExtractedData)<-gsub("tBody", "TimeBody", names(ExtractedData))


# Task 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
ExtractedData$Subject <- as.factor(ExtractedData$Subject)
ExtractedData <- data.table(ExtractedData)

TidyData <- aggregate(. ~Subject + Activity, ExtractedData, mean)
TidyData <- TidyData[order(TidyData$Subject,TidyData$Activity),]
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)


