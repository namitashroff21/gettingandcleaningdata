#Loading the required dplyr and data.table packages
library(dplyr)
library(data.table)
#Reading the features and activity data and storing it in a variable
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
#Reading training and test data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
# Part 1- Merging the train and test datasets
# combing the train and test respective datasets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
# Naming the column in the features dataset
colnames(features) <- t(featureNames[2])
#Merging the features,  activity and subject of the train and test dataset and store it in variable called completeData
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
#Part 2- Extract the measurements on the mean and STD for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
#Adding the activity and subject columns and check the dimension
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
#creating the extractedData with selected columns
extractedData <- completeData[,requiredColumns]
dim(extractedData)
#Part 3- Descriptive activity names to name the activities in the dataset

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
        extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#Now factor the activity
extractedData$Activity <- as.factor(extractedData$Activity)
#Part 4 - Appropriately labels the dataset with descriptive variable names
names(extractedData)
#By viewing, we can replace the following acronyms as under
# Acc - can be replaced with Accelerometer
#Gyro - can be replaced with Gyroscope
# BodyBody - can be replaced with Body
# Mag - can be replaced with Magnitude
# f and t - can be replaced with Frequency and Time
# mean() - Mean, std() - STD,  freq() - Frequency, angle - Angle, gravity - Gravity
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Modified extractedData
names(extractedData)
#part 5- creating tidy data set with avergae of each variable for activity and subject
#set the subject as factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
# Creating the tidyData with average of activity and subject datasets. Then, writing it into Tidy.txt as processed data
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

