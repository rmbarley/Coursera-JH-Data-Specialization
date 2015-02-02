## You should create one R script called run_analysis.R that does the following. 

## 0. Download and read in all of the data 
if(!file.exists("data.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url, destfile = "data.zip")
    unzip(zipfile="data.zip", files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = getwd(), unzip = "internal",
          setTimes = FALSE)

}

features <- read.table("./UCI HAR Dataset/features.txt")
names(features) <- c("Feature Key", "FeatureName")

##Rename FeatureNames to be easier to call in R (we are getting ahead of
## ourself here)
features$FeatureName = gsub('-mean\\(\\)', 'Mean', features$FeatureName)
features$FeatureName = gsub('-std\\(\\)', 'Std', features$FeatureName)
features$FeatureName = gsub('-', '', features$FeatureName)

activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
##Give column names, clean up values
names(activityLabels) <- c("Code" ,"Activity")
activityLabels$Activity <- tolower(gsub("_", " ", activityLabels$Activity))


testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabels<- read.table("./UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")

trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainingLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")


## 1. Merges the training and the test sets to create one data set.
joinedDataSets <- rbind(trainingSet, testSet)
joinedLabels <- rbind(trainingLabels, testLabels)
joinedSubjects <- rbind(subjectTrain, subjectTest)
names(joinedSubjects) <- c("Subject")

## 2. Extracts only the measurements on the mean and standard deviation for each
##    measurement.

## Search for features containing "Mean" or "Std:, subset their index
featuresWanted <- grep("Mean|Std", features[ ,2])
features <- features[featuresWanted,]

##Eliminate unwanted columns from data sets
joinedDataSets <- joinedDataSets[, featuresWanted]

## 3. Uses descriptive activity names to name the activities in the data set

## Replace activity codes with labels 
activity <- activityLabels[joinedLabels[,1], 2] 
joinedLabels[, 1] <- activity
names(joinedLabels) <- "Activity"
## Also label test values
names(joinedDataSets) <- features$FeatureName

##Combine all the data
allData <- cbind(joinedSubjects, joinedLabels, joinedDataSets)

## 4. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

## Use aggregate() to generate an ordered list of means (solving your pesky
## missing subjects problem)
tidyData = aggregate(allData, by=list(Activity = allData$Activity, Subject = allData$Subject), mean)
## Remove new index columns, remove furthest one first, so as to not mess up the 
## other one's index
tidyData[,4] = NULL
tidyData[,3] = NULL

## Write the tidy data to text
write.table(tidyData, "tidy.txt", row.name = FALSE)
