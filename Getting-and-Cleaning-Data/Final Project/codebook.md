Codebook For Final Project
=============================
Assignment:
------------
You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names.

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each          activity and each subject.



run_analysis.r script performs the following steps to clean the raw data:
-------------------------------------------------------------------------------
  1.  Reads in the features.txt file from the /./UCI HAR Dataset directory and saves it to the variable *features*. It then renames the columns in features.txt, and takes out some of the formatting that would break functions (removes (), -, etc.)
  2.  Reads in the file activity_labels.txt from the ./UCI HAR Dataset/ directory and stores it as the variable *activityLabels*. It then cleans up the column names in activity_labels.txt as well.
  3.  Reads in the files X_test.txt, y_test.txt, and subject_test.txt from the ./UCI HAR Dataset/test directory and saves them to the variables *testSet*, *testLabels*, and *subjectTest* respectively
  4. Reads in the files X_train.txt, y_train.txt, and subject_train.txt from the ./UCI HAR Dataset/train directory and saves them to the variables *trainingSet*, *trainingLabels*, and *subjectTrain* respectively.
  5. Joins *trainingSet* and *testSet*, *trainingLabels* and *testLabels*, and *subjectTrain* and *subjectTest* into the variables *joinedDataSets*, *joinedLabels*, and *joinedSubjects*.
  6. In order to extract only the columns that measure mean or standard deviation, we use grep() to find the index of all of the instances of those words, store it as the variable *featuresWanted*, and then subset *features* to only have the columns we want.
  7. Then the script replaces the activity codes in *joinedLabels* with the descriptive names from *activityLabels*. It does the same with the columns in *joinedDataSets*, replacing the vague headers with the descriptive names found in *features*
  8. The script then cbind()s the three joined sets together to make *allData*. *allData* is a clean, descriptive dataset.
  9. Finally, the script uses the aggregate() function to take the mean of the feature columns. It also sorts the activities and subjects into a pleasing order. However, it produces two extra columns, which we get rid of by setting them to NULL. The write.table() function is then called to produce a text file names tidy.txt
