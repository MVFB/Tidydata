
rm(list = ls())

## Step 1: Load packages
library(dplyr)

## Step 2: Load data
traindatapath <- "./train/X_train.txt"
testdatapath <- "./test/X_test.txt"
featurespath <- "./features.txt"
activity_labelspath <- "./activity_labels.txt"
trainactivitiespath <- "./train/y_train.txt"
testactivitiespath <- "./test/y_test.txt"
subject_trainpath <- "./train/subject_train.txt"
subject_testpath <- "./test/subject_test.txt"

traindata <- read.table(traindatapath)
testdata <- read.table(testdatapath)
features <- read.table(featurespath)
activity_labels <- read.table(activity_labelspath)
trainactivities <- read.table(trainactivitiespath)
testactivities <- read.table(testactivitiespath)
subject_train <- read.table(subject_trainpath)
subject_test <- read.table(subject_testpath)


## Step 3: Merges the training and the test sets to create one data set
traindata <- mutate(traindata, V562 = "train", V563 = subject_train$V1)
testdata <- mutate(testdata, V562 = "test", V563 = subject_test$V1)

mdata <- rbind(traindata, testdata)


## Step 4: Extracts only the measurements on the mean and standard deviation for each measurement
variables <- features$V2
variables <- as.vector(variables)
variables <- c(variables, "set", "subject")
names(mdata) <- variables

extractdata <- mdata[, grep("(std|mean|subject)", names(mdata), perl = TRUE)]


## Step 5: Uses descriptive activity names to name the activities in the data set

activitydata <- rbind(trainactivities, testactivities)
extractdata <- mutate(extractdata, activity = activitydata$V1)
descriptivedata <- merge(extractdata, activity_labels, by.x = "activity", by.y = "V1")
names(descriptivedata)[82] <- "activity_label"


## Step 6: Appropriately labels the data set with descriptive variable names
## Already done in Steps 4 and 5


## Step 7: Creates a second, independent tidy data set with the average of each variable for each activity and each subject

by_sub_act <- descriptivedata %>% 
      group_by(subject, activity_label) %>%
      summarise(
            "avg-tBodyAcc-mean()-X" = mean(`tBodyAcc-mean()-X`),
            "avg-tBodyAcc-mean()-Y" = mean(`tBodyAcc-mean()-Y`),
            "avg-tBodyAcc-mean()-Z" = mean(`tBodyAcc-mean()-Z`),
            "avg-tBodyAcc-std()-X" = mean(`tBodyAcc-std()-X`),
            "avg-tBodyAcc-std()-Y" = mean(`tBodyAcc-std()-Y`),
            "avg-tBodyAcc-std()-Z" = mean(`tBodyAcc-std()-Z`),
            "avg-tGravityAcc-mean()-X" = mean(`tGravityAcc-mean()-X`),
            "avg-tGravityAcc-mean()-Y" = mean(`tGravityAcc-mean()-Y`),
            "avg-tGravityAcc-mean()-Z" = mean(`tGravityAcc-mean()-Z`),
            "avg-tGravityAcc-std()-X" = mean(`tGravityAcc-std()-X`),
            "avg-tGravityAcc-std()-Y" = mean(`tGravityAcc-std()-Y`),
            "avg-tGravityAcc-std()-Z" = mean(`tGravityAcc-std()-Z`),
            "avg-tBodyAccJerk-mean()-X" = mean(`tBodyAccJerk-mean()-X`),
            "avg-tBodyAccJerk-mean()-Y" = mean(`tBodyAccJerk-mean()-Y`),
            "avg-tBodyAccJerk-mean()-Z" = mean(`tBodyAccJerk-mean()-Z`),
            "avg-tBodyAccJerk-std()-X" = mean(`tBodyAccJerk-std()-X`),
            "avg-tBodyAccJerk-std()-Y" = mean(`tBodyAccJerk-std()-Y`),
            "avg-tBodyAccJerk-std()-Z" = mean(`tBodyAccJerk-std()-Z`),
            "avg-tBodyGyro-mean()-X" = mean(`tBodyGyro-mean()-X`),
            "avg-tBodyGyro-mean()-Y" = mean(`tBodyGyro-mean()-Y`),
            "avg-tBodyGyro-mean()-Z" = mean(`tBodyGyro-mean()-Z`),
            "avg-tBodyGyro-std()-X" = mean(`tBodyGyro-std()-X`),
            "avg-tBodyGyro-std()-Y" = mean(`tBodyGyro-std()-Y`),
            "avg-tBodyGyro-std()-Z" = mean(`tBodyGyro-std()-Z`),
            "avg-tBodyGyroJerk-mean()-X" = mean(`tBodyGyroJerk-mean()-X`),
            "avg-tBodyGyroJerk-mean()-Y" = mean(`tBodyGyroJerk-mean()-Y`),
            "avg-tBodyGyroJerk-mean()-Z" = mean(`tBodyGyroJerk-mean()-Z`),
            "avg-tBodyGyroJerk-std()-X" = mean(`tBodyGyroJerk-std()-X`),
            "avg-tBodyGyroJerk-std()-Y" = mean(`tBodyGyroJerk-std()-Y`),
            "avg-tBodyGyroJerk-std()-Z" = mean(`tBodyGyroJerk-std()-Z`),
            "avg-tBodyAccMag-mean()" = mean(`tBodyAccMag-mean()`),
            "avg-tBodyAccMag-std()" = mean(`tBodyAccMag-std()`),
            "avg-tGravityAccMag-mean()" = mean(`tGravityAccMag-mean()`),
            "avg-tGravityAccMag-std()" = mean(`tGravityAccMag-std()`),
            "avg-tBodyAccJerkMag-mean()" = mean(`tBodyAccJerkMag-mean()`),
            "avg-tBodyAccJerkMag-std()" = mean(`tBodyAccJerkMag-std()`),
            "avg-tBodyGyroMag-mean()" = mean(`tBodyGyroMag-mean()`),
            "avg-tBodyGyroMag-std()" = mean(`tBodyGyroMag-std()`),
            "avg-tBodyGyroJerkMag-mean()" = mean(`tBodyGyroJerkMag-mean()`),
            "avg-tBodyGyroJerkMag-std()" = mean(`tBodyGyroJerkMag-std()`),
            "avg-fBodyAcc-mean()-X" = mean(`fBodyAcc-mean()-X`),
            "avg-fBodyAcc-mean()-Y" = mean(`fBodyAcc-mean()-Y`),
            "avg-fBodyAcc-mean()-Z" = mean(`fBodyAcc-mean()-Z`),
            "avg-fBodyAcc-std()-X" = mean(`fBodyAcc-std()-X`),
            "avg-fBodyAcc-std()-Y" = mean(`fBodyAcc-std()-Y`),
            "avg-fBodyAcc-std()-Z" = mean(`fBodyAcc-std()-Z`),
            "avg-fBodyAcc-meanFreq()-X" = mean(`fBodyAcc-meanFreq()-X`),
            "avg-fBodyAcc-meanFreq()-Y" = mean(`fBodyAcc-meanFreq()-Y`),
            "avg-fBodyAcc-meanFreq()-Z" = mean(`fBodyAcc-meanFreq()-Z`),
            "avg-fBodyAccJerk-mean()-X" = mean(`fBodyAccJerk-mean()-X`),
            "avg-fBodyAccJerk-mean()-Y" = mean(`fBodyAccJerk-mean()-Y`),
            "avg-fBodyAccJerk-mean()-Z" = mean(`fBodyAccJerk-mean()-Z`),
            "avg-fBodyAccJerk-std()-X" = mean(`fBodyAccJerk-std()-X`),
            "avg-fBodyAccJerk-std()-Y" = mean(`fBodyAccJerk-std()-Y`),
            "avg-fBodyAccJerk-std()-Z" = mean(`fBodyAccJerk-std()-Z`),
            "avg-fBodyAccJerk-meanFreq()-X" = mean(`fBodyAccJerk-meanFreq()-X`),
            "avg-fBodyAccJerk-meanFreq()-Y" = mean(`fBodyAccJerk-meanFreq()-Y`),
            "avg-fBodyAccJerk-meanFreq()-Z" = mean(`fBodyAccJerk-meanFreq()-Z`),
            "avg-fBodyGyro-mean()-X" = mean(`fBodyGyro-mean()-X`),
            "avg-fBodyGyro-mean()-Y" = mean(`fBodyGyro-mean()-Y`),
            "avg-fBodyGyro-mean()-Z" = mean(`fBodyGyro-mean()-Z`),
            "avg-fBodyGyro-std()-X" = mean(`fBodyGyro-std()-X`),
            "avg-fBodyGyro-std()-Y" = mean(`fBodyGyro-std()-Y`),
            "avg-fBodyGyro-std()-Z" = mean(`fBodyGyro-std()-Z`),
            "avg-fBodyGyro-meanFreq()-X" = mean(`fBodyGyro-meanFreq()-X`),
            "avg-fBodyGyro-meanFreq()-Y" = mean(`fBodyGyro-meanFreq()-Y`),
            "avg-fBodyGyro-meanFreq()-Z" = mean(`fBodyGyro-meanFreq()-Z`),
            "avg-fBodyAccMag-mean()" = mean(`fBodyAccMag-mean()`),
            "avg-fBodyAccMag-std()" = mean(`fBodyAccMag-std()`),
            "avg-fBodyAccMag-meanFreq()" = mean(`fBodyAccMag-meanFreq()`),
            "avg-fBodyBodyAccJerkMag-mean()" = mean(`fBodyBodyAccJerkMag-mean()`),
            "avg-fBodyBodyAccJerkMag-std()" = mean(`fBodyBodyAccJerkMag-std()`),
            "avg-fBodyBodyAccJerkMag-meanFreq()" = mean(`fBodyBodyAccJerkMag-meanFreq()`),
            "avg-fBodyBodyGyroMag-mean()" = mean(`fBodyBodyGyroMag-mean()`),
            "avg-fBodyBodyGyroMag-std()" = mean(`fBodyBodyGyroMag-std()`),
            "avg-fBodyBodyGyroMag-meanFreq()" = mean(`fBodyBodyGyroMag-meanFreq()`),
            "avg-fBodyBodyGyroJerkMag-mean()" = mean(`fBodyBodyGyroJerkMag-mean()`),
            "avg-fBodyBodyGyroJerkMag-std()" = mean(`fBodyBodyGyroJerkMag-std()`),
            "avg-fBodyBodyGyroJerkMag-meanFreq()" = mean(`fBodyBodyGyroJerkMag-meanFreq()`)
          )