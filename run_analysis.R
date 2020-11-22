# 0. Download necessary files

if(!file.exists("./data")){dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./data/Dataset.zip")

# unzip file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

#Read data in a table form

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#  Read  testing tables 
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Read  feature vector and activity labels
features <- read.table('./data/UCI HAR Dataset/features.txt')

activity_labels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

#  Assigning column names:

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activity_labels) <- c('activityId','activityType')

#Merging all data in one set:

mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
setAllInOne <- rbind(mrg_train, mrg_test)


#******************************************************************
#Step 2.-Extracts only the measurements on the mean and standard deviation for each measurement.
#******************************************************************

#2.1 Reading column names:

colNames <- colnames(setAllInOne)

#2.2 Defining ID, mean and standard deviation:

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

#2.3 subset from setAllInOne:

setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

#******************************************************************
#Step 3. Uses descriptive activity names to name the activities in the data set
#******************************************************************

setWithActivityNames <- merge(setForMeanAndStd, activity_labels,
                              by='activityId',
                              all.x=TRUE)

#******************************************************************
#Step 4. Appropriately labels the data set with descriptive variable names.
#******************************************************************

names(setAllInOne)<-gsub("Acc", "Accelerometer", names(setAllInOne))
names(setAllInOne)<-gsub("Gyro", "Gyroscope", names(setAllInOne))
names(setAllInOne)<-gsub("BodyBody", "Body", names(setAllInOne))
names(setAllInOne)<-gsub("Mag", "Magnitude", names(setAllInOne))
names(setAllInOne)<-gsub("^t", "Time", names(setAllInOne))
names(setAllInOne)<-gsub("^f", "Frequency", names(setAllInOne))
names(setAllInOne)<-gsub("tBody", "TimeBody", names(setAllInOne))
names(setAllInOne)<-gsub("-mean()", "Mean", names(setAllInOne), ignore.case = TRUE)
names(setAllInOne)<-gsub("-std()", "STD", names(setAllInOne), ignore.case = TRUE)
names(setAllInOne)<-gsub("-freq()", "Frequency", names(setAllInOne), ignore.case = TRUE)
names(setAllInOne)<-gsub("angle", "Angle", names(setAllInOne))
names(setAllInOne)<-gsub("gravity", "Gravity", names(setAllInOne))


#******************************************************************
#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#******************************************************************

#5.1 Making a second tidy data set

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

#5.2 Writing second tidy data set in txt file

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)