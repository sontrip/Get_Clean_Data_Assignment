## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## January 25th 2018 - Assignment Working for Getting & Cleaning Data
        
## Intel (R) Core(TM) i3-4160 CPU @ 3.60GHz 
## 8GB RAM 64-bit Processor
## Windows 8.1 C2013
## R Studio 1.0.153 (R version 3.4.1)
        
setwd("C:/Users/Stephane/Documents/Sonia/Learning/coursera/5_Get_Clean_Data")

# (1) Merge the training and the test sets to create one data set.

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"./data/assignment.zip")
unzip("./data/assignment.zip",exdir="./data/assignment")

## Read in the names of each column and the activity labels
features<-read.table("./data/assignment/UCI HAR Dataset/features.txt",colClasses = "character")
activity_labels<-read.table("./data/assignment/UCI HAR Dataset/activity_labels.txt",colClasses = "character")

# Import the test/train set of data and assign column names using the features (2947 observations)
test<-read.table("./data/assignment/UCI HAR Dataset/test/X_test.txt",col.names=features$V2)
train<-read.table("./data/assignment/UCI HAR Dataset/train/X_train.txt",col.names=features$V2)
# Import the person ID
testid<-read.table("./data/assignment/UCI HAR Dataset/test/subject_test.txt",colClasses = "character")
trainid<-read.table("./data/assignment/UCI HAR Dataset/train/subject_train.txt",colClasses = "character")
# Import the activity labels
testlabels<-read.table("./data/assignment/UCI HAR Dataset/test/y_test.txt",colClasses="character")
trainlabels<-read.table("./data/assignment/UCI HAR Dataset/train/y_train.txt",colClasses="character")

library(dplyr)
# Add the personID and activity labels to the dataset and add a label "test or train"

testa<-test %>% 
        mutate(personID=testid$V1) %>%
        mutate(activity_label=testlabels$V1) %>%
        mutate(data="test")

traina<-train %>% 
        mutate(personID=trainid$V1) %>%
        mutate(activity_label=trainlabels$V1) %>%
        mutate(data="train")

all<-rbind(testa,traina)


# (2) Extracts only the measurements on the mean and standard deviation for each measurement.

shortlist<-grep("mean|std",names(all))
meanstd<-select(all,names(all[shortlist]),personID,activity_label,data)

        
# (3) Uses descriptive activity names to name the activities in the data set

meanstd1<-meanstd %>%
        filter(activity_label=="1") %>%
        mutate(activity=activity_labels[1,2])

meanstd2<-meanstd %>%
        filter(activity_label=="2") %>%
        mutate(activity=activity_labels[2,2])

meanstd3<-meanstd %>%
        filter(activity_label=="3") %>%
        mutate(activity=activity_labels[3,2])

meanstd4<-meanstd %>%
        filter(activity_label=="4") %>%
        mutate(activity=activity_labels[4,2])

meanstd5<-meanstd %>%
        filter(activity_label=="5") %>%
        mutate(activity=activity_labels[5,2])

meanstd6<-meanstd %>%
        filter(activity_label=="6") %>%
        mutate(activity=activity_labels[6,2])

meanstdall<-select(rbind(meanstd1,meanstd2,meanstd3,meanstd4,meanstd5,meanstd6),-activity_label)

#(4) Appropriately labels the data set with descriptive variable names.

names(meanstdall) <- gsub("^t", "time",names(meanstdall))
names(meanstdall) <- gsub("^f", "freq", names(meanstdall))
names(meanstdall) <- gsub("\\.","",names(meanstdall))
names(meanstdall) <- tolower(names(meanstdall))
names(meanstdall) <- gsub("bodybody", "body", names(meanstdall))
names(meanstdall) <- gsub("acc", "accelerator", names(meanstdall))
names(meanstdall) <- gsub("mag", "magnitude", names(meanstdall))
names(meanstdall) <- gsub("gyro", "gyroscope", names(meanstdall))
names(meanstdall) <- gsub("jerk", "jerk", names(meanstdall))

meanstdall<-select(meanstdall,personid,activity,1:79,-data)

# (5) From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
avgdata<-summarize_all(group_by(meanstdall,personid,activity),funs(mean))
View(avgdata)


