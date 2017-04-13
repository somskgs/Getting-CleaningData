##Name: Someshwar Rao Sattiraju
##Project Assignment: Getting and Cleaning Data

##Description:
## R Script called run_analysis.R that does the following:
## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set GETWD()with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##################### 1,2,3,4 MERGING/BINDING TEST SET AND TRAIN SET & EXTRACTION OF MEAN,STD FEATURES AND DESCRPTIVE NAMES#########################################
##Getting required packages data.table and reshape2 if doesnot exist in system 

## Here we are installing these packages in case system doesnt have them the install.packages() does the same
if(!require("data.table")){
  install.packages("data.table")
}
if(!require("reshape2")){
  install.packages("reshape2")
}

## Getting your working directory and loading reshape2 and data.table packages
wd<-getwd()

### Loading the required librarys
library(data.table)
library(reshape2)


##Reading features.txt & Activity labels from the folder
features<-read.table(paste(wd,"UCI HAR Dataset/features.txt",sep="/"))
activity_labels<-read.table(paste(wd,"UCI HAR Dataset/activity_labels.txt",sep="/"))
features<-as.character(features[,2])
activity_labels<-as.character(activity_labels[,2])

## Readin all data pertaining to test folder and preparing the test set
X_test<-read.table(paste(wd,"UCI HAR Dataset/test/X_test.txt",sep="/"))
X_testlabels<-read.table(paste(wd,"UCI HAR Dataset/test/y_test.txt",sep="/"))
subjecttest<-read.table(paste(wd,"UCI HAR Dataset/test/subject_test.txt",sep="/"))
names(subjecttest)<-"Subject"
X_testlabels[,2]<-activity_labels[X_testlabels[,1]] ## adding second column with activity labels
meanstdlogical<-grepl("mean|std",features) # The grepl searches for text with mean or std and passes True values at those values  
names(X_test)<-features 
X_test<-X_test[,meanstdlogical] # Sub selecting columns as required 
names(X_testlabels)<-c("ActivityId","ActivityLabel") ## renaming the labels
testset<-cbind(subjecttest,X_testlabels,X_test) ## Binding the columns
print("Dimentsions of the Test Set")
print(dim(testset))

## Reading all data pertaining to train folder and preparing the train set
X_train<-read.table(paste(wd,"UCI HAR Dataset/train/X_train.txt",sep="/"))
X_trainlabels<-read.table(paste(wd,"UCI HAR Dataset/train/y_train.txt",sep="/")) ## reading the data as required
subjecttrain<-read.table(paste(wd,"UCI HAR Dataset/train/subject_train.txt",sep="/"))
names(subjecttrain)<-"Subject"
X_trainlabels[,2]<-activity_labels[X_trainlabels[,1]]  ## adding second column with activity labels
meanstdlogic<-grepl("mean|std",features) # The grepl searches for text with mean or std and passes True values at those values
names(X_train)<-features
X_train<-X_train[,meanstdlogic]   # Sub selecting columns as required 
names(X_trainlabels)<-c("ActivityId","ActivityLabel") ## renaming the labels
trainset<-cbind(subjecttrain,X_trainlabels,X_train) ## Binding the columns
print("Dimensions of the Train Set")
print(dim(trainset)) ## Checking the dimensions of the data set

##Combining the two data sets trainset and testset

finalset<-rbind(testset,trainset) ## Binding test set and train set
print("Dimensions of the final set with mean and std features")
print(dim(finalset)) ## Checking the dimensions of the final set


################################# 4. Creating Descrptive Variable Names #######################

data_names<-names(finalset)
data_names<-gsub('[-()]',"",data_names) ## Renaming the feature columns by removing the braces ( better descrption or labelling)
names(finalset)<-data_names

################################## 5. Getting Tidy Data Set with the average of each variable for each activity and each subject#####

### Melting and casting the data as per requirements to get mean of unique Activity, Subject and varibles
finaldata<-melt(finalset,id=c("Subject","ActivityId","ActivityLabel"))
print(head(finaldata,2))
print(dim(finaldata))

tidydata<-dcast(finaldata,Subject+ActivityId+ActivityLabel~variable,mean)
print(dim(tidydata))
print(head(tidydata,2))
write.csv(tidydata,file="tidydataset.csv") ## Writing the tidy dataser to a CSV