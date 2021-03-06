# This is the code book for the Getting and Cleaning Data Project of Coursera

##What is explained ? 
This code book explains the variables, data, and  transformation steps and data wrangling done to clean up the data.

# Links To The Data Source pertaining to UCI Machine Learning Repositories

Original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Original description of the dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# About This Data Set 
This data set belongs to the UCI Machine Learning repository.

Explanation as below:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) 
on the waist. Using its embedded accelerometer and gyroscope, team captured 3-axial linear acceleration and 3-axial angular velocity
at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually.
The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for
generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width
sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational 
and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. 
The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used.
From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The dataset includes the following files which are important for reference:

<li> README.txt </li>

<li> features_info.txt: Shows information about the variables used on the feature vector </li>

<li> features.txt: List of all features </li>

<li> activity_labels.txt: Links the class labels with their activity name</li>

<li> train/X_train.txt': Training set</li>

<li> train/y_train.txt': Training labels </li>

<li> test/X_test.txt': Test set </li>

<li> test/y_test.txt': Test labels </li>

# Variables or Data Description

Subject	Numeric
ActivityId	Numeric
ActivityLabel	Text
tBodyAccmeanX	Numeric
tBodyAccmeanY	Numeric
tBodyAccmeanZ	Numeric
tBodyAccstdX	Numeric
tBodyAccstdY	Numeric
tBodyAccstdZ	Numeric
tGravityAccmeanX	Numeric
tGravityAccmeanY	Numeric
tGravityAccmeanZ	Numeric
tGravityAccstdX	Numeric
tGravityAccstdY	Numeric
tGravityAccstdZ	Numeric
tBodyAccJerkmeanX	Numeric
tBodyAccJerkmeanY	Numeric
tBodyAccJerkmeanZ	Numeric
tBodyAccJerkstdX	Numeric
tBodyAccJerkstdY	Numeric
tBodyAccJerkstdZ	Numeric
tBodyGyromeanX	Numeric
tBodyGyromeanY	Numeric
tBodyGyromeanZ	Numeric
tBodyGyrostdX	Numeric
tBodyGyrostdY	Numeric
tBodyGyrostdZ	Numeric
tBodyGyroJerkmeanX	Numeric
tBodyGyroJerkmeanY	Numeric
tBodyGyroJerkmeanZ	Numeric
tBodyGyroJerkstdX	Numeric
tBodyGyroJerkstdY	Numeric
tBodyGyroJerkstdZ	Numeric
tBodyAccMagmean	Numeric
tBodyAccMagstd	Numeric
tGravityAccMagmean	Numeric
tGravityAccMagstd	Numeric
tBodyAccJerkMagmean	Numeric
tBodyAccJerkMagstd	Numeric
tBodyGyroMagmean	Numeric
tBodyGyroMagstd	Numeric
tBodyGyroJerkMagmean	Numeric
tBodyGyroJerkMagstd	Numeric
fBodyAccmeanX	Numeric
fBodyAccmeanY	Numeric
fBodyAccmeanZ	Numeric
fBodyAccstdX	Numeric
fBodyAccstdY	Numeric
fBodyAccstdZ	Numeric
fBodyAccmeanFreqX	Numeric
fBodyAccmeanFreqY	Numeric
fBodyAccmeanFreqZ	Numeric
fBodyAccJerkmeanX	Numeric
fBodyAccJerkmeanY	Numeric
fBodyAccJerkmeanZ	Numeric
fBodyAccJerkstdX	Numeric
fBodyAccJerkstdY	Numeric
fBodyAccJerkstdZ	Numeric
fBodyAccJerkmeanFreqX	Numeric
fBodyAccJerkmeanFreqY	Numeric
fBodyAccJerkmeanFreqZ	Numeric
fBodyGyromeanX	Numeric
fBodyGyromeanY	Numeric
fBodyGyromeanZ	Numeric
fBodyGyrostdX	Numeric
fBodyGyrostdY	Numeric
fBodyGyrostdZ	Numeric
fBodyGyromeanFreqX	Numeric
fBodyGyromeanFreqY	Numeric
fBodyGyromeanFreqZ	Numeric
fBodyAccMagmean	Numeric
fBodyAccMagstd	Numeric
fBodyAccMagmeanFreq	Numeric
fBodyBodyAccJerkMagmean	Numeric
fBodyBodyAccJerkMagstd	Numeric
fBodyBodyAccJerkMagmeanFreq	Numeric
fBodyBodyGyroMagmean	Numeric
fBodyBodyGyroMagstd	Numeric
fBodyBodyGyroMagmeanFreq	Numeric
fBodyBodyGyroJerkMagmean	Numeric
fBodyBodyGyroJerkMagstd	Numeric
fBodyBodyGyroJerkMagmeanFreq	Numeric



# Data Wrangling & Transformation Steps 

Before you begin the the code ensure you have the UCI HAR Dataset folder in the working directory or the folder where you download the R code.

<b> Part 1,2,3 : </b>

Parts 1 to 3 are performed as follows:

<li> Checking Installation of packages:  We check if the packages of reshape2 and data.table exist or not and we install them accordingly </li>
<li> Getting directory: Next we get directory or current working path of the computer </li>
<li> Test Set: The code reads data from the required files performs transformation in the  test  folders along with reading the activity labels text file, subject file and X set and Y label set, post transformations the dataset is binded</li>
<li> Test Set: Before binding the data set the data in X set is filtered by selecting only the variables explaing the STD and Mean columns  </li>
<li> Train Set: The code reads data from the required files performs transformation in the  train folders along with reading the activity labels text file, subject file and X set and Y label set, post transformations the dataset is binded</li>
<li> Test Set: Before binding the data set the data in X set is filtered by selecting only the variables explaing the STD and Mean columns  </li>
<li> Binding the Tes Set and Train Set: Next the Data Sets of test and train are binded to form one finalset using rowbinding (rbind)</li>
<li> Transformation on final set: We cast and dcast the data to get the mean </li>

# Running Or Setting Up the files

1. Load UCI HAR Dataset directory in the working path 
2. Load the R code into the working directory
3. Execute the R file using R Studio or other ide supporting R.
4. Go through the downloaded tidy data 