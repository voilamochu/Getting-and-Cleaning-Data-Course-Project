Getting-and-Cleaning-Data-Course-Project
========================================

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.

The objective is to create an R script that
  1.   Merges the training and the test sets to create one data set.
  2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
  3.  Uses descriptive activity names to name the activities in the data set
  4.  Appropriately labels the data set with descriptive variable names. 
  5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The script ("run_analysis.R") achieves the stated objectives. It requires the plyr and reshape2 packages. Please refer to the paths used in the read.table statements to infer the path containing the Samsung data. 

##Running the script:
source("run_analysis.R")