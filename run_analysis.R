library(plyr)
library(reshape2)

##Create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Step 1 - Merge the training and test data sets. 
input <- list()
input$features <- read.table("UCI HAR Dataset/features.txt", col.names=c('id', 'name'), stringsAsFactors=FALSE)
input$activity_labels <- read.table("UCI HAR Dataset/features.txt", col.names=c('id', 'Activity'))
input$test <- cbind(subject=read.table("UCI HAR Dataset/test/subject_test.txt", col.names="Subject"), y=read.table("UCI HAR Dataset/test/y_test.txt", col.names="Activity.ID"), x=read.table("UCI HAR Dataset/test/x_test.txt"))
input$train <- cbind(subject=read.table("UCI HAR Dataset/train/subject_train.txt", col.names="Subject"), y=read.table("UCI HAR Dataset/train/y_train.txt", col.names="Activity.ID"), x=read.table("UCI HAR Dataset/train/X_train.txt"))
input$merge <- rbind(input$test, input$train)

#Step 2 - Extract the mean and stdev for each measurement
tidy <- input$merge[,c(1, 2, grep("mean\\(|std\\(", input$features$name) + 2)]

#Step 3 - Name activities in data set using descriptive actvity names
activity_names <- input$features$name[grep("mean\\(|std\\(", input$features$name)]
activity_names <- gsub("\\-mean\\(\\)\\-", ".Mean.", activity_names)
activity_names <- gsub("\\-std\\(\\)\\-", ".Std.", activity_names)
activity_names <- gsub("\\-mean\\(\\)", ".Mean", activity_names)
activity_names <- gsub("\\-std\\(\\)", ".Std", activity_names)
activity_names <- gsub("tBody", "Time.Body", activity_names)
activity_names <- gsub("tGravity", "Time.Gravity", activity_names)
activity_names <- gsub("fBody", "FFT.Body", activity_names)
activity_names <- gsub("fGravity", "FFT.Gravity", activity_names)
names(tidy) <- c("Subject", "Activity.ID", activity_names)

#Step 4 - Appropriately label the data set with descriptive variable names
tidy <- merge(tidy, input$activity_labels, by.x="Activity.ID", by.y="id")
tidy <- tidy[,!(names(tidy) %in% c("Activity.ID"))]

#Step 5 - Create an independent data set with average of each variable for each activity and each subject
tidy.means <- ddply(melt(tidy, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))

#Writing the final data set created in Step 5
write.csv(tidy.means, file = "output.txt",row.names = FALSE)