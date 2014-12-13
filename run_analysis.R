#sorry if my english is not good
# You should create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#First download and unzip the data from  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#I use the carpet "UCI HAR Dataset" as my working directory
#Prepare the library "dplyr", "tidyr" and "reshape2"
library("dplyr","tidyr")
#library("reshape2")
data_train <- read.table("train/X_train.txt") # I see in my environment 561 variables. So add..
data_train[,562]<- read.table("train/Y_train.txt") # activity train
data_train[,563]<- read.table("train/subject_train.txt") 

data_test<- read.table("test/X_test.txt") # do the same with Test
data_test[,562]<- read.table("test/Y_test.txt")# activity test
data_test[,563]<- read.table("test/subject_test.txt")


#1 Merges the training and the test sets to create one data set
data_total<-rbind(data_train, data_test) 


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
variables<- read.table("features.txt")#read all variables
indice_variable<- grep("-mean\\(\\)|-std\\(\\)", variables[, 2])#select index of mean and std
data_final <- data_total[, indice_variable]
names(data_final) <- variables[indice_variable, 2]
names(data_final) <- gsub("\\(|\\)", "", names(data_final))#erase ()
names(data_final) <- tolower(names(data_final))#all variables in lower
data_final$activity<-data_total$V1.1 #add the activity variable
data_final$subject<-as.factor(data_total$V1.2) #add the subject variable as a factor

#3. Uses descriptive activity names to name the activities in the data set

activity = read.table("activity_labels.txt") #six activities

# replace 1:6 for activity names
act = 1
for (i in activity$V2) {
	data_final$activity <- gsub(act, i, data_final$activity)
	act <- act + 1
}

data_final$activity <- as.factor(tolower(data_final$activity)) 

#4. Appropriately labels the data set with descriptive variable names. 

# any changes for best variable names
names(data_final) <- gsub("-", "_", names(data_final))
names(data_final) <- gsub("body", "body_", names(data_final))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

by_act_sub<-group_by(data_final, activity, subject)#group the data by activity and subject
tidy<-summarise_each(by_act_sub,funs(mean))# mean for each column
write.table(tidy, file="tidy_means.txt",sep="\t", row.names = FALSE)
