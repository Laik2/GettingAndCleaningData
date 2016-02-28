run_analysis <- function()
  
  # data is unzipped and lying in the working directory
  # setwd('UCI HAR Dataset/')
  
  # load the following libraries which will be needed
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(data.table)

  # read data
  test_subject<- read.table("test/subject_test.txt")
  testx<- read.table("test/X_test.txt")
  testy<- read.table("test/Y_test.txt")
  
  train_subject<- read.table("train/subject_train.txt")
  trainx<- read.table("train/X_train.txt")
  trainy<- read.table("train/Y_train.txt")
  
  features<- read.table("features.txt")
  activity_labels<- read.table("activity_labels.txt")
  
  # merge x datasets and set column names
  merged<-rbind(trainx,testx)
  colnames(merged)<-make.names(features[,2], unique=T, allow_=T)
  
  # extract only the measurements on the mean (=column containg "mean") and standard deviation (columns containg "std") for each measurement
  # we need to exclude columns with string "Freq", because the weighted average of the frequency components contains the string "mean" as well
  mean_std <- select(merged,  contains("std"), contains("mean") , -contains("Freq"))
  
  # use descriptive activity names to name the activities in the data set 
  # appropriately labels the data set with descriptive variable names
  activity<-rbind(trainy,testy)
  activity$V1 <- factor(activity$V1, levels=activity_labels$V1, labels=activity_labels$V2)
  colnames(activity)=c("activity")
  
  # put all sets together into one dataset
  subject<-rbind(train_subject,test_subject)
  colnames(subject)<-c("subject")
  merged_set<-cbind(mean_std,activity,subject)
  
  # create a second, independent tidy data set with the average of each variable for each activity and each subject.
  # for this, create first a table which contains the calculated mean per subject and activity
  setDT(merged_set)
  average_per_act_sub <- merged_set[, lapply(.SD, mean), by=c ("activity", "subject")]
  tidy_set <- melt(average_per_act_sub, id.vars=c("activity", "subject"),variable.name = "variable_name",value.name = "mean")
  
  #write.csv(tidy_set, tidy_set.csv)
  write.table(tidy_set, "tidyset.txt", row.names=FALSE)
  
  
  
  