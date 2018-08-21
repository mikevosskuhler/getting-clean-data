# ### what this script does:
# the script takes data from a directory that can be set manually
# this particular script is designated to be used on the dataset published on: 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# the script extracts data of both training and test groups and combines the data into one dataset
# next both that rows and the column names are added from the files containing that information
# then a subset of the data containing only means and standard deviations (Std) is taken
# the rownames are adjusted for readability purposes and then the average of each unique combination of 
# subject and activity, finally the script creates an .txt file in the working directory 


require(tidyverse)
require(magrittr)
#Merges the training and the test sets to create one data set.
safedir <- dirname(sys.frame(1)$ofile) #safing file location
directory <- "" #location of the file directory
setwd(directory) #setting wd to file directory

#reading test.txt and train.txt
data.TST <- read.table("test/X_test.txt")    #test
data.TRN <- read.table("train/X_train.txt")  #train

#row-binding data
data.TTL <- rbind(data.TST, data.TRN)

# Extracts only the measurements on the mean and standard deviation for 
# each measurement.

#reading features.txt
features <- read.table("features.txt")

#selecting only Mean and SD
features_selected <- data.TTL %>% subset(select=grepl('-(mean|std)\\(',features$V2))

# Uses descriptive activity names to name the activities in the data set
activity.TST <- read.table("test/y_test.txt")
activity.TRN <- read.table("train/y_train.txt")
activity_labels <- read.table("activity_labels.txt")
activity <- rbind(activity.TST, activity.TRN)

rplc <- function(x, activity_df){#function replacing activity 
  for (i in activity_df[,1]){    #numbers for corresponding string in df
    x = gsub(i,activity_df[i,2], x)
  }
  x
}
activity %<>% mutate(V1 = V1 %>% rplc(activity_df = activity_labels, x = .))
total_df <- cbind(activity, features_selected)

# Appropriately labels the data set with descriptive variable names.
#naming cols
colnames(total_df)[2:length(colnames(total_df))] <- grep('-(mean|std)\\(',
                                                         features$V2, 
                                                         value = T)

#adjusting names
colnames(total_df) <- gsub("^t", "Time", colnames(total_df))
colnames(total_df) <- gsub("^f", "Frequency", colnames(total_df))
colnames(total_df) <- gsub("\\(|\\)", "", colnames(total_df))
colnames(total_df) <- gsub("-", " ", colnames(total_df))


# From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
subject.TST <- read.table("test/subject_test.txt")   #loading test data
subject.TRN <- read.table("train/subject_train.txt") #loading train data
subject <- rbind(subject.TST, subject.TRN)           #binding test and train data
total_df2 <- cbind(subject, total_df) #adding subjects to data
colnames(total_df2)[1] <- "subject"   # naming cols
colnames(total_df2)[2] <- "activities"
list_of_colnames <- colnames(total_df2) #capturing colnames
LCN <- list_of_colnames[3:length(list_of_colnames)] #colnames without sbj & ctvt

# creating final dataset called complete_df
complete_df <- total_df2 %>% aggregate(. ~ subject + activities, data = ., mean)
complete_df %<>% arrange(subject)

# exporting dataset
setwd(safedir) ##setting wd to script dir
write.table(complete_df, "dataset.txt", row.names = F) #writting table