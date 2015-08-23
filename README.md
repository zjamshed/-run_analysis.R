#This runscript, run_analysis.R, performs the following tasks:

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.
library(reshape2)


#Import features and labels
activity_labels<- read.table("./activity_labels.txt")[,2]
features<- read.table("./features.txt")[,2]

#Import train and test
test_x<- read.table("./test/X_test.txt")
names(test_x)<- features
test_y <- read.table("./test/y_test.txt")
test_subject<- read.table("./test/subject_test.txt")

train_x<- read.table("./train/X_train.txt")
names(train_x)<- features
train_y<- read.table("./train/y_train.txt")
train_subject<- read.table("./train/subject_train.txt")

#Find only mean and stdev for each measurement
exfeatures<- grepl("mean|std", features)

#Extract only the measurements on the mean and standard deviation for each measurement.
test_x<- test_x[,exfeatures]
train_x<- train_x[,exfeatures]

# Load activity labels for test and train
test_y[,2]<- activity_labels[test_y[,1]]
names(test_y)<- c("Activity_ID", "Activity_Label")
names(test_subject)<- "subject"

train_y[,2]<- activity_labels[train_y[,1]]
names(train_y)<- c("Activity_ID", "Activity_Label")
names(train_subject)<- "subject"

#Combine datasets
CombineData<- rbind(cbind(test_subject, test_y, test_x),
                    cbind(train_subject, train_y, train_x))
#Rename the columns with descriptive names
Renamed<-colnames(CombineData)
Renamed<-gsub("tBody", "Time.Body", Renamed)
Renamed<- gsub("tGravity", "Time.Gravity", Renamed)
Renamed<- gsub("fBody", "FFT.Body", Renamed)
Renamed<- gsub("fGravity", "FFT.Gravity", Renamed)
Renamed<- gsub("\\-mean\\(\\)\\-", ".Mean.", Renamed)
Renamed<- gsub("\\-std\\(\\)\\-", ".Std.", Renamed)
Renamed<- gsub("\\-mean\\(\\)", ".Mean", Renamed)
Renamed<- gsub("\\-std\\(\\)", ".Std", Renamed)

colnames(CombineData)<-Renamed

#Reshape data
id_labels<- c("subject", "Activity_ID", "Activity_Label")
data_labels<- setdiff(colnames(CombineData), id_labels)
Melted<- melt(CombineData, id<- id_labels, measure.vars<-data_labels)

#Apply mean function to dataset using dcast function
tidy_data<- dcast(Melted, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt",row.name=FALSE)
