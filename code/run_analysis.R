library(data.table)
library(reshape2)

dataDir <- "UCI HAR Dataset"

## getData function is to download the data file and prepare to read it.
getData <- function() {
    # Declearing the provided file names
    zipFileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFile <- "../data/getdata_projectfiles_UCI HAR Dataset.zip"
    
    # if data file does not exists then download and unzip it.
    if(!file.exists(dataDir)) {
        if(!file.exists(zipFile)) {
            download.file(zipFileURl, destfile=zipFile, method="curl", quiet=TRUE)
        }
        unzip(zipFile,)
    }
}

# run the getData function to download the data.
getData()

# Global decelaration of common variables
# load the activity_labels, features data and extract the required features
activity_labels_data <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)[,2]
features_data <- read.table("./UCI HAR Dataset/features.txt",header=FALSE)[,2]
extract_features_data <- grepl("mean|std", features_data)

# getDataSet function to load the data set.
# its used to load the test and train data
getDataSet <- function(X_data_file, y_data_file, subject_data_file) {
    X_data <- read.table(X_data_file)
    y_data <- read.table(y_data_file)
    subject_data <- read.table(subject_data_file)
    
    # assign proper variables names
    names(X_data) = features_data
    
    # get the required data variables
    X_data = X_data[,extract_features_data]
    y_data[,2] = activity_labels_data[y_data[,1]]
    names(y_data) = c("Activity_ID", "Activity_Label")
    names(subject_data) = "subject"
    
    # bind the data 
    data <- cbind(as.data.table(subject_data), y_data, X_data)
}

# load the test data
test_data <- getDataSet("./UCI HAR Dataset/test/X_test.txt", "./UCI HAR Dataset/test/y_test.txt", "./UCI HAR Dataset/test/subject_test.txt")
# load the train data
train_data <- getDataSet("./UCI HAR Dataset/train/X_train.txt", "./UCI HAR Dataset/train/y_train.txt", "./UCI HAR Dataset/train/subject_train.txt")

# merge the test and train data
data = rbind(test_data, train_data)

# assign the proper attributes labels
id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
melt_data      = melt(data, id = id_labels, measure.vars = data_labels)

# compute the average
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

# write the result to output
write.table(tidy_data, file = "../output/tidy_data.txt")
