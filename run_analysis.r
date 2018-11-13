# run_analysis.r
# Purpose: JHU DS3 "Peer-graded Assignment: Getting and Cleaning Data Course Project"
# input: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# output: TXT file

# load libraries
library(tidyverse)
library(reshape2)   # for 'melt' function

# 1. Merges the training and the test sets to create one data set.

# read data

train_dir <- "~/GitHub/JHUDS/UCI HAR Dataset/train/"
subject_train <- read.csv(paste0(train_dir, "subject_train.txt"), header = FALSE) # 7352 x 1
X_train <- read.fwf(paste0(train_dir, "X_train.txt"), widths = rep(16, 561), header = FALSE) # 7352 x 561 (8976/16) - see "features.txt"
y_train <- read.csv(paste0(train_dir, "y_train.txt"), header = FALSE) # 7352 x 1

test_dir <- "~/GitHub/JHUDS/UCI HAR Dataset/test/"
subject_test <- read.csv(paste0(test_dir, "subject_test.txt"), header = FALSE) # 2947 x 1
X_test <- read.fwf(paste0(test_dir, "X_test.txt"), widths = rep(16, 561), header = FALSE) # 2947 x 561 (8976/16) - see "features.txt"
y_test <- read.csv(paste0(test_dir, "y_test.txt"), header = FALSE) # 2947 x 1

X_names <- read.csv(paste0("~/GitHub/JHUDS/UCI HAR Dataset/", "features.txt"), sep = "", header = FALSE, as.is = TRUE) # 561 x 2

# name variables
names(subject_train) <- "subject"
names(subject_test)  <- "subject"

names(y_train) <- "actcode"
names(y_test)  <- "actcode"

names(X_train) <- X_names$V2
names(X_test)  <- X_names$V2

# merge data (rbind)
subject_data <- rbind(subject_train, subject_test)
X_data <- rbind(X_train, X_test) # different number of columns:  662 and 667
y_data <- rbind(y_train, y_test)

# merge data (cbind)
all_data <- cbind(subject_data, y_data, X_data)

# 2. Extracts only the measurements on the mean and standard deviation for each
# measurement.

# select variables with 'mean(' or 'std(' in name
keep <- grep("mean\\(|std\\(", X_names$V2, value = TRUE)
keep <- c("subject", "actcode", keep)
all_data <- all_data[ , keep] # 10299 x 68

# 3. Uses descriptive activity names to name the activities in the data set

# activity_labels.txt
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

# add column for activity

all_data <- all_data %>%
  mutate(
    activity = case_when(
      actcode == 1 ~ "WALKING",
      actcode == 2 ~ "WALKING_UPSTAIRS",
      actcode == 3 ~ "WALKING_DOWNSTAIRS",
      actcode == 4 ~ "SITTING",
      actcode == 5 ~ "STANDING",
      actcode == 6 ~ "LAYING"
    )
  )

# 4. Appropriately labels the data set with descriptive variable names.

# rename variables (all ready done?

# 5. From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.

# melt: variable and value by activity and subject
# group_by (activity, subject, and variable from melt) and summarize (mean of value from melt)

final_data <- all_data %>%
  dplyr::select(., -actcode) %>%
  melt(., id = c("activity", "subject")) %>%
  dplyr::group_by(activity, subject, variable) %>%
  dplyr::summarize(average = mean(value, na.rm = TRUE))

# save results for grading
write.table(final_data, "~/GitHub/JHUDS/JHU_DS3_Assignment.txt", row.names = FALSE)
